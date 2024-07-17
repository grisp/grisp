-module(grisp_max17260).

-behaviour(gen_server).

% API
-export([start_link/2]).
-export([read/1]).
-export([write/2]).

% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

% Includes
-include("../grisp.hrl").
-include("../grisp_i2c.hrl").

%--- Records -------------------------------------------------------------------

-record(state, {bus :: grisp_i2c:bus(),
                reg_files :: #{atom() => reg_file()}
                }).

-record(reg_file, {addr :: non_neg_integer(),
                   size :: non_neg_integer(),
                   type :: read_only | write_only | read_write | special}).

%--- Types ---------------------------------------------------------------------

-type state() :: #state{}.
-type reg_file() :: atom().

%--- MACROS --------------------------------------------------------------------

-define(MAX17260ADDR, 16#36).

-define(MAP_FIELD(Name, Type, Value), Name => field(decode, Name, Type, Value)).
-define(BIN_FIELD(Name, Type, ValueMap), (field(encode, Name, Type, maps:get(Name, ValueMap)))).

-define(RSENSE, 0.01). % 0,01 ohm
% Cf. Table2 MAX17260 datasheet
-define(CAPACITY_MUL, (0.005/?RSENSE)).
-define(CURRENT_MUL, (0.0015625/?RSENSE)).
-define(VOLTAGE_MUL, 0.000078125).
-define(PERCENTAGE_MUL, (1/256)).
-define(TEMP_MUL, (1/256)).
-define(POWER_MUL, (8/?RSENSE)).
-define(TIME_MUL, 5.625).

-define(VE_RESOLUTION, 0.01). % Cf. p.17 => VE resolution is 10mV = 0,01V
-define(VR_RESOLUTION, 0.04). % Cf. p.17 => VR resolution is 40mV = 0,04V

-define(INR_DESIGN_CAP, 2500). % Design Cap is 2500 mAh
-define(INR_TERM_CURR, 500). % Design Charge termination current is 500 mA
-define(INR_EMPTY_VOLTAGE, 2.5). % Design empty voltage is 2,5V
-define(INR_FULL_VOLTAGE, 4.2). % Design charge voltage is 4,2V

%--- API -----------------------------------------------------------------------


start_link(Slot, _Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Slot, []).

-spec read(reg_file()) -> map().
read(RegisterFile) ->
    call({read, RegisterFile}).

-spec write(reg_file(), map()) -> ok.
write(RegisterFile, Value) ->
    call({write, RegisterFile, Value}).

%--- gen_server callbacks ------------------------------------------------------
init(Slot) ->
    Bus = grisp_i2c:open(i2c0),
    State = #state{bus = Bus,
                   reg_files = reg_files()},
    verify_device(State),
    grisp_devices:register(Slot, ?MODULE),
    State1 = initialize_device(State),
    {ok, State1}.

handle_call(Call, _From, State) ->
    try execute_call(Call, State)
    catch throw:Reason -> {reply, {error, Reason}, State}
    end.

handle_cast(Request, _State) ->
  error({unknown_cast, Request}).

handle_info(Info, _State) ->
  error({unknown_info, Info}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%--- Internal ------------------------------------------------------------------
call(Call) ->
    Dev = grisp_devices:default(?MODULE),
    case gen_server:call(Dev#device.pid, Call) of
        {error, Reason} -> error(Reason);
        Result -> Result
    end.

execute_call({read, RegFileName}, State) ->
    {ok, State1, Value} = read_reg_file(State, RegFileName),
    {reply, Value, State1};
execute_call({write, RegisterFile, Value}, State) ->
    RegFileInfo = maps:get(RegisterFile, State#state.reg_files),
    case RegFileInfo of
        #reg_file{type = read_only} ->
            error({read_only_register_file, RegisterFile});
        #reg_file{type = special} ->
            error({special_register_file, RegisterFile});
        _ -> ok
    end,
    {ok, State1} = write_reg_file(State, RegisterFile, Value),
    {reply, ok, State1}.

%--- Internal: Initialization
verify_device(State) ->
    verify_component(State),

    lists:foldl(fun verify_reg_file/2, State, [
        {16#21, <<16#4031:16/big>>, 2}
    ]).

-spec verify_component(state()) -> ok.
verify_component(#state{bus = Bus}) ->
    Detected = grisp_i2c:detect(Bus),
    logger:notice("Detected: ~p", [Detected]),
    case lists:member(?MAX17260ADDR, Detected) of
        true -> ok;
        false -> error({max17260_not_detected_on_i2c, Detected})
    end.

-spec verify_reg_file({RegFile, Value, Size}, State) -> State when
      RegFile  :: non_neg_integer(),
      Value    :: <<_:8>>,
      Size     :: pos_integer(),
      State    :: state().
verify_reg_file({RegFile, <<Value:16/big>>, Size}, State) ->
    case read_request(State, RegFile, Size) of
        {ok, <<Value:16/little>>} -> State;
        {ok, Other} -> error({register_mismatch, RegFile, Other})
    end.

-spec initialize_device(state()) -> state().
initialize_device(State) ->
    {ok, State1, Status} = read_reg_file(State, status),
    ChgVoltage = application:get_env(grisp, chg_voltage, 4.2),
    ModelCfg = if ChgVoltage > 4.25 ->
                      #{refresh => true, v_chg => true};
                  true ->
                      #{refresh => true}
               end,
    case  Status of
        #{por := true} ->
            State2 = wait_dnr(State1, 10),
            {State3, HibCFG} = exit_hibernate(State2),
            % Not overwritting VR in VEmpty is recommended by datasheet (p.28)
            {ok, State4} = write_n_reg_files(State3, [
                              {design_cap, #{design_cap => ?INR_DESIGN_CAP}},
                              {i_chg_term, #{i_chg_term => ?INR_TERM_CURR}},
                              {v_empty,#{ve => ?INR_EMPTY_VOLTAGE}},
                              {model_cfg, ModelCfg}
                         ]),

            State5 = wait_register_value(State4, model_cfg, #{refresh => false}, 50),

            {ok, State6} = write_reg_file(State5, hib_cfg, HibCFG),
            {ok, FinalState} = write_reg_file(State6, status, #{por => false}),
            FinalState;
        #{por := false} ->
            State1
    end.

% @doc wait for the DNR status bit to be set to 0 by the IC
% Step 1 of initialization algorithm given by the document:
% "ModelGauge m5 Host side software implementation guide"
% @throws wait_dnr_failed if after N tries, dnr isn't set to 0
-spec wait_dnr(state(), non_neg_integer()) -> state() | no_return().
wait_dnr(_State, 0) ->
    error({wait_dnr_failed});
wait_dnr(State, N) ->
    {ok, State1, FStat} = read_reg_file(State, fstat),
    case FStat of
        #{dnr := true} ->
            timer:sleep(10),
            wait_dnr(State1, N-1);
        #{dnr := false} ->
            State1
    end.

-spec wait_register_value(state(), atom(), map(), non_neg_integer()) -> state().
wait_register_value(_State, RegisterFile, Value, 0) ->
    error({wait_for_value_failed, RegisterFile, Value});
wait_register_value(State, RegisterFile, Value, N) ->
    {ok, State1, ReadValue} = read_reg_file(State, RegisterFile),
    IsSubSet = lists:all(fun(Key) ->
                                 maps:get(Key, Value) == maps:get(Key, ReadValue)
                         end, maps:keys(Value)),
    if IsSubSet ->
           State1;
       true ->
           timer:sleep(10),
           wait_register_value(State1, RegisterFile, Value, N-1)
    end.

exit_hibernate(State) ->
    {ok, State1, HibCFG} = read_reg_file(State, hib_cfg),
    {ok, NewState} = write_n_reg_files(State1,
                         [{soft_wakeup, #{soft_wakeup => soft_wakeup}},
                          {hib_cfg, #{en_hib => false,
                                      hib_enter_time => 0,
                                      hib_threshold => 0,
                                      hib_exit_time => 0,
                                      hib_scalar => 0}},
                          {soft_wakeup, #{soft_wakeup => clear}}]),
    {NewState, HibCFG}.

-spec reg_files() -> #{atom() := reg_file()}.
reg_files() ->
    #{status => #reg_file{addr = 16#00, size = 2, type = read},
      rep_cap => #reg_file{addr = 16#05, size = 2, type = read},
      rep_soc => #reg_file{addr = 16#06, size = 2, type = read},
      temp => #reg_file{addr = 16#08, size = 2, type = read},
      current => #reg_file{addr = 16#0A, size = 2, type = read},
      v_cell => #reg_file{addr = 16#09, size = 2, type = read},
      full_cap_rep => #reg_file{addr = 16#10, size = 2, type = read},
      tte => #reg_file{addr = 16#11, size = 2, type = read},
      design_cap => #reg_file{addr = 16#18, size = 2, type = read_write}, % User manual not clear for type
      avg_v_cell => #reg_file{addr = 16#19, size = 2, type = read_write},
      max_min_volt => #reg_file{addr = 16#1B, size = 2, type = read_write},
      i_chg_term => #reg_file{addr = 16#1E, size = 2, type = read_write},
      ttf => #reg_file{addr = 16#20, size = 2, type = read},
      dev_name => #reg_file{addr = 16#21, size = 2, type = read},
      v_empty => #reg_file{addr = 16#3A, size = 2, type = read_write},
      fstat => #reg_file{addr = 16#3D, size = 2,type = read},
      soft_wakeup => #reg_file{addr = 16#60, size = 2, type = read_write},
      power => #reg_file{addr = 16#B1, size = 2, type = read},
      avg_power => #reg_file{addr = 16#B2, size = 2, type = read},
      hib_cfg => #reg_file{addr = 16#BA, size = 2, type = read_write},
      r_sense => #reg_file{addr = 16#D0, size = 2, type = read},
      model_cfg => #reg_file{addr = 16#DB, size = 2, type = read_write}}.

%--- Internal: Read and Write functions ----------------------------------------

% @doc reading the register file of the given component
-spec read_reg_file(state(), atom()) -> {ok, state(), map()}.
read_reg_file(State, RegFileName) ->
    #reg_file{addr = RAddr,
              size = RSize} = maps:get(RegFileName, State#state.reg_files),
    {ok, ReadValue} = read_request(State, RAddr, RSize),
    DecodedValue = reg_file(decode, RegFileName, ReadValue),
    {ok, State, DecodedValue}.

% @doc writing the given value(s) to the register file of the given component
% the function makes sure to not overwrite the other values of the register file
% @end
-spec write_reg_file(state(), atom(), map()) -> {ok, state()}.
write_reg_file(State, RegFileName, Value) ->
    #reg_file{addr = RFAddr} = maps:get(RegFileName, State#state.reg_files),

    % Reading the current value of the register file
    {ok, State1, CurrVal} = read_reg_file(State, RegFileName),

    % Preparing the value to write and writting
    NewVal = maps:merge(CurrVal, Value),
    logger:notice("Result merge: ~p~n", [NewVal]),
    ValueToWrite = reg_file(encode, RegFileName, NewVal),
    write_request(State1, RFAddr, ValueToWrite),
    {ok, State1}.

% @doc Performs a series of write
% The write are given as tuples in a list.
% The 1st element of the tuple is the name of the register file
% The 2nd element of the tuple is the map of the values to be written
-spec write_n_reg_files(state(), [{atom(), map()}]) -> {ok, state()}.
write_n_reg_files(State, WriteList) ->
    NewState = lists:foldl(
                 fun({RegFile, Val}, AState) ->
                         {ok, NewAState} = write_reg_file(AState, RegFile, Val),
                         NewAState
                 end, State, WriteList),
    {ok, NewState}.

% @hidden
% Do no use this function to read a register file directly.
% Use @see read_reg_file/2
-spec read_request(State, RegFile, Size) -> {ok, Value} when
      State    :: state(),
      RegFile  :: non_neg_integer(),
      Size     :: pos_integer(),
      Value    :: <<_:8>>.
read_request(#state{bus = Bus}, RegFile, Size) ->
     Resp = grisp_i2c:read(Bus, ?MAX17260ADDR, RegFile, Size),
     debug_read(?MAX17260ADDR, RegFile, Resp),
     {ok, Resp}.

-spec write_request(State, RegFile, Value) -> ok when
      State :: state(),
      RegFile :: non_neg_integer(),
      Value    :: binary().
write_request(#state{bus = Bus}, RegFile, Value) ->
    debug_write(?MAX17260ADDR, RegFile, Value),
    [ok] = grisp_i2c:transfer(Bus, [{write,
                                     ?MAX17260ADDR,
                                     0,
                                     <<RegFile:8, Value/bitstring>>}]),
    ok.

%--- Internal: Register mapping
-spec reg_file(Type, RegFile, Value) -> Ret when
      Type    :: encode | decode,
      RegFile :: atom(),
      Value   :: binary() | map(),
      Ret     :: binary() | map().
reg_file(decode, status, ReadValue) -> % 0x00
    <<DSOCi:1,
      Imx:1,
      _:2,
      Bst:1,
      Imn:1,
      POR:1,
      _:1,
      Br:1,
      Smx:1,
      Tmx:1,
      Vmx:1,
      Bi:1,
      Smn:1,
      Tmn:1,
      Vmn:1>> = ReadValue,
    #{
      ?MAP_FIELD(por, boolean, POR),
      ?MAP_FIELD(imn, boolean, Imn),
      ?MAP_FIELD(imx, boolean, Imx),
      ?MAP_FIELD(vmn, boolean, Vmn),
      ?MAP_FIELD(vmx, boolean, Vmx),
      ?MAP_FIELD(tmn, boolean, Tmn),
      ?MAP_FIELD(tmx, boolean, Tmx),
      ?MAP_FIELD(smn, boolean, Smn),
      ?MAP_FIELD(smx, boolean, Smx),
      ?MAP_FIELD(bst, boolean, Bst),
      ?MAP_FIELD(dscosi, boolean, DSOCi),
      ?MAP_FIELD(bi, boolean, Bi),
      ?MAP_FIELD(br, boolean, Br)
     };
reg_file(encode, status, ValueMap) ->
    <<?BIN_FIELD(dscosi, boolean, ValueMap):1,
      ?BIN_FIELD(imx, boolean, ValueMap):1,
      0:2,
      ?BIN_FIELD(bst, boolean, ValueMap):1,
      ?BIN_FIELD(imn, boolean, ValueMap):1,
      ?BIN_FIELD(por, boolean, ValueMap):1,
      0:1,
      ?BIN_FIELD(br, boolean, ValueMap):1,
      ?BIN_FIELD(smx, boolean, ValueMap):1,
      ?BIN_FIELD(tmx, boolean, ValueMap):1,
      ?BIN_FIELD(vmx, boolean, ValueMap):1,
      ?BIN_FIELD(bi, boolean, ValueMap):1,
      ?BIN_FIELD(smn, boolean, ValueMap):1,
      ?BIN_FIELD(tmn, boolean, ValueMap):1,
      ?BIN_FIELD(vmn, boolean, ValueMap):1>>;
reg_file(decode, rep_cap, ReadValue) -> % 0x05
    <<RepCap:16/little>> = ReadValue,
    #{
      ?MAP_FIELD(rep_cap, capacity, RepCap)
     };
reg_file(decode, rep_soc, ReadValue) -> % 0x06
    <<RepSoc:16/little>> = ReadValue,
    #{
      ?MAP_FIELD(rep_soc, percentage, RepSoc)
     };
reg_file(decode, temp, ReadValue) -> % 0x08
    <<Temp:16/little>> = ReadValue,
    #{
      ?MAP_FIELD(temp, temperature, Temp)
     };
reg_file(decode, v_cell, ReadValue) -> % 0x09
    <<VCell:16/little>> = ReadValue,
    #{
      ?MAP_FIELD(v_cell, voltage, VCell)
     };
reg_file(decode, current, ReadValue) ->
    <<Current:16/little>> = ReadValue,
    #{
      ?MAP_FIELD(current, current, Current)
     };
reg_file(decode, full_cap_rep, ReadValue) -> % 0x10
    <<FullCapRep:16/little>> = ReadValue,
    #{
      ?MAP_FIELD(full_cap_rep, capacity, FullCapRep)
     };
reg_file(decode, tte, ReadValue) -> % 0x11
    <<TTE:16/little>> = ReadValue,
    #{
      ?MAP_FIELD(tte, time, TTE)
     };
reg_file(decode, design_cap, ReadValue) -> % 0x18
    <<DesignCap:16/little>> = ReadValue,
    #{
      ?MAP_FIELD(design_cap, capacity, DesignCap)
     };
reg_file(encode, design_cap, ValueMap) ->
    <<
      ?BIN_FIELD(design_cap, capacity, ValueMap):16/little
      >>;
reg_file(decode, avg_v_cell, ReadValue) -> % 0x19
    <<AvgVCell:16/little>> = ReadValue,
    #{
      ?MAP_FIELD(avg_v_cell, voltage, AvgVCell)
     };
reg_file(decode, i_chg_term, ReadValue) -> % 0x1E
    <<IChgTerm:16/little>> = ReadValue,
    #{
      ?MAP_FIELD(i_chg_term, unsigned, IChgTerm)
     };
reg_file(encode, i_chg_term, ValueMap) ->
    <<
      ?BIN_FIELD(i_chg_term, unsigned, ValueMap):16/little
      >>;
reg_file(decode, ttf, ReadValue) -> % 0x20
    <<TTF:16/little>> = ReadValue,
    #{
      ?MAP_FIELD(ttf, time, TTF)
     };
reg_file(decode, dev_name, ReadValue) -> % 0x21
    <<DevName:16/little>> = ReadValue,
    #{dev_name => DevName};
reg_file(decode, fstat, ReadValue) ->
    <<FQ:1,
      RelDt2:1,
      _:5,
      Dnr:1,
      _:6,
      RelDt:1,
      EDet:1>> = ReadValue,
    #{
      ?MAP_FIELD(rel_dt, boolean, RelDt),
      ?MAP_FIELD(e_det, boolean, EDet),
      ?MAP_FIELD(fq, boolean, FQ),
      ?MAP_FIELD(rel_dt_2, boolean, RelDt2),
      ?MAP_FIELD(dnr, boolean, Dnr)
     };
reg_file(decode, v_empty, ReadValue) -> % 0x3A
    <<VE_LSB:1,
      VR:7,
      VE_MSB:8>> = ReadValue,
    VE = (VE_MSB bsl 1) bor VE_LSB,
    #{
      ?MAP_FIELD(ve, special, VE),
      ?MAP_FIELD(vr, special, VR)
     };
reg_file(encode, v_empty, ValueMap) ->
    <<VE_MSB:8, VE_LSB:1>> = <<?BIN_FIELD(ve, special, ValueMap):9>>,
    <<VE_LSB:1,
      ?BIN_FIELD(vr, special, ValueMap):7,
      VE_MSB:8>>;
reg_file(decode, soft_wakeup, ReadValue) -> % 0x60
    <<SoftWakeup:16/little>> = ReadValue,
    #{
      ?MAP_FIELD(soft_wakeup, pick, SoftWakeup)
     };
reg_file(encode, soft_wakeup, ValueMap) ->
    <<?BIN_FIELD(soft_wakeup, pick, ValueMap):16/little>>;
reg_file(decode, power, ReadValue) -> % 0xB1
    <<Power:16/little>> = ReadValue,
    #{
      ?MAP_FIELD(power, power, Power)
     };
reg_file(decode, avg_power, ReadValue) -> % 0xB2
    <<AvgPower:16/little>> = ReadValue,
    #{
      ?MAP_FIELD(avg_power, power, AvgPower)
     };
reg_file(decode, hib_cfg, ReadValue) -> % 0xBA
    <<_:3,
      HibExitTime:2,
      HibScalar:3,
      EnHib:1,
      HibEnterTime:3,
      HibThreshold:4>> = ReadValue,
    #{
      ?MAP_FIELD(en_hib, boolean, EnHib),
      ?MAP_FIELD(hib_enter_time, unsigned, HibEnterTime),
      ?MAP_FIELD(hib_threshold, unsigned, HibThreshold),
      ?MAP_FIELD(hib_exit_time, unsigned, HibExitTime),
      ?MAP_FIELD(hib_scalar, unsigned, HibScalar)
     };
reg_file(encode, hib_cfg, ValueMap) ->
    <<0:3,
      ?BIN_FIELD(hib_exit_time, unsigned, ValueMap):2,
      ?BIN_FIELD(hib_scalar, unsigned, ValueMap):3,
      ?BIN_FIELD(en_hib, boolean, ValueMap):1,
      ?BIN_FIELD(hib_enter_time, unsigned, ValueMap):3,
      ?BIN_FIELD(hib_threshold, unsigned, ValueMap):4>>;
reg_file(decode, r_sense, ReadValue) -> % 0xD0
    <<RSense:16/little>> = ReadValue,
    #{
      ?MAP_FIELD(r_sense, unsigned, RSense) % FIXME: type of Rsense isn't clear
     };
reg_file(decode, model_cfg, ReadValue) -> % 0xDB
    <<ModelId:4,
      _:1,
      CSEL:1,
      _:2,
      Refresh:1,
      _:1,
      R100:1,
      _:2,
      VChg:1,
      _:2>> = ReadValue,
    #{
      ?MAP_FIELD(refresh, boolean, Refresh),
      ?MAP_FIELD(r100, boolean, R100),
      ?MAP_FIELD(v_chg, boolean, VChg),
      ?MAP_FIELD(model_id, pick, ModelId),
      ?MAP_FIELD(csel, boolean, CSEL)
     };
reg_file(encode, model_cfg, ValueMap) ->
    <<?BIN_FIELD(model_id, pick, ValueMap):4,
      0:1,
      ?BIN_FIELD(csel, boolean, ValueMap):1,
      0:2,
      ?BIN_FIELD(refresh, boolean, ValueMap):1,
      0:1,
      ?BIN_FIELD(r100, boolean, ValueMap):1,
      0:2,
      ?BIN_FIELD(v_chg, boolean, ValueMap):1,
      0:2>>;
reg_file(_, RegFile, _) ->
    error({unknown_register_file, RegFile}).


-spec field(Action, RegFile, Type, Value) -> Ret when
    Action  :: encode | decode,
    RegFile :: atom(),
    Type    :: pick | {signed, Size} | unsigned | boolean,
    Size    :: 16 | 32,
    Value   :: map() | binary(),
    Ret     :: map() | binary().
field(decode, _, {signed, Size}, Value) ->
    MSB = Value bsr (Size - 1),
    print("MSB: ~p~n", [MSB]),
    case MSB of
        0 ->
            Value;
        1 ->
            Mask = round(math:pow(2, Size) - 1),
            print("Value: ~p | Val - 1: ~p | bnot(Val - 1): ~p~n", [Value, (Value - 1) band Mask, bnot((Value - 1) band Mask) band Mask]),
            -(bnot((Value - 1) band Mask) band Mask)
    end;
field(encode, _, unsigned, Value) ->
    Value;
field(decode, _, unsigned, Value) ->
    Value;
field(decode,  _, boolean, Value) ->
    case Value of
        0 -> false;
        1 -> true
    end;
field(encode, _, boolean, Value) ->
    case Value of
        false -> 0;
        true -> 1
    end;
field(decode, _, bit, Value) ->
    Value;
field(decode, _, capacity, Value) ->
    Value * ?CAPACITY_MUL; % TODO check if Rsense is 0,01 ohm + explain how I found this value
field(encode, _, capacity, Value) ->
    C = round(math:floor(Value / ?CAPACITY_MUL)),
    logger:notice("Given capacity: ~p - Encoded: ~p", [Value, C]),
    C;
field(decode, _, current, Value) ->
    twos_complement(decode, Value) * ?CURRENT_MUL;
field(encode, _, current, Value) ->
    ValueToEncode = round(math:floor(Value / ?CURRENT_MUL)),
    twos_complement(encode, ValueToEncode);
field(decode, _, voltage, Value) ->
    Value * ?VOLTAGE_MUL;
field(encode, _, voltage, Value) ->
    round(math:floor(Value / ?VOLTAGE_MUL));
field(decode, _, percentage, Value) ->
    Value * ?PERCENTAGE_MUL;
field(encode, _, percentage, Value) ->
    round(math:floor(Value / ?PERCENTAGE_MUL));
field(decode, _, temperature, Value) ->
    twos_complement(decode, Value) * ?PERCENTAGE_MUL;
field(encode, _, temperature, Value) ->
    ValueToEncode = round(math:floor(Value / ?TEMP_MUL)),
    twos_complement(encode, ValueToEncode);
field(decode, _, power, Value) ->
    (Value * ?POWER_MUL) / 1000;
field(decode, _, time, Value) ->
    Value * ?TIME_MUL;
field(encode, _, time, Value) ->
    round(math:floor(Value / ?TIME_MUL));
field(decode, ve, special, VE) ->
    VE * ?VE_RESOLUTION;
field(encode, ve, special, VE) ->
    round(math:floor(VE / ?VE_RESOLUTION));
field(decode, vr, special, VR) ->
    VR * ?VR_RESOLUTION;
field(encode, vr, special, VR) ->
    round(math:floor(VR / ?VR_RESOLUTION));
field(decode, soft_wakeup, pick, Value) ->
    % Note: It appears that there is an error in the datasheet
    % The value soft wakeup value is given as 0x0090 but should be written 0x9000
    case Value of
        16#0000 -> clear;
        16#9000 -> soft_wakeup;
        16#0090 -> error({soft_wakeup_encoded_in_big_endian})
    end;
field(encode, soft_wakeup, pick, Value) ->
    case Value of
        clear -> 16#0000;
        soft_wakeup -> 16#9000
    end;
field(decode, model_id, pick, Value) ->
    case Value of
        0 -> li_co_oxide_variants;
        2 -> ncr_or_nca;
        6 -> li_fe_po4
    end;
field(encode, model_id, pick, Value) ->
    case Value of
        li_co_oxide_variants -> 0;
        ncr_or_nca -> 2;
        li_fe_po4 -> 6
    end;
field(_, Register, Type, _) ->
    error({unknown_register, Register, Type}).

% TODO test correctly this function
twos_complement(encode, Value) ->
    Mask = round(math:pow(2, 17)) - 1,
    if Value < 0 ->
           ((bnot(Value) band Mask) + 1) band Mask;
       true -> Value
    end;
twos_complement(decode, Value) ->
    Mask = round(math:pow(2, 17)) - 1,
    MSB = Value bsr 15,
    print("MSB ~p~n", [MSB]),
    case MSB of
        1 ->
            print("Value: ~p~n", [- (bnot((Value - 1) band Mask) band Mask)]),
            - (bnot((Value - 1) band Mask) band Mask);
        _ -> Value
    end.

%--- Internal: Debug
debug_read(Comp, Reg, Value) ->
    print("[Charger][~p] read 16#~2.16.0B <-- ~s~n",
        [Comp, Reg, debug_bitstring(Value)]
    ).

debug_write(Comp, Reg, Value) ->
    print("[Charger][~p] write 16#~2.16.0B <-- ~s~n",
        [Comp, Reg, debug_bitstring(Value)]
    ).

debug_bitstring(Bitstring) ->
    lists:flatten([io_lib:format("2#~8.2.0B ", [X]) || <<X>> <= Bitstring]).

print(String, Format) ->
    case global:whereis_name(global_io_srv) of
        undefined -> io:format(String, Format);
        Name -> io:format(Name, String, Format)
    end.
