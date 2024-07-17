-module(grisp_pac1933).

-behaviour(gen_server).

% API
-export([start_link/2]).
-export([voltage/0]).
-export([voltage/1]).
-export([power/0]).
-export([read/1]).

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

%--- MACROS --------------------------------------------------------------------

-define(PAC1933ADDR, 16#13).

-define(SOURCES, [wall, usb, battery]).

-define(PFSR, 640). % c.f. Eq. 4-5

-define(CH1Bid, true).
-define(CH1TYPE, signed).

-define(CH2Bid, false).
-define(CH2TYPE, unsigned).

-define(CH3Bid, true).
-define(CH3TYPE, signed).

-define(MAP_FIELD(Name, Type, Value), Name => field(decode, Name, Type, Value)).
-define(BIN_FIELD(Name, Type, Value), (field(encode, Name, Type, Value))).

-define(SINGLE_16BITS_REGS, #{vbus1 => true,
                              vbus2 => true,
                              vbus3 => true,
                              vsense1 => true,
                              vsense2 => true,
                              vsense3 => true,
                              vbus1_avg => true,
                              vbus2_avg => true,
                              vbus3_avg => true,
                              vsense1_avg => true,
                              vsense2_avg => true,
                              vsense3_avg => true}).
%--- Record --------------------------------------------------------------------

-record(state, {bus :: grisp_i2c:bus(),
                reg_files :: #{atom() => reg_file()}
                }).

-record(reg_file, {addr :: non_neg_integer(),
                   size :: non_neg_integer(),
                   type :: read_only | write_only | read_write | special}).

%--- Types ---------------------------------------------------------------------

-type state() :: #state{}.
-type reg_file() :: #reg_file{}.

-type source() :: wall | usb | battery.

%--- API -----------------------------------------------------------------------
start_link(Slot, _Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Slot, []).

-spec read(reg_file()) -> map().
read(RegisterFile) ->
    call({read, RegisterFile}).

-spec voltage() -> #{wall := float(), usb := float(), battery := float()}.
voltage() ->
    apply_on_sources(fun voltage/1).

-spec voltage(source()) -> integer().
voltage(Source) ->
    call({voltage, Source}).

power() ->
    apply_on_sources(fun power/1).

power(Source) ->
    call({power, Source}).

%--- gen_server callbacks ------------------------------------------------------

init(Slot) ->
    Bus = grisp_i2c:open(i2c0),
    State =  #state{bus = Bus,
                    reg_files = reg_files()
                   },
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
%--- Internal: Call handling
call(Call) ->
    Dev = grisp_devices:default(?MODULE),
    case gen_server:call(Dev#device.pid, Call) of
        {error, Reason} -> error(Reason);
        Result -> Result
    end.

execute_call({voltage, Source}, State) ->
    V = read_voltage(State, Source),
    {reply, V, State};
execute_call({power, Source}, State) ->
    P = read_power(State, Source),
    {reply, P, State};
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
-spec initialize_device(state()) -> state().
initialize_device(State) ->
    NegPwr = #{ch1_bidi => ?CH1Bid, ch1_bidv => ?CH1Bid,
               ch2_bidi => ?CH2Bid, ch2_bidv => ?CH2Bid,
               ch3_bidi => ?CH3Bid, ch3_bidv => ?CH3Bid},
    Configs = [{neg_pwr, NegPwr}],
    lists:foldl(fun({RegFile, Config}, AccState) ->
                        {ok, NewState} = write_reg_file(AccState, RegFile, Config),
                        NewState
                end, State, Configs).

% @doc builds a map of the registers of a component and with their addresses
-spec reg_files() -> RegisterFiles when
      RegisterFiles :: #{atom() => reg_file()}.
reg_files() ->
    % Registers 0x06, 0x0A, 0x0E, 0x12, 0x16, 0x1A
    % Are used by other chips of the PAC193x familly.
    #{refresh => #reg_file{addr = 16#00, size = 0, type = special},
      ctrl => #reg_file{addr = 16#01, size = 1, type = read_write},
      acc_count => #reg_file{addr = 16#02, size = 3, type = read},
      vpower1_acc => #reg_file{addr = 16#03, size = 6, type  = read},
      vpower2_acc => #reg_file{addr = 16#04, size = 6, type  = read},
      vpower3_acc => #reg_file{addr = 16#05, size = 6, type  = read},
      vbus1 => #reg_file{addr = 16#07, size = 2, type  = read},
      vbus2 => #reg_file{addr = 16#08, size = 2, type  = read},
      vbus3 => #reg_file{addr = 16#09, size = 2, type  = read},
      vbus4 => #reg_file{addr = 16#0A, size = 2, type  = read}, % FIXME: To remove later
      vsense1 => #reg_file{addr = 16#0B, size = 2, type  = read},
      vsense2 => #reg_file{addr = 16#0C, size = 2, type  = read},
      vsense3 => #reg_file{addr = 16#0D, size = 2, type  = read},
      vbus1_avg => #reg_file{addr = 16#0F, size = 2, type  = read},
      vbus2_avg => #reg_file{addr = 16#10, size = 2, type  = read},
      vbus3_avg => #reg_file{addr = 16#11, size = 2, type  = read},
      vsense1_avg => #reg_file{addr = 16#13, size = 2, type  = read},
      vsense2_avg => #reg_file{addr = 16#14, size = 2, type  = read},
      vsense3_avg => #reg_file{addr = 16#15, size = 2, type  = read},
      vpower1 => #reg_file{addr = 16#17, size = 4, type  = read},
      vpower2 => #reg_file{addr = 16#18, size = 4, type  = read},
      vpower3 => #reg_file{addr = 16#19, size = 4, type  = read},
      % channel_dis Also contains SMBUS ctrl
      channel_dis => #reg_file{addr = 16#1C, size = 1, type = read_write},
      neg_pwr => #reg_file{addr = 16#1D, size = 1, type = read_write},
      refresh_g => #reg_file{addr = 16#1E, size = 0, type = special},
      refresh_v => #reg_file{addr = 16#1F, size = 0, type = special},
      slow => #reg_file{addr = 16#20, size = 1, type = read_write},
      ctrl_act => #reg_file{addr = 16#21, size = 1, type = read},
      channel_dis_act => #reg_file{addr = 16#22, size = 1, type = read},
      neg_pwr_act => #reg_file{addr = 16#23, size = 1, type = read},
      ctrl_lat => #reg_file{addr = 16#24, size = 1, type = read},
      channel_dis_lat => #reg_file{addr = 16#25, size = 1, type = read},
      neg_pwr_lat => #reg_file{addr = 16#26, size = 1, type = read},
      prod_id => #reg_file{addr = 16#FD, size = 1, type = read},
      man_id => #reg_file{addr = 16#FE, size = 1, type = read},
      rev_id => #reg_file{addr = 16#FF, size = 1, type = read}
     }.

apply_on_sources(Fun) ->
    lists:foldl(fun(Source, Map) ->
                        maps:put(Source, Fun(Source), Map)
                end, #{}, ?SOURCES).

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

%--- Internal: Verifiers -------------------------------------------------------
-spec verify_device(state()) -> ok.
verify_device(State) ->
    verify_component(State),

    lists:foldl(fun verify_reg_file/2, State, [
        {16#FD, <<2#01011010>>, 1},
        {16#FE, <<16#5D>>, 1},
        {16#FF, <<16#03>>, 1}
    ]).

-spec verify_component(state()) -> ok.
verify_component(#state{bus = Bus}) ->
    Detected = grisp_i2c:detect(Bus),
    logger:notice("Detected: ~p", [Detected]),
    case lists:member(?PAC1933ADDR, Detected) of
        true -> ok;
        false -> error({pac1933_not_detected_on_i2c, Detected})
    end.

-spec verify_reg_file({RegFile, Value, Size}, State) -> State when
      RegFile  :: non_neg_integer(),
      Value    :: <<_:8>>,
      Size     :: pos_integer(),
      State    :: state().
verify_reg_file({RegFile, Value, Size}, State) ->
    case read_request(State, RegFile, Size) of
        {ok, Value} -> State;
        {ok, Other} -> error({register_mismatch, RegFile, Other})
    end.

%--- Internals: Measurements functions -----------------------------------------
% @doc Gets the measured <b>voltage<b> at SENSE+ for the given output
% It converts the values of VBus using Eq. 4-1 of the data sheet
% The units of the returned value is in Volt (V)
-spec read_voltage(state(), source()) -> integer().
read_voltage(State, Source) ->
    Den = case Source of
              wall -> 16#10000; % 2^16
              _ -> 16#8000 % 2^15
          end,
    [VBus] = maps:values(read_vbus(State, Source)),
    32 * (VBus/Den).

-spec read_vbus(state(), source()) -> float().
read_vbus(State, Source) ->
    % FIXME: find a better way to fetch infos related to the sources
    refreshv(State),
    timer:sleep(2),
    RegFileName = case Source of
                   usb -> vbus1;
                   wall -> vbus2;
                   battery -> vbus3
               end,
    {ok, _, VBus} = read_reg_file(State, RegFileName),
    VBus.

% @doc Gets the measured <b> power for the given output
% It converts the value of VPower using equation 4-7
% The unit of the value returned is in Watts.
-spec read_power(state(), source()) -> float().
read_power(State, Source) ->
    [VPower] = maps:values(read_vpower(State, Source)),
    Den = case Source of
              wall -> 16#10000000;
              _ -> 16#8000000
          end,
    ?PFSR * (VPower/Den).

read_vpower(State, Source) ->
    refreshv(State),
    timer:sleep(2),
    RegFileName = case Source of
                      usb -> vpower1;
                      wall -> vpower2;
                      battery -> vpower3
                  end,
    {ok, _, VPower} = read_reg_file(State, RegFileName),
    logger:notice("VPower: ~p", [VPower]),
    VPower.

%--- Internals: Utils ----------------------------------------------------------
% @doc Sends the refresh command for Vbus, Vsense data only
refreshv(#state{bus = Bus}) ->
    [ok] = grisp_i2c:transfer(Bus, [{write, ?PAC1933ADDR, 0, <<16#1F>>}]),
    timer:sleep(1).

-spec read_request(State, RegFile, Size) -> {ok, Value} when
      State    :: state(),
      RegFile  :: non_neg_integer(),
      Size     :: pos_integer(),
      Value    :: <<_:8>>.
read_request(#state{bus = Bus}, RegFile, Size) ->
    [ok] = grisp_i2c:transfer(Bus, [{write, ?PAC1933ADDR, 0, <<RegFile>>}]),
    timer:sleep(1),
    [Resp] = grisp_i2c:transfer(Bus, [{read, ?PAC1933ADDR, 1, Size}]),
    debug_read(?PAC1933ADDR, RegFile, Resp),
    {ok, Resp}.

-spec write_request(State, RegFile, Value) -> ok when
      State :: state(),
      RegFile :: non_neg_integer(),
      Value    :: binary().
write_request(#state{bus = Bus}, RegFile, Value) ->
    debug_write(?PAC1933ADDR, RegFile, Value),
    [ok] = grisp_i2c:transfer(Bus, [{write, ?PAC1933ADDR, 0, <<RegFile:8, Value/bitstring>>}]),
    ok.

%--- Internals: Encode/Decode --------------------------------------------------
-spec reg_file(Type, RegFile, Value) -> Ret when
      Type    :: encode | decode,
      RegFile :: atom(),
      Value   :: binary() | map(),
      Ret     :: binary() | map().
reg_file(decode, RegFile, ReadValue) when RegFile == ctrl orelse
                                          RegFile == ctrl_act orelse
                                          RegFile == ctrl_lat ->
    <<SampleRate:2,
      Sleep:1,
      Sing:1,
      AlertPin:1,
      AlertCC:1,
      OvfAlert:1,
      Ovf:1>> = ReadValue,
    #{
        ?MAP_FIELD(sample_rate, pick, SampleRate),
        ?MAP_FIELD(sleep, boolean, Sleep),
        ?MAP_FIELD(sing, pick, Sing),
        ?MAP_FIELD(alert_pin, boolean, AlertPin),
        ?MAP_FIELD(alert_cc, boolean, AlertCC),
        ?MAP_FIELD(ovf_alert, boolean, OvfAlert),
        ?MAP_FIELD(ovf, boolean, Ovf)
     };
reg_file(encode, ctrl, Value) ->
    #{sample_rate := SampleRate,
      sleep := Sleep,
      sing := Sing,
      alert_pin := AlertPin,
      alert_cc := AlertCC,
      ovf_alert := OvfAlert,
      ovf := Ovf
     } = Value,
    <<?BIN_FIELD(sample_rate, pick, SampleRate):1,
      ?BIN_FIELD(sleep, boolean, Sleep):1,
      ?BIN_FIELD(sing, pick, Sing):1,
      ?BIN_FIELD(alert_pin, boolean, AlertPin):1,
      ?BIN_FIELD(alert_cc, boolean, AlertCC):1,
      ?BIN_FIELD(ovf_alert, boolean, OvfAlert):1,
      ?BIN_FIELD(ovf, boolean, Ovf):1>>;
reg_file(decode, acc_count, ReadValue) ->
    <<AccCount:24>> = ReadValue,
    #{
      ?MAP_FIELD(acc_count, {unsigned, 24}, AccCount)
     };
reg_file(decode, vpower1_acc, ReadValue) ->
    <<VPower1Acc:48>> = ReadValue,
    #{
      ?MAP_FIELD(vpower1_acc, {?CH1TYPE, 48}, VPower1Acc)
     };
reg_file(decode, vpower2_acc, ReadValue) ->
    <<VPower2Acc:48>> = ReadValue,
    #{
      ?MAP_FIELD(vpower2_acc, {?CH2TYPE, 48}, VPower2Acc)
     };
reg_file(decode, vpower3_acc, ReadValue) ->
    <<VPower3Acc:48>> = ReadValue,
    #{
      ?MAP_FIELD(vpower3_acc, {?CH3TYPE, 48}, VPower3Acc)
     };
reg_file(decode, RegFile, ReadValue)
  when is_map_key(RegFile, ?SINGLE_16BITS_REGS) ->
    % Maps: VBusN, VSenseN, VBusN_avg, VSenseN_Avg
    <<Value:16>> = ReadValue,
    Prefix = lists:nth(1, string:split(atom_to_list(RegFile), "_")),
    Type = case lists:last(Prefix) of
               $1 -> ?CH1TYPE;
               $2 -> ?CH2TYPE;
               $3 -> ?CH3TYPE
           end,
    #{
      ?MAP_FIELD(RegFile, {Type, 16}, Value)
     };
reg_file(decode, vpower1, ReadValue) ->
    <<VPower1:28,
      _:4>> = ReadValue,
    #{
      ?MAP_FIELD(vpower1, {?CH1TYPE, 28}, VPower1)
     };
reg_file(decode, vpower2, ReadValue) ->
    <<VPower2:28,
      _:4>> = ReadValue,
    #{
      ?MAP_FIELD(vpower2, {?CH2TYPE, 28}, VPower2)
     };
reg_file(decode, vpower3, ReadValue) ->
    <<VPower3:28,
      _:4>> = ReadValue,
    #{
      ?MAP_FIELD(vpower3, {?CH3TYPE, 28}, VPower3)
     };
reg_file(decode, channel_dis, ReadValue) ->
    <<Ch1Off:1,
      Ch2Off:1,
      Ch3Off:1,
      Ch4Off:1,
      Timeout:1,
      ByteCount:1,
      NoSkip:1,
      _:1>> = ReadValue,
    #{
      ?MAP_FIELD(ch1_off, boolean, Ch1Off),
      ?MAP_FIELD(ch2_off, boolean, Ch2Off),
      ?MAP_FIELD(ch3_off, boolean, Ch3Off),
      ?MAP_FIELD(ch4_off, boolean, Ch4Off),
      ?MAP_FIELD(timeout, boolean, Timeout),
      ?MAP_FIELD(byte_count, boolean, ByteCount),
      ?MAP_FIELD(no_skip, boolean, NoSkip)
     };
reg_file(encode, channel_dis, Value) ->
    #{ch1_off := Ch1Off,
      ch2_off := Ch2Off,
      ch3_off := Ch3Off,
      ch4_off := Ch4Off,
      timeout := Timeout,
      byte_count := ByteCount,
      no_skip := NoSkip
     } = Value,
    <<?BIN_FIELD(ch1_off, boolean, Ch1Off),
      ?BIN_FIELD(ch2_off, boolean, Ch2Off),
      ?BIN_FIELD(ch3_off, boolean, Ch3Off),
      ?BIN_FIELD(ch4_off, boolean, Ch4Off),
      ?BIN_FIELD(timeout, boolean, Timeout),
      ?BIN_FIELD(byte_count, boolean, ByteCount),
      ?BIN_FIELD(no_skip, boolean, NoSkip),
      0:1>>;
reg_file(decode, RegFile, ReadValue) when RegFile == neg_pwr orelse
                                                   RegFile == neg_pwr_act orelse
                                                   RegFile == neg_pwr_lat ->
    <<Ch1BidI:1,
      Ch2BidI:1,
      Ch3BidI:1,
      _:1,
      Ch1BidV:1,
      Ch2BidV:1,
      Ch3BidV:1,
      _:1>> = ReadValue,
    #{
      ?MAP_FIELD(ch1_bidi, boolean, Ch1BidI),
      ?MAP_FIELD(ch2_bidi, boolean, Ch2BidI),
      ?MAP_FIELD(ch3_bidi, boolean, Ch3BidI),
      ?MAP_FIELD(ch1_bidv, boolean, Ch1BidV),
      ?MAP_FIELD(ch2_bidv, boolean, Ch2BidV),
      ?MAP_FIELD(ch3_bidv, boolean, Ch3BidV)
     };
reg_file(encode, neg_pwr, Value) ->
    #{ch1_bidi := Ch1BidI,
      ch2_bidi := Ch2BidI,
      ch3_bidi := Ch3BidI,
      ch1_bidv := Ch1BidV,
      ch2_bidv := Ch2BidV,
      ch3_bidv := Ch3BidV} = Value,
    <<?BIN_FIELD(ch1_bidi, boolean, Ch1BidI):1,
      ?BIN_FIELD(ch2_bidi, boolean, Ch2BidI):1,
      ?BIN_FIELD(ch3_bidi, boolean, Ch3BidI):1,
      0:1,
      ?BIN_FIELD(ch1_bidv, boolean, Ch1BidV):1,
      ?BIN_FIELD(ch2_bidv, boolean, Ch2BidV):1,
      ?BIN_FIELD(ch3_bidv, boolean, Ch3BidV):1,
      0:1>>;
reg_file(decode, slow, ReadValue) ->
    <<Slow:1,
      SlowLH:1,
      SlowHL:1,
      RRise:1,
      RVRise:1,
      RFall:1,
      RVFall:1,
      POR:1>> = ReadValue,
    #{
      ?MAP_FIELD(slow, bit, Slow),
      ?MAP_FIELD(slow_lh, bit, SlowLH),
      ?MAP_FIELD(slow_hl, bit, SlowHL),
      ?MAP_FIELD(r_rise, bit, RRise),
      ?MAP_FIELD(r_v_rise, bit, RVRise),
      ?MAP_FIELD(r_fall, bit, RFall),
      ?MAP_FIELD(r_v_fall, bit, RVFall),
      ?MAP_FIELD(por, bit, POR)
     };
reg_file(encode, slow, Value) ->
    #{slow := Slow,
      slow_lh := SlowLH,
      slow_hl := SlowHL,
      r_rise := RRise,
      r_v_rise := RVRise,
      r_fall := RFall,
      r_v_fall := RVFall,
      por := POR
     } = Value,
    <<Slow:1,
      SlowLH:1,
      SlowHL:1,
      RRise:1,
      RVRise:1,
      RFall:1,
      RVFall:1,
      POR:1>>;
reg_file(decode, RegFile, ReadValue)
  when RegFile == channel_dis_act orelse RegFile == channel_dis_lat ->
    <<Ch1Off:1,
      Ch2Off:1,
      Ch3Off:1,
      Ch4Off:1,
      _:4>> = ReadValue,
    #{
      ?MAP_FIELD(ch1_off, boolean, Ch1Off),
      ?MAP_FIELD(ch2_off, boolean, Ch2Off),
      ?MAP_FIELD(ch3_off, boolean, Ch3Off),
      ?MAP_FIELD(ch4_off, boolean, Ch4Off)
     };
reg_file(decode, RegFile, ReadValue)
  when RegFile == prod_id orelse RegFile == man_id orelse RegFile == rev_id ->
    #{RegFile => ReadValue};
reg_file(_, RegFile, _) ->
    error({unknown_register_file, RegFile}).

-spec field(Action, RegFile, Type, Value) -> Ret when
    Action  :: encode | decode,
    RegFile :: atom(),
    Type    :: pick | {signed, Size} | unsigned | boolean,
    Size    :: 16 | 32,
    Value   :: map() | binary(),
    Ret     :: map() | binary().
field(decode, sample_rate, pick, Value) ->
    pick(Value, {1024, 256, 64, 8});
field(encode, sample_rate, pick, 1024) ->
    0;
field(encode, sample_rate, pick, 256) ->
    1;
field(encode, sample_rate, pick, 64) ->
    2;
field(encode, sample_rate, pick, 8) ->
    3;
field(decode, sing, pick, Value) ->
    pick(Value, {sequential, single_shot});
field(encode, sing, pick, single_shot) ->
    0;
field(encode, sing, pick, sequential) ->
    1;
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
field(decode, _, {unsigned, _}, Value) ->
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
field(_, Register, Type, _) ->
    error({unknown_register, Register, Type}).

-spec pick(non_neg_integer(), tuple()) -> term().
pick(Value, Elements) ->
    element(Value+1, Elements).

%--- Internal: Debug -----------------------------------------------------------
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
