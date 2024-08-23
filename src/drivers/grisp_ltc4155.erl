-module(grisp_ltc4155).

-behaviour(gen_server).

% API
-export([start_link/2]).
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

-define(LTC4155ADDR, 16#09).

%--- Record --------------------------------------------------------------------

-record(state, {bus :: grisp_i2c:bus(),
                regs :: #{reg_name() => reg()}
                }).

% @doc This record represent a single register
% @param addr: the sub address of the register
% @param padding: the number of bits from MSB to ignore
% @param size: the size (in bits) of the register
% @param v_type: the type of value stored in the register (boolean, integer, ...)
% @param type: the type of the register (read only, read write, ...)
% @end
-record(reg, {addr    :: non_neg_integer(),
              padding :: non_neg_integer(),
              size    :: non_neg_integer(),
              mapping :: type(),
              type    :: read_only | write_only | read_write | special}).

%--- Types ---------------------------------------------------------------------

-type state() :: #state{}.
-type reg() :: #reg{}.

-type reg_name() :: disable_input_ucvl.

-type type() :: flag | special | {pick, tuple()}.
-type flag() :: 0 | 1.

%--- API -----------------------------------------------------------------------
start_link(Slot, _Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Slot, []).

-spec read(reg_name()) -> term().
read(RegisterName) ->
    call({read, RegisterName}).

%--- gen_server callbacks ------------------------------------------------------

init(Slot) ->
    Bus = grisp_i2c:open(i2c0),
    State =  #state{bus = Bus,
                    regs = reg_files()
                   },
    verify_component(State),
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
%--- Internal: Call handling ---------------------------------------------------
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
    RegFileInfo = maps:get(RegisterFile, State#state.regs),
    case RegFileInfo of
        #reg{type = read_only} ->
            error({read_only_register_file, RegisterFile});
        #reg{type = special} ->
            error({special_register_file, RegisterFile});
        _ -> ok
    end,
    {ok, State1} = write_reg_file(State, RegisterFile, Value),
    {reply, ok, State1}.


%--- Internal: Initialization --------------------------------------------------
-spec verify_component(state()) -> ok.
verify_component(#state{bus = Bus}) ->
    Detected = grisp_i2c:detect(Bus),
    case lists:member(?LTC4155ADDR, Detected) of
        true -> ok;
        false -> error({ltc4155_not_detected_on_i2c, Detected})
    end.

-spec initialize_device(state()) -> state().
initialize_device(State) ->
    State. % TODO

% @doc builds a map of the registers of a component and with their addresses
-spec reg_files() -> RegisterFiles when
      RegisterFiles :: #{reg_name() => reg()}.
reg_files() -> #{
    disable_input_ucvl =>
        #reg{addr = 16#00, padding = 0, size = 1, type = read_write,
             mapping = flag},
    en_bat_conditioner =>
        #reg{addr = 16#00, padding = 1, size = 1, type = read_write,
             mapping = flag},
    lockout_id_pin =>
        #reg{addr = 16#00, padding = 2, size = 1, type = read_write,
             mapping = flag},
    usbilim =>
        #reg{addr = 16#00, padding = 3, size = 5, type = read_write,
             mapping = {pick, {100, 500, 600, 700, 800, 900, 1000, 1250, 1500,
                               1750, 2000, 2250, 2500, 2750, 3000, 2.5, clprog}}},
    priority =>
        #reg{addr = 16#01, padding = 0, size = 1, type = read_write,
             mapping = flag},
    timer =>
        #reg{addr = 16#01, padding = 1, size = 2, type = read_write,
             mapping = {pick, {4, 8, 1, 2}}},
    wallilim =>
        #reg{addr = 16#01, padding = 3, size = 5, type = read_write,
             mapping = {pick, {100, 500, 600, 700, 800, 900, 1000, 1250, 1500,
                               1750, 2000, 2250, 2500, 2750, 3000, 2.5, clprog}}},
    icharge =>
        #reg{addr = 16#02, padding = 0, size = 4, type = read_write,
             mapping = {pick, {disabled, 12.5, 18.75, 25.0, 31.25, 37.50, 43.75,
                               50.0, 56.25, 62.50, 68.75, 75.0, 81.25, 87.50, 93.75, 100.0}}},
    vfloat =>
        #reg{addr = 16#02, padding = 4, size = 2, type = read_write,
             mapping = {pick, {4.05, 4.10, 4.15, 4.20}}},
    cxset =>
        #reg{addr = 16#02, padding = 6, size = 2, type = read_write,
             mapping = {pick, {10, 20, 2, 5}}},
    charger_status =>
        #reg{addr = 16#03, padding = 0, size = 3, type = read,
             mapping = {pick, {off, low_battery_voltage, constant_current,
                               vprog_gt_vcx, vprog_st_vcx, ntc_too_warm,
                               ntc_too_cold, ntc_hot_fault}}},
    id_pin_detect =>
        #reg{addr = 16#03, padding = 3, size = 1, type = read,
             mapping = flag},
    otg_enabled =>
        #reg{addr = 16#03, padding = 4, size = 1, type = read,
             mapping = flag},
    ntcstat =>
        #reg{addr = 16#03, padding = 5, size = 2, type = read,
             mapping = {pick, {ntc_normal, ntc_too_cold, ntc_too_warm, ntc_hot_fault}}},
    lowbat =>
        #reg{addr = 16#03, padding = 7, size = 1, type = read,
             mapping = flag},
    ext_pwr_good =>
        #reg{addr = 16#04, padding = 0, size = 1, type = read,
             mapping = flag},
    usbsns_good =>
        #reg{addr = 16#04, padding = 1, size = 1, type = read,
             mapping = flag},
    at_input_ilim =>
        #reg{addr = 16#04, padding = 1, size = 1, type = read,
             mapping = flag},
    input_uvcl_active =>
        #reg{addr = 16#04, padding = 1, size = 1, type = read,
             mapping = flag},
    ovp_active =>
        #reg{addr = 16#04, padding = 5, size = 1, type = read,
             mapping = flag},
    bad_cell =>
        #reg{addr = 16#04, padding = 7, size = 1, type = read,
             mapping = flag},
    ntcval =>
        #reg{addr = 16#05, padding = 0, size = 7, type = read,
             mapping = special},
    ntc_warning =>
        #reg{addr = 16#05, padding = 7, size = 1, type = read,
             mapping = flag},
    enable_charger_int =>
        #reg{addr = 16#06, padding = 0, size = 1, type = read_write,
             mapping = flag},
    enable_fault_int =>
        #reg{addr = 16#06, padding = 1, size = 1, type = read_write,
             mapping = flag},
    enable_extpwr_int =>
        #reg{addr = 16#06, padding = 2, size = 1, type = read_write,
             mapping = flag},
    enable_otg_int =>
        #reg{addr = 16#06, padding = 3, size = 1, type = read_write,
             mapping = flag},
    enable_at_ilim_int =>
        #reg{addr = 16#06, padding = 4, size = 1, type = read_write,
             mapping = flag},
    enable_input_uvcl_int =>
        #reg{addr =  16#06, padding = 5, size = 1, type = read_write,
             mapping = flag},
    request_otg =>
        #reg{addr =  16#06, padding = 6, size = 1, type = read_write,
             mapping = flag}
}.

%--- Internal: Read and Write functions ----------------------------------------

% @doc reading the register file of the given component
-spec read_reg_file(state(), reg_name()) -> {ok, state(), map()}.
read_reg_file(State, RegName) ->
    #reg{addr = Addr,
         padding = Padding,
         mapping = VType,
         size = RSize} = maps:get(RegName, State#state.regs),
    {ok, ReadValue} = read_request(State, Addr, 1),
    <<_:Padding, RawValue:RSize, _:(8-Padding-RSize)>> = ReadValue,
    DecodedValue = decode(RegName, VType, RawValue),
    {ok, State, DecodedValue}.

% @doc writing the given value(s) to the register file of the given component
% the function makes sure to not overwrite the other values of the register file
% @end
-spec write_reg_file(state(), reg_name(), map()) -> {ok, state()}.
write_reg_file(State, RegName, Value) ->
    #reg{addr = Addr,
         padding = Padding,
         size = RSize,
         mapping = VType} = maps:get(RegName, State#state.regs),

    % Reading the current value of the register file
    {ok, State1, CurrVal} = read_reg_file(State, RegName),

    Offset = 8-Padding-RSize,
    <<MSBs:Padding, _:RSize, LSBs:Offset>> = CurrVal,

    RawValue = encode(VType, Value),

    NewRegFileVal = <<MSBs:Padding, RawValue:RSize, LSBs:Offset>>,
    write_request(State1, Addr, NewRegFileVal),
    {ok, State1}.

-spec read_request(State, Register, Size) -> {ok, Value} when
      State    :: state(),
      Register  :: non_neg_integer(),
      Size     :: pos_integer(),
      Value    :: binary().
read_request(#state{bus = Bus}, Register, Size) ->
    [ok] = grisp_i2c:transfer(Bus, [{write, ?LTC4155ADDR, 0, <<Register>>}]),
    timer:sleep(1),
    [Resp] = grisp_i2c:transfer(Bus, [{read, ?LTC4155ADDR, 1, Size}]),
    debug_read(?LTC4155ADDR, Register, Resp),
    {ok, Resp}.

-spec write_request(State, RegFile, Value) -> ok when
      State :: state(),
      RegFile :: non_neg_integer(),
      Value    :: binary().
write_request(#state{bus = Bus}, RegFile, Value) ->
    debug_write(?LTC4155ADDR, RegFile, Value),
    [ok] = grisp_i2c:transfer(Bus, [{write, ?LTC4155ADDR, 0, <<RegFile:8, Value/bitstring>>}]),
    ok.

%--- Internal: Encoding and decoding functions ---------------------------------
-spec decode(reg_name(), type(), non_neg_integer()) -> boolean | atom().
decode(ntcval, special, Value) ->
    % TODO convert to temp
    Value;
decode(_, flag, Value) ->
    Value;
decode(RegName, {pick, PickValues}, Value)
  when RegName == wallilim orelse RegName == usbilim ->
    case Value of
        31 -> clprog;
        _ -> pick(Value, PickValues)
    end;
decode(_, {pick, PickValues}, Value) ->
    pick(Value, PickValues);
decode(RegName, Type, Value) ->
    error({unknown_type, RegName, Type, Value}).

% TODO adapt encode to fit decode specs
-spec encode(type(), flag() | atom()) -> non_neg_integer().
encode({pick, Elements}, Value) ->
    find(tuple_to_list(Elements), Value);
encode(flag, Value) ->
    Value;
encode(Type, Value) ->
    error({unknown_type, Type, Value}).

-spec pick(non_neg_integer(), tuple()) -> term().
pick(Value, Elements) ->
    element(Value+1, Elements).

find(List, Element) ->
    find(List, Element, 1).

find([], Element, _) ->
    error({no_element, Element});
find([H|T], Element, N) ->
    case H of
        Element -> N;
        _ -> find(T, Element, N+1)
    end.
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
