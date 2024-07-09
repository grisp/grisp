-module(grisp_charger).

-behaviour(gen_server).

% API
-export([start_link/2]).
-export([voltage/0]).
-export([voltage/1]).
-export([power/0]).
-export([read/2]).

% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

% Includes
-include("grisp.hrl").
-include("grisp_i2c.hrl").

%--- MACROS --------------------------------------------------------------------

% 0x09 (9)   => LTC4155IUFD
% 0x13 (19)  => PAC1933T
% 0x36 (54)  => MAX17260SETD+ ?? Detects 0x37 (55) ??
% 0x6F (111) => MCP7940MT
% -define(ADDRESSES, [9, 19, 54, 111]).
-define(ADDRESSES, [9, 19, 111]).

-define(PAC1933ADDR, 16#13).
-define(MAX17260ADDR, 16#36).

-define(SOURCES, [wall, usb, battery]).

% PAC1933 IC specific values
-define(PFSR, 640). % c.f. Eq. 4-5

-define(CH1Bid, true).
-define(CH1TYPE, signed).

-define(CH2Bid, false).
-define(CH2TYPE, unsigned).

-define(CH3Bid, true).
-define(CH3TYPE, signed).

-define(PAC_MAP_FIELD(Name, Type, Value), Name => field(decode, pac1933, Name, Type, Value)).
-define(PAC_BIN_FIELD(Name, Type, Value), (field(encode, pac1933, Name, Type, Value))).

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
                comps ::#{component() => comp_info()}}).

-record(comp_info, {addr :: grisp_i2c:target_addr(),
                    reg_files :: #{atom() => reg_file()}}).

-record(reg_file, {addr :: non_neg_integer(),
              size :: non_neg_integer(),
              type :: read_only | write_only | read_write | special}).

%--- Types ---------------------------------------------------------------------

-type state() :: #state{}.
-type comp_info() :: #comp_info{}.
-type component() :: pac1933.
-type reg_file() :: #reg_file{}.

%--- Types: PAC1933
-type source() :: wall | usb | battery.

%--- API -----------------------------------------------------------------------
start_link(Slot, _Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Slot, []).

-spec read(component(), reg_file()) -> map().
read(IC, RegisterFile) ->
    call({read, IC, RegisterFile}).

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

init(i2c = Slot) ->
    Bus = grisp_i2c:open(i2c0),
    State = #state{
               bus = Bus,
               comps =  #{
                   pac1933 => init_comp(pac1933)
               }
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
execute_call({read, IC, RegFileName}, State) ->
    {ok, State1, Value} = read_reg_file(State, IC, RegFileName),
    {reply, Value, State1};
execute_call({write, IC, RegisterFile, Value}, State) ->
    #state{comps = #{IC := Comp}} = State,
    RegFileInfo = maps:get(RegisterFile, Comp#comp_info.reg_files),
    case RegFileInfo of
        #reg_file{type = read_only} ->
            error({read_only_register_file, IC, RegisterFile});
        #reg_file{type = special} ->
            error({special_register_file, IC, RegisterFile});
        _ -> ok
    end,
    {ok, State1} = write_reg_file(State, IC, RegisterFile, Value),
    {reply, ok, State1}.

-spec init_comp(component()) -> comp_info().
init_comp(Comp) ->
    Addr = case Comp of
               pac1933 -> ?PAC1933ADDR;
               _ -> error({unknown_comp, Comp})
           end,
    #comp_info{addr = Addr,
               reg_files = reg_files(Comp)}.

initialize_device(State) ->
    NegPwr = #{ch1_bidi => ?CH1Bid, ch1_bidv => ?CH1Bid,
               ch2_bidi => ?CH2Bid, ch2_bidv => ?CH2Bid,
               ch3_bidi => ?CH3Bid, ch3_bidv => ?CH3Bid},
    Configs = [
        {pac1933, [
            {neg_pwr, NegPwr}
        ]}
    ],
    lists:foldl(fun({IC, Confs}, AccState) ->
                        configure(AccState, IC, Confs)
                end, State, Configs).

-spec configure(state(), component(), [{atom(), map()}]) -> state().
configure(State, IC, Configs) ->
    lists:foldl(
      fun({RegFile, Config}, AccState) ->
              {ok, NextState} = write_reg_file(AccState, IC, RegFile, Config),
              NextState
      end, State, Configs).

% @doc builds a map of the registers of a component and with their addresses
-spec reg_files(Comp) -> RegisterFiles when
      Comp      :: component(),
      RegisterFiles :: #{atom() => reg_file()}.
reg_files(pac1933) ->
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
-spec read_reg_file(state(), component(), atom()) -> {ok, state(), map()}.
read_reg_file(State, IC, RegFileName) ->
    Comp = maps:get(IC, State#state.comps),
    CompAddr = Comp#comp_info.addr,
    #reg_file{addr = RAddr,
              size = RSize} = maps:get(RegFileName, Comp#comp_info.reg_files),
    {ok, ReadValue} = read_request(State, CompAddr, RAddr, RSize),
    DecodedValue = reg_file(decode, IC, RegFileName, ReadValue),
    {ok, State, DecodedValue}.

% @doc writing the given value(s) to the register file of the given component
% the function makes sure to not overwrite the other values of the register file
% @end
-spec write_reg_file(state(), component(), atom(), map()) -> {ok, state()}.
write_reg_file(State, IC, RegFileName, Value) ->
    Comp = maps:get(IC, State#state.comps),
    CompAddr = Comp#comp_info.addr,
    #reg_file{addr = RFAddr} = maps:get(RegFileName, Comp#comp_info.reg_files),

    % Reading the current value of the register file
    {ok, State1, CurrVal} = read_reg_file(State, IC, RegFileName),

    % Preparing the value to write and writting
    NewVal = maps:merge(CurrVal, Value),
    logger:notice("Result merge: ~p~n", [NewVal]),
    ValueToWrite = reg_file(encode, IC, RegFileName, NewVal),
    write_request(State1, CompAddr, RFAddr, ValueToWrite),
    {ok, State1}.

%--- Internal: Verifiers -------------------------------------------------------
-spec verify_device(state()) -> ok.
verify_device(State) ->
    verify_components(State),

    lists:foldl(fun verify_reg_file/2, State, [
        {?PAC1933ADDR, 16#FD, <<2#01011010>>, 1} %,
        % {?MAX17260ADDR, 16#21, <<2#01011010>>}
    ]).

-spec verify_components(state()) -> ok.
verify_components(#state{bus = Bus}) ->
    Detected = grisp_i2c:detect(Bus),
    logger:notice("Detected: ~p", [Detected]),
    NonDetectedAddr = lists:filter(fun(Addr) ->
                                           not lists:member(Addr, Detected)
                                   end, ?ADDRESSES),
    NbrIC = length(?ADDRESSES),
    NbrNDetected = length(NonDetectedAddr),
    if NbrIC == NbrNDetected ->
           error(charger_not_detected);
       NbrNDetected > 0 ->
           error({charger_ic_missing, list_to_tuple(NonDetectedAddr)});
       true ->
           ok
    end.

-spec verify_reg_file({CompAddr, RegFile, Value, Size}, State) -> State when
      CompAddr :: grisp_i2c:target_addr(),
      RegFile  :: non_neg_integer(),
      Value    :: <<_:8>>,
      Size     :: pos_integer(),
      State    :: state().
verify_reg_file({CompAddr, RegFile, Value, Size}, State) ->
    case read_request(State, CompAddr, RegFile, Size) of
        {ok, Value} -> State;
        {ok, Other} -> error({register_mismatch, CompAddr, RegFile, Other})
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
    pac1931_refreshv(State),
    timer:sleep(2),
    RegFileName = case Source of
                   usb -> vbus1;
                   wall -> vbus2;
                   battery -> vbus3
               end,
    {ok, _, VBus} = read_reg_file(State, pac1933, RegFileName),
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
    pac1931_refreshv(State),
    timer:sleep(2),
    RegFileName = case Source of
                      usb -> vpower1;
                      wall -> vpower2;
                      battery -> vpower3
                  end,
    {ok, _, VPower} = read_reg_file(State, pac1933, RegFileName),
    logger:notice("VPower: ~p", [VPower]),
    VPower.

%--- Internals: Utils ----------------------------------------------------------
% @doc Sends the refresh command for Vbus, Vsense data only
pac1931_refreshv(#state{bus = Bus}) ->
    [ok] = grisp_i2c:transfer(Bus, [{write, ?PAC1933ADDR, 0, <<16#1F>>}]),
    timer:sleep(1).

-spec read_request(State, CompAddr, RegFile, Size) -> {ok, Value} when
      State    :: state(),
      CompAddr :: grisp_i2c:target_addr(),
      RegFile  :: non_neg_integer(),
      Size     :: pos_integer(),
      Value    :: <<_:8>>.
read_request(#state{bus = Bus}, CompAddr, RegFile, Size) ->
    [ok] = grisp_i2c:transfer(Bus, [{write, CompAddr, 0, <<RegFile>>}]),
    timer:sleep(1),
    [Resp] = grisp_i2c:transfer(Bus, [{read, CompAddr, 1, Size}]),
    debug_read(CompAddr, RegFile, Resp),
    {ok, Resp}.

-spec write_request(State, CompAddr, RegFile, Value) -> ok when
      State :: state(),
      CompAddr :: grisp_i2c:target_addr(),
      RegFile :: non_neg_integer(),
      Value    :: binary().
write_request(#state{bus = Bus}, CompAddr, RegFile, Value) ->
    debug_write(CompAddr, RegFile, Value),
    [ok] = grisp_i2c:transfer(Bus, [{write, CompAddr, 0, <<RegFile:8, Value/bitstring>>}]),
    ok.

%--- Internals: Encode/Decode --------------------------------------------------
-spec reg_file(Type, Comp, RegFile, Value) -> Ret when
      Type    :: encode | decode,
      Comp    :: component(),
      RegFile :: atom(),
      Value   :: binary() | map(),
      Ret     :: binary() | map().
reg_file(decode, pac1933, RegFile, ReadValue) when RegFile == ctrl orelse
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
        ?PAC_MAP_FIELD(sample_rate, pick, SampleRate),
        ?PAC_MAP_FIELD(sleep, boolean, Sleep),
        ?PAC_MAP_FIELD(sing, pick, Sing),
        ?PAC_MAP_FIELD(alert_pin, boolean, AlertPin),
        ?PAC_MAP_FIELD(alert_cc, boolean, AlertCC),
        ?PAC_MAP_FIELD(ovf_alert, boolean, OvfAlert),
        ?PAC_MAP_FIELD(ovf, boolean, Ovf)
     };
reg_file(encode, pac1933, ctrl, Value) ->
    #{sample_rate := SampleRate,
      sleep := Sleep,
      sing := Sing,
      alert_pin := AlertPin,
      alert_cc := AlertCC,
      ovf_alert := OvfAlert,
      ovf := Ovf
     } = Value,
    <<?PAC_BIN_FIELD(sample_rate, pick, SampleRate):1,
      ?PAC_BIN_FIELD(sleep, boolean, Sleep):1,
      ?PAC_BIN_FIELD(sing, pick, Sing):1,
      ?PAC_BIN_FIELD(alert_pin, boolean, AlertPin):1,
      ?PAC_BIN_FIELD(alert_cc, boolean, AlertCC):1,
      ?PAC_BIN_FIELD(ovf_alert, boolean, OvfAlert):1,
      ?PAC_BIN_FIELD(ovf, boolean, Ovf):1>>;
reg_file(decode, pac1933, acc_count, ReadValue) ->
    <<AccCount:24>> = ReadValue,
    #{
      ?PAC_MAP_FIELD(acc_count, {unsigned, 24}, AccCount)
     };
reg_file(decode, pac1933, vpower1_acc, ReadValue) ->
    <<VPower1Acc:48>> = ReadValue,
    #{
      ?PAC_MAP_FIELD(vpower1_acc, {?CH1TYPE, 48}, VPower1Acc)
     };
reg_file(decode, pac1933, vpower2_acc, ReadValue) ->
    <<VPower2Acc:48>> = ReadValue,
    #{
      ?PAC_MAP_FIELD(vpower2_acc, {?CH2TYPE, 48}, VPower2Acc)
     };
reg_file(decode, pac1933, vpower3_acc, ReadValue) ->
    <<VPower3Acc:48>> = ReadValue,
    #{
      ?PAC_MAP_FIELD(vpower3_acc, {?CH3TYPE, 48}, VPower3Acc)
     };
reg_file(decode, pac1933, RegFile, ReadValue)
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
      ?PAC_MAP_FIELD(RegFile, {Type, 16}, Value)
     };
reg_file(decode, pac1933, vpower1, ReadValue) ->
    <<VPower1:28,
      _:4>> = ReadValue,
    #{
      ?PAC_MAP_FIELD(vpower1, {?CH1TYPE, 28}, VPower1)
     };
reg_file(decode, pac1933, vpower2, ReadValue) ->
    <<VPower2:28,
      _:4>> = ReadValue,
    #{
      ?PAC_MAP_FIELD(vpower2, {?CH2TYPE, 28}, VPower2)
     };
reg_file(decode, pac1933, vpower3, ReadValue) ->
    <<VPower3:28,
      _:4>> = ReadValue,
    #{
      ?PAC_MAP_FIELD(vpower3, {?CH3TYPE, 28}, VPower3)
     };
reg_file(decode, pac1933, channel_dis, ReadValue) ->
    <<Ch1Off:1,
      Ch2Off:1,
      Ch3Off:1,
      Ch4Off:1,
      Timeout:1,
      ByteCount:1,
      NoSkip:1,
      _:1>> = ReadValue,
    #{
      ?PAC_MAP_FIELD(ch1_off, boolean, Ch1Off),
      ?PAC_MAP_FIELD(ch2_off, boolean, Ch2Off),
      ?PAC_MAP_FIELD(ch3_off, boolean, Ch3Off),
      ?PAC_MAP_FIELD(ch4_off, boolean, Ch4Off),
      ?PAC_MAP_FIELD(timeout, boolean, Timeout),
      ?PAC_MAP_FIELD(byte_count, boolean, ByteCount),
      ?PAC_MAP_FIELD(no_skip, boolean, NoSkip)
     };
reg_file(encode, pac1933, channel_dis, Value) ->
    #{ch1_off := Ch1Off,
      ch2_off := Ch2Off,
      ch3_off := Ch3Off,
      ch4_off := Ch4Off,
      timeout := Timeout,
      byte_count := ByteCount,
      no_skip := NoSkip
     } = Value,
    <<?PAC_BIN_FIELD(ch1_off, boolean, Ch1Off),
      ?PAC_BIN_FIELD(ch2_off, boolean, Ch2Off),
      ?PAC_BIN_FIELD(ch3_off, boolean, Ch3Off),
      ?PAC_BIN_FIELD(ch4_off, boolean, Ch4Off),
      ?PAC_BIN_FIELD(timeout, boolean, Timeout),
      ?PAC_BIN_FIELD(byte_count, boolean, ByteCount),
      ?PAC_BIN_FIELD(no_skip, boolean, NoSkip),
      0:1>>;
reg_file(decode, pac1933, RegFile, ReadValue) when RegFile == neg_pwr orelse
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
      ?PAC_MAP_FIELD(ch1_bidi, boolean, Ch1BidI),
      ?PAC_MAP_FIELD(ch2_bidi, boolean, Ch2BidI),
      ?PAC_MAP_FIELD(ch3_bidi, boolean, Ch3BidI),
      ?PAC_MAP_FIELD(ch1_bidv, boolean, Ch1BidV),
      ?PAC_MAP_FIELD(ch2_bidv, boolean, Ch2BidV),
      ?PAC_MAP_FIELD(ch3_bidv, boolean, Ch3BidV)
     };
reg_file(encode, pac1933, neg_pwr, Value) ->
    #{ch1_bidi := Ch1BidI,
      ch2_bidi := Ch2BidI,
      ch3_bidi := Ch3BidI,
      ch1_bidv := Ch1BidV,
      ch2_bidv := Ch2BidV,
      ch3_bidv := Ch3BidV} = Value,
    <<?PAC_BIN_FIELD(ch1_bidi, boolean, Ch1BidI):1,
      ?PAC_BIN_FIELD(ch2_bidi, boolean, Ch2BidI):1,
      ?PAC_BIN_FIELD(ch3_bidi, boolean, Ch3BidI):1,
      0:1,
      ?PAC_BIN_FIELD(ch1_bidv, boolean, Ch1BidV):1,
      ?PAC_BIN_FIELD(ch2_bidv, boolean, Ch2BidV):1,
      ?PAC_BIN_FIELD(ch3_bidv, boolean, Ch3BidV):1,
      0:1>>;
reg_file(decode, pac1933, slow, ReadValue) ->
    <<Slow:1,
      SlowLH:1,
      SlowHL:1,
      RRise:1,
      RVRise:1,
      RFall:1,
      RVFall:1,
      POR:1>> = ReadValue,
    #{
      ?PAC_MAP_FIELD(slow, bit, Slow),
      ?PAC_MAP_FIELD(slow_lh, bit, SlowLH),
      ?PAC_MAP_FIELD(slow_hl, bit, SlowHL),
      ?PAC_MAP_FIELD(r_rise, bit, RRise),
      ?PAC_MAP_FIELD(r_v_rise, bit, RVRise),
      ?PAC_MAP_FIELD(r_fall, bit, RFall),
      ?PAC_MAP_FIELD(r_v_fall, bit, RVFall),
      ?PAC_MAP_FIELD(por, bit, POR)
     };
reg_file(encode, pac1933, slow, Value) ->
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
reg_file(decode, pac1933, RegFile, ReadValue)
  when RegFile == channel_dis_act orelse RegFile == channel_dis_lat ->
    <<Ch1Off:1,
      Ch2Off:1,
      Ch3Off:1,
      Ch4Off:1,
      _:4>> = ReadValue,
    #{
      ?PAC_MAP_FIELD(ch1_off, boolean, Ch1Off),
      ?PAC_MAP_FIELD(ch2_off, boolean, Ch2Off),
      ?PAC_MAP_FIELD(ch3_off, boolean, Ch3Off),
      ?PAC_MAP_FIELD(ch4_off, boolean, Ch4Off)
     };
reg_file(decode, pac1933, RegFile, ReadValue)
  when RegFile == prod_id orelse RegFile == man_id orelse RegFile == rev_id ->
    #{RegFile => ReadValue};
reg_file(_, Comp, RegFile, _) ->
    error({unknown_register_file, Comp, RegFile}).

-spec field(Action, Comp, RegFile, Type, Value) -> Ret when
    Action  :: encode | decode,
    Comp    :: component(),
    RegFile :: atom(),
    Type    :: pick | {signed, Size} | unsigned | boolean,
    Size    :: 16 | 32,
    Value   :: map() | binary(),
    Ret     :: map() | binary().
field(decode, pac1933, sample_rate, pick, Value) ->
    pick(Value, {1024, 256, 64, 8});
field(encode, pac1933, sample_rate, pick, 1024) ->
    0;
field(encode, pac1933, sample_rate, pick, 256) ->
    1;
field(encode, pac1933, sample_rate, pick, 64) ->
    2;
field(encode, pac1933, sample_rate, pick, 8) ->
    3;
field(decode, pac1933, sing, pick, Value) ->
    pick(Value, {sequential, single_shot});
field(encode, pac1933, sing, pick, single_shot) ->
    0;
field(encode, pac1933, sing, pick, sequential) ->
    1;
field(decode, pac1933, _, {signed, Size}, Value) ->
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
field(decode, pac1933, _, {unsigned, _}, Value) ->
    Value;
field(decode, pac1933, _, boolean, Value) ->
    case Value of
        0 -> false;
        1 -> true
    end;
field(encode, pac1933, _, boolean, Value) ->
    case Value of
        false -> 0;
        true -> 1
    end;
field(decode, pac1933, _, bit, Value) ->
    Value;
field(_, Comp, Register, Type, _) ->
    error({unknown_register, Comp, Register, Type}).

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
