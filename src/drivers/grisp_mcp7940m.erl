-module(grisp_mcp7940m).

-behaviour(gen_server).

% API
-export([start_link/2]).
-export([read/1]).
-export([write/2]).
-export([switch_hour_format/0]).
-export([get_time/0]).
-export([get_date/0]).

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

-define(MCPADDR, 16#6F).

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
              type    :: read_only | write_only | read_write | special}).

%--- Types ---------------------------------------------------------------------

-type state() :: #state{}.
-type reg() :: #reg{}.
-type reg_name() :: atom(). % TODO
-type type() :: two_digit | week_day | hour_format | am_pm | boolean. % TODO

%--- API -----------------------------------------------------------------------
start_link(Slot, _Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Slot, []).

-spec read(reg_name()) -> map().
read(RegisterName) ->
    call({read, RegisterName}).

-spec write(reg_name(), map()) -> ok.
write(RegisterName, Value) ->
    call({write, RegisterName, Value}).

-spec switch_hour_format() ->
    #{format => 12 | 24, am_pm := boolean(), hour => 0..23}.
switch_hour_format() ->
    call(switch_hour_format).

-spec get_time() -> map().
get_time() ->
    call(get_time).

-spec get_date() -> map().
get_date() ->
    call(get_date).

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
    {ok, NewState, Value} = read_register_file(State, RegFileName),
    {reply, Value, NewState};
execute_call({write, RegFileName, Value}, State) ->
    {ok, NewState} = write_register_file(State, RegFileName, Value),
    {reply, ok, NewState};
execute_call(switch_hour_format, State) ->
    {ok, NewState, CurrRtcHour} = read_register_file(State, rtchour),
    NewRtcHour = convert_hours(CurrRtcHour),
    {ok, NewState1} = write_register_file(NewState, rtchour, NewRtcHour),
    {reply, ok, NewState1};
execute_call(get_time, State) ->
    {ok, State1, #{rtcsec := Seconds}} = read_register_file(State, rtcsec),
    {ok, State2, #{rtcmin := Minutes}} = read_register_file(State1, rtcmin),
    {ok, State3, RtcHour} = read_register_file(State2, rtchour),
    Hours = maps:remove(format, RtcHour),
    HMS = maps:merge(#{sec => Seconds, min => Minutes}, Hours),
    {reply, HMS, State3};
execute_call(get_date, State) ->
    {ok, State1, #{wkday := WkDay}} = read_register_file(State, rtcwkday),
    {ok, State2, #{rtcdate := RtcDate}} = read_register_file(State1, rtcdate),
    {ok, State3, #{rtcmth := RtcMth}} = read_register_file(State2, rtcmth),
    {ok, State4, #{rtcyear := RtcYear}} = read_register_file(State3, rtcyear),
    {reply, #{wkday => WkDay, day => RtcDate,
              month => RtcMth, year => RtcYear}, State4};
execute_call(Call, _) ->
    error({unknown_call, Call}).

%--- Internal: Initialization --------------------------------------------------
-spec verify_component(state()) -> ok.
verify_component(#state{bus = Bus}) ->
    Detected = grisp_i2c:detect(Bus),
    case lists:member(?MCPADDR, Detected) of
        true -> ok;
        false -> error({mcp17260_not_detected_on_i2c, Detected})
    end.

-spec initialize_device(state()) -> state().
initialize_device(State) ->
    RtcSec = {rtcsec, #{st => true}},
    Configs = [RtcSec],
    lists:foldl(
      fun({RegFile, Value}, AccState) ->
              {ok, NewState} = write_register_file(AccState, RegFile, Value),
              NewState
      end, State, Configs).

% @doc builds a map of the registers of a component and with their addresses
-spec reg_files() -> RegisterFiles when
      RegisterFiles :: #{reg_name() => reg()}.
reg_files() -> #{
    rtcsec => #reg{addr = 16#00, type = read_write},
    rtcmin => #reg{addr = 16#01, type = read_write},
    rtchour => #reg{addr = 16#02, type = read_write},
    rtcwkday => #reg{addr = 16#03, type = read_write},
    rtcdate => #reg{addr = 16#04, type = read_write},
    rtcmth => #reg{addr = 16#05, type = read_write}, % Can't write lpyr ?
    rtcyear => #reg{addr = 16#06, type = read_write},
    control => #reg{addr = 16#07, type = read_write},
    osctrim => #reg{addr = 16#08, type = read_write},
    alm0sec => #reg{addr = 16#0A, type = read_write},
    alm0min => #reg{addr = 16#0B, type = read_write},
    alm0hour => #reg{addr = 16#0C, type = read_write},
    alm0wkday => #reg{addr = 16#0D, type = read_write},
    alm0date => #reg{addr = 16#0E, type = read_write},
    alm0mth => #reg{addr = 16#0F, type = read_write},
    alm1sec => #reg{addr = 16#11, type = read_write},
    alm1min => #reg{addr = 16#12, type = read_write},
    alm1hour => #reg{addr = 16#13, type = read_write},
    alm1wkday => #reg{addr = 16#14, type = read_write},
    alm1date => #reg{addr= 16#15, type = read_write},
    alm1mth => #reg{addr = 16#16, type = read_write}
}.

%--- Internal: Read and Write functions ----------------------------------------
-spec read_register_file(state(), reg()) -> {ok, state(), map()}.
read_register_file(State, RegFileName) ->
    #reg{addr = RegAddr} = maps:get(RegFileName, State#state.regs),
    {ok, BinValue} = read_request(State, RegAddr),
    {ok, State, decode_reg_file(RegFileName, BinValue)}.

-spec write_register_file(state(), reg(), map()) -> {ok, state()}.
write_register_file(State, RegFileName, Value) ->
    #reg{addr = RegAddr} = maps:get(RegFileName, State#state.regs),
    {ok, BinValue} = read_request(State, RegAddr),
    InitValue = decode_reg_file(RegFileName, BinValue),
    ValueToWrite = encode_reg_file(RegFileName, maps:merge(InitValue, Value)),
    write_request(State, RegAddr, ValueToWrite),
    {ok, State}.

-spec read_request(State, Register) -> {ok, Value} when
      State    :: state(),
      Register  :: non_neg_integer(),
      Value    :: binary().
read_request(#state{bus = Bus}, Register) ->
    [ok] = grisp_i2c:transfer(Bus, [{write, ?MCPADDR, 0, <<Register>>}]),
    timer:sleep(1),
    [Resp] = grisp_i2c:transfer(Bus, [{read, ?MCPADDR, 1, 1}]),
    debug_read(?MCPADDR, Register, Resp),
    {ok, Resp}.

-spec write_request(State, RegFile, Value) -> ok when
      State :: state(),
      RegFile :: non_neg_integer(),
      Value    :: binary().
write_request(#state{bus = Bus}, RegFile, Value) ->
    debug_write(?MCPADDR, RegFile, Value),
    [ok] = grisp_i2c:transfer(Bus, [{write, ?MCPADDR, 0, <<RegFile:8, Value/bitstring>>}]),
    ok.

%--- Internal: Register mapping ------------------------------------------------
-spec decode_reg_file(reg(), binary()) -> map() | no_return().
decode_reg_file(rtcsec, RawValue) ->
    <<ST:1,
      SecTens:3,
      SecOnes:4>> = RawValue,
    Seconds = decode(two_digit, {SecTens, SecOnes}),
    #{st => decode(boolean, ST),
      rtcsec => Seconds};
decode_reg_file(rtcmin, RawValue) ->
    <<_:1,
      MinTens:3,
      MinOnes:4>> = RawValue,
    Minutes = decode(two_digit, {MinTens, MinOnes}),
    #{rtcmin => Minutes};
decode_reg_file(rtchour, RawValue) ->
    case RawValue of
        <<_:1, 0:1, HrTens:2, HrOnes:4>> ->
            Hour = decode(two_digit, {HrTens, HrOnes}),
            #{format => decode(hour_format, 0),
              hour => Hour};
        <<_:1, 1:1, AmPm:1, HrTens:1, HrOnes:4>> ->
            Hour = decode(two_digit, {HrTens, HrOnes}),
            #{format => decode(hour_format, 1),
              am_pm => decode(am_pm, AmPm),
              hour => Hour}
    end;
decode_reg_file(rtcwkday, RawValue) ->
    <<_:2,
      OscRun:1,
      _:2,
      WkDay:3>> = RawValue,
    #{oscrun => decode(boolean, OscRun),
      wkday => decode(week_day, WkDay)};
decode_reg_file(rtcdate, RawValue) ->
    <<_:2,
      DateTens:2,
      DateOnes:4>> = RawValue,
    #{rtcdate => decode(two_digit, {DateTens, DateOnes})};
decode_reg_file(rtcmth, RawValue) ->
    <<_:2,
     LPYR:1,
     MthTens:1,
     MthOnes:4>> = RawValue,
    #{lpyr => decode(boolean, LPYR),
      rtcmth => decode(two_digit, {MthTens, MthOnes})};
decode_reg_file(rtcyear, RawValue) ->
    <<YrTens:4,
      YrOnes:4>> = RawValue,
    #{rtcyear => decode(two_digit, {YrTens, YrOnes})};
decode_reg_file(control, RawValue) ->
    <<Out:1,
      SQWEn:1,
      Alm1En:1,
      Alm0En:1,
      ExtOsc:1,
      CrsTrim:1,
      SQWFS:2>> = RawValue,
    #{out => decode(polarity, Out),
      sqwen => decode(boolean, SQWEn),
      alm1en => decode(boolean, Alm1En),
      alm0en => decode(boolean, Alm0En),
      extosc => decode(boolean, ExtOsc),
      crstrim => decode(boolean, CrsTrim),
      sqwfs => decode(mfp_freq, SQWFS)};
decode_reg_file(osctrim, RawValue) ->
    <<Sign:1,
      TrimVal:7>> = RawValue,
    #{sign => decode(sign, Sign),
      trimval => decode(trim, TrimVal)};
decode_reg_file(RegFile, RawValue) 
  when RegFile =:= alm0sec orelse RegFile =:= alm1sec ->
    <<_:1,
      SecTens:3,
      SecOnes:4>> = RawValue,
    #{RegFile => decode(two_digit, {SecTens, SecOnes})};
decode_reg_file(RegFile, RawValue)
  when RegFile =:= alm0min orelse RegFile =:= alm1min ->
    <<_:1,
      MinTens:3,
      MinOnes:4>> = RawValue,
    #{RegFile => decode(two_digit, {MinTens, MinOnes})};
decode_reg_file(RegFile, RawValue)
  when RegFile =:= alm0hour orelse RegFile =:= alm1hour ->
    case RawValue of
        <<0:1, 0:1, HrTens:2, HrOnes:4>> ->
            #{format => decode(hour_format, 0),
              hour => decode(two_digit, {HrTens, HrOnes})};
        <<0:1, 1:1, AmPm:1, HrTens:1, HrOnes:4>> ->
            #{format => decode(hour_format, 1),
              am_pm => decode(am_pm, AmPm),
              hour => decode(two_digit, {HrTens, HrOnes})}
    end;
decode_reg_file(RegFile, RawValue)
  when RegFile =:= alm0wkday orelse RegFile =:= alm1wkday ->
    <<AlmPol:1,
      AlmMsk:3,
      AlmIf:1,
      AlmWkDay:3>> = RawValue,
    #{almpol => decode(polarity, AlmPol),
      almmsk => decode(alm_mask, AlmMsk),
      almif => decode(boolean, AlmIf),
      almwkday => decode(week_day, AlmWkDay)};
decode_reg_file(RegFile, RawValue)
  when RegFile =:= alm0date orelse RegFile =:= alm1date ->
    <<_:2,
      DateTens:2,
      DateOnes:4>> = RawValue,
    #{RegFile => decode(two_digit, {DateTens, DateOnes})};
decode_reg_file(RegFile, RawValue)
  when RegFile =:= alm0mth orelse RegFile =:= alm1mth ->
    <<_:3,
      MthTens:1,
      MthOnes:4>> = RawValue,
    #{RegFile => decode(two_digit, {MthTens, MthOnes})};
decode_reg_file(RegFile, _) ->
    error({unknown_reg_file, RegFile}).

-spec encode_reg_file(reg(), map()) -> binary().
encode_reg_file(rtcsec, Value) ->
    #{st := ST,
      rtcsec := Seconds} = Value,
    {SecTens, SecOnes} = encode(two_digit, Seconds),
    <<(encode(boolean, ST)):1,
      SecTens:3,
      SecOnes:4>>;
encode_reg_file(rtcmin, Value) ->
    #{rtcmin := Minutes} = Value,
    {MinTens, MinOnes} = encode(two_digit, Minutes),
    <<0:1,
      MinTens:3,
      MinOnes:4>>;
encode_reg_file(rtchour, Value) ->
    case Value of
        #{format := 12, am_pm := AmPm, hour := Hour} ->
            if Hour > 12 ->
                   error({hour_not_match_format_12, Hour});
               true ->
                   {Tens, Ones} = encode(two_digit, Hour),
                   <<0:1,
                     (encode(hour_format, 12)):1,
                     (encode(am_pm, AmPm)):1,
                     Tens:1,
                     Ones:4>>
            end;
        #{format := 24, hour := Hour} ->
            {Tens, Ones} = encode(two_digit, Hour),
            <<0:1,
              (encode(hour_format, 24)):1,
              Tens:2,
              Ones:4>>
    end;
encode_reg_file(rtcwkday, Value) ->
    #{oscrun := OscRun,
      wkday := WkDay} = Value,
    <<0:2,
      (encode(boolean, OscRun)):1,
      0:2,
      (encode(week_day, WkDay)):3>>;
encode_reg_file(rtcdate, Value) ->
    #{rtcdate := Date} = Value,
    {Tens, Ones} = encode(two_digit, Date),
    <<0:2,
      Tens:2,
      Ones:4>>;
encode_reg_file(rtcmth, Value) ->
    #{lpyr := LPYR,
      rtcmth := RtcMth} = Value,
    {Tens, Ones} = encode(two_digit, RtcMth),
    <<0:2,
      (encode(boolean, LPYR)):1,
      Tens:1,
      Ones:4>>;
encode_reg_file(rtcyear, Value) ->
    #{rtcyear := Year} = Value,
    {Tens, Ones} = encode(two_digit, Year),
    <<Tens:4,
      Ones:4>>;
encode_reg_file(control, Value) ->
    #{out := Out,
      sqwen := SQWEn,
      alm1en := Alm1En,
      alm0en := Alm0En,
      extosc := ExtOsc,
      crstrim := CrsTrim,
      sqwfs := SQWFS} = Value,
    <<(encode(polarity, Out)):1,
      (encode(boolean, SQWEn)):1,
      (encode(boolean, Alm1En)):1,
      (encode(boolean, Alm0En)):1,
      (encode(boolean, ExtOsc)):1,
      (encode(boolean, CrsTrim)):1,
      (encode(mfp_freq, SQWFS)):1>>;
encode_reg_file(osctrim, Value) ->
    #{sign := Sign,
      trimval := TrimVal} = Value,
    <<(encode(sign, Sign)):1,
      (encode(trim, TrimVal)):7>>;
encode_reg_file(RegFile, Value)
  when RegFile =:= alm0sec orelse RegFile =:= alm1sec ->
    #{RegFile := AlmSec} = Value,
    {SecTens, SecOnes} = encode(two_digit, AlmSec),
    <<0:1,
      SecTens:3,
      SecOnes:4>>;
encode_reg_file(RegFile, Value)
  when RegFile =:= alm0min orelse alm1min ->
    #{RegFile := AlmMin} = Value,
    {MinTens, MinOnes} = encode(two_digit, AlmMin),
    <<0:1,
      MinTens:3,
      MinOnes:4>>;
encode_reg_file(RegFile, Value)
  when RegFile =:= alm0hour orelse RegFile =:= alm1hour ->
    case Value of
        #{format := 12, hour := Hour} when Hour > 12->
            error({hour_not_match_format_12, Hour});
        #{format := 12, am_pm := AmPm, hour := Hour} ->
            {HourTens, HourOnes} = encode(two_digit, Hour),
            <<0:1,
              (encode(hour_format, 12)):1,
              (encode(am_pm, AmPm)):1,
              HourTens:1,
              HourOnes:4>>;
        #{format := 24, hour := Hour} ->
            {HourTens, HourOnes} = encode(two_digit, Hour),
            <<0:1,
              (encode(hour_format, 24)):1,
              HourTens:2,
              HourOnes:4>>
    end;
encode_reg_file(RegFile, Value)
  when RegFile =:= alm0wkday orelse RegFile =:= alm1wkday ->
    #{almpol := AlmPol,
      almmsk := AlmMsk,
      almif := AlmIf,
      almday := AlmDay} = Value,
    <<(encode(polarity, AlmPol)):1,
      (encode(alm_mask, AlmMsk)):3,
      (encode(boolean, AlmIf)):1,
      (encode(week_day, AlmDay)):3>>;
encode_reg_file(RegFile, Value)
  when RegFile =:= alm0date orelse RegFile =:= alm1date ->
    #{RegFile := AlmDate} = Value,
    {DateTens, DateOnes} = encode(two_digit, AlmDate),
    <<0:2,
      DateTens:2,
      DateOnes:4>>;
encode_reg_file(RegFile, Value)
  when RegFile =:= alm0mth orelse RegFile =:= alm1mth ->
    #{RegFile := Month} = Value,
    {MthTens, MthOnes} = encode(two_digit, Month),
    <<0:3,
      MthTens:1,
      MthOnes:4>>;
encode_reg_file(RegFile, _) ->
    error({unknown_reg_file, RegFile}).

%--- Internal: Encoding and decoding functions ---------------------------------
-spec decode(Type, Value) -> Decoded | no_return() when
      Type :: type(),
      Value :: non_neg_integer(),
      Decoded :: term().
decode(two_digit, {Tens, Units}) ->
    (Tens * 10) + Units;
decode(week_day, Value) ->
    Value;
decode(hour_format, Value) ->
    case Value of
        0 -> 24;
        1 -> 12
    end;
decode(am_pm, Value) ->
    case Value of
        0 -> am;
        1 -> pm
    end;
decode(sign, Value) ->
    case Value of
        0 -> add;
        1 -> sub
    end;
decode(trim, Value) ->
    if Value > 0 ->
           Value - 1;
       Value =:= 0 ->
           disabled
    end;
decode(polarity, Value) ->
    case Value of
        0 -> low;
        1 -> high
    end;
decode(alm_mask, Value) ->
    element(Value + 1, {seconds, minutes, hours, day, date, rsvd, rsvd, all});
decode(mfp_freq, Value) ->
    element(Value + 1, {1, 4096, 8192, 32768}); % all values are in Hz
decode(boolean, Value) ->
    Value =:= 1;
decode(Type, Value) ->
    error({unknown_type, Type, Value}).

-spec encode(type(), term()) -> non_neg_integer().
encode(two_digit, Value) ->
    Tens = round(math:floor(Value/10)),
    Units = Value - (Tens*10),
    {Tens, Units};
encode(week_day, Value) ->
    Value;
encode(hour_format, Value) ->
    case Value of
        24 -> 0;
        12 -> 1
    end;
encode(am_pm, Value) ->
    case Value of
        am -> 0;
        pm -> 1
    end;
encode(sign, Value) ->
    case Value of
        add -> 0;
        sub -> 1
    end;
encode(trim, Value) ->
    if Value > 0 andalso Value =< 254 ->
           Value + 1;
       Value =:= disabled ->
           0;
       true ->
           error({trim_value_incorrect, Value})
    end;
encode(polarity, Value) ->
    case Value of
        low -> 0;
        high -> 1
    end;
encode(alm_mask, Value) ->
    case Value of
        seconds -> 0;
        minute -> 1;
        hours -> 2;
        day -> 3;
        date -> 4;
        all -> 7
    end;
encode(mfp_freq, Value) ->
    % All values are in Hz (datasheet gives some of them in KHz)
    case Value of
        1 -> 0;
        4096 -> 1;
        8192 -> 2;
        32768 -> 3
    end;
encode(boolean, Value) ->
    case Value of
        true -> 1;
        false -> 0
    end;
encode(Type, Value) ->
    error({unknown_type, Type, Value}).

%--- Internal: Utils -----------------------------------------------------------

-spec convert_hours(RtcHour) -> NewRtcHour when
      RtcHour :: #{format := 12, am_pm := am | pm, rtchour := Hour}
                 | #{format := 24, hour := Hour},
      NewRtcHour :: #{format := 12, am_pm := am | pm, rtchour := Hour}
                    | #{format := 24, hour := Hour},
      Hour :: non_neg_integer().
convert_hours(#{format := 12, am_pm := AmPm, hour := Hour}) ->
    case {AmPm, Hour} of
        {am, 12} -> % 12am is 00:00
            #{format => 24, hour => 0};
        {pm, 12} -> % 12pm is 12:00
            #{format => 24, hour => 12};
        {am, H} ->
            #{format => 24, hour => H};
        {pm, H} ->
            #{format => 23, hour => H + 12}
    end;
convert_hours(#{format := 24, hour := Hour}) ->
    case Hour of
        12 ->
            #{format => 12, am_pm => pm, hour => 12};
        0 ->
            #{format => 12, am_pm => am, hour => 12};
        _ when Hour > 12 ->
            #{format => 12, am_pm => pm, hour => Hour - 12};
        _ ->
            #{format => 12, am_pm => am, hour => Hour}
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
