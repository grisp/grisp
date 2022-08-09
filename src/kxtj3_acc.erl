% @doc API for the
% <a href="https://www.mouser.it/datasheet/2/348/rohm_s_a0002904669_1-2281852.pdf">
% kxtj3-1057 002
% </a>.
%
% Start the driver with
% ```
% 1> grisp:add_device(i2c, kxtj3_acc).
% '''
% @end
-module(kxtj3_acc).
-behaviour(gen_server).

% API
-export([start_link/2]).
-export([measurements/0]).
-export([reset/1]).
-export([check_interrupt/0]).
-export([clear_interrupt/0]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

% defines

-define(DEVICE_ADR, 16#0E).
-define(WHO_AM_I, 16#0F).
-define(CTRL_REG1, 16#1B).
    -define(PC1,    (1 bsl 7)).
    -define(RES,    (1 bsl 6)).
    -define(DRDYE,  (1 bsl 5)).
    -define(GSEL1,  (1 bsl 4)).
    -define(GSEL0,  (1 bsl 3)).
    -define(EN16G,  (1 bsl 2)).
    -define(WUFE,   (1 bsl 1)).
-define(CTRL_REG2, 16#1D).
    -define(SRST,    (1 bsl 7)).
    -define(DCST,  (1 bsl 4)).
    -define(OWUFA,  (1 bsl 2)).
    -define(OWUFB,   (1 bsl 1)).
    -define(OWUFC,   (1 bsl 0)).
-define(INT_CTRL_REG1, 16#1E).
    -define(IEN,    (1 bsl 5)).
    -define(IEA,    (1 bsl 4)).
    -define(IEL,    (1 bsl 3)).
    -define(STPOL,  (1 bsl 1)).
-define(WAKEUP_COUNTER, 16#29).
-define(NA_COUNTER, 16#2A).
-define(WAKEUP_THRESHOLD_H, 16#6A).
-define(WAKEUP_THRESHOLD_L, 16#6B).
-define(INT_SOURCE1, 16#16).
-define(INT_SOURCE2, 16#17).
-define(INT_REL, 16#1A).
-define(STATUS_REG, 16#18).
    -define(INT, (1 bsl 4)).
-define(DATA_CTRL_REG, 16#21).
    -define(OSAA,  (1 bsl 3)).
    -define(OSAB,  (1 bsl 2)).
    -define(OSAC,  (1 bsl 1)).
    -define(OSAD,  (1 bsl 0)).
-define(XOUT_L, 16#06).
-define(XOUT_H, 16#07).
-define(YOUT_L, 16#08).
-define(YOUT_H, 16#09).
-define(ZOUT_L, 16#0A).
-define(ZOUT_H, 16#0B).

-define(DEVICE_ID, 16#35).

%--- Types -------------------------------------------------------------------
%
-type wakeup_config() ::
    #{
        threshold => float(),
        delay => float(),
        cooldown => float(),
        data_rate => pos_integer() % 1 to 8
    }.

-type config() ::
    #{
        g_range => 2 | 4 | 8 | 16,
        resolution => 8 | 12 | 14,
        data_rate =>  pos_integer(), % 1 to 12
        wuf => wakeup_config() | false
    }.
%
%--- Records -------------------------------------------------------------------
%
-record(state, {
    bus,
    g_range = 2     :: 2 | 4 | 8 | 16, % positive and negative
    resolution = 8  :: 8 | 12 | 14 % in bits
}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Slot, Opts], []).

% @doc Measure the triple axiss acceleration.
%
% === Example ===
% ```
% 2> kxtj3_acc:measurements().
% {0.0, 0.0, 1.0}
% '''
-spec measurements() -> { X :: float(), Y :: float(), Z :: float()}.
measurements() ->
    gen_server:call(?MODULE, measurements).

% @doc Repeats the setup procedure,
% allowing to change the initial device settings.
%
% === Example ===
% ```
% 2> kxtj3_acc:reset(#{g_range => 16, resolution => 14}).
% ok
% '''
-spec reset(Opts :: config()) -> ok.
reset(Opts) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Opts}).

% @doc Checks if an interrupt occurred
-spec check_interrupt() -> true | false.
check_interrupt() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME).

% @doc Sets the interrut pin to low
-spec clear_interrupt() -> ok.
clear_interrupt() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME).

%--- Callbacks -----------------------------------------------------------------

% @private
init([i2c = Slot, Opts]) ->
    Bus = grisp_i2c:open(i2c1),
    verify_device(Bus),
    grisp_devices:register(Slot, ?MODULE),
    State = setup_device(Bus, Opts),
    {ok, State}.

% @private
handle_call(measurements, _From, State) ->
    {X, Y, Z} = read_registers(State),
    Result =  {convert(X, State), convert(Y, State), convert(Z, State)},
    {reply, Result, State};
handle_call({reset, Opts}, _From, #state{bus = Bus}) ->
    State = setup_device(Bus, Opts),
    {reply, ok, State};
handle_call(check_interrupt, _From, #state{bus = Bus} = S) ->
    Reply = case device_read(Bus, ?STATUS_REG) of
        {ok, <<Val:8>>} when Val == ?INT -> true;
        {ok, _} -> false
    end,
    {reply, Reply, S};
handle_call(clear_interrupt, _From, #state{bus = Bus} = S) ->
    device_read(Bus, ?INT_REL),
    {reply, ok, S};
handle_call(Request, _From, _State) ->
    error({unknown_cast, Request}).

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------

verify_device(Bus) ->
    {ok, <<DeviceID:8>>} = device_read(Bus, ?WHO_AM_I),
    case DeviceID of
        ?DEVICE_ID -> ok;
        Other -> error({device_mismatch, Other})
    end.

setup_device(Bus, Opts) ->
    unset_bits(Bus, ?CTRL_REG1, ?PC1), % stop operations
    device_write(Bus, ?CTRL_REG1, <<0:8>>), % set all bits to 0
    G = maps:get(g_range, Opts, 2), % def 2g
    R = maps:get(resolution, Opts, 8), % def 8 bit
    DR = maps:get(data_rate, Opts, 7), % def 50Hz
    set_range_and_resolution(Bus, G, R),
    set_output_datarates(Bus, DR),
    Wuf = maps:get(wuf, Opts, false),
    set_wake_up_function(Bus, Wuf),
    set_bits(Bus, ?CTRL_REG1, ?PC1),
    #state{bus = Bus, g_range = G, resolution = R}.

set_bits(Bus, Reg, Mask) ->
    {ok, <<RegState:8>>} = device_read(Bus, Reg),
    device_write(Bus, Reg, <<(RegState bor Mask):8>>).

unset_bits(Bus, Reg, Mask) ->
    {ok, <<RegState:8>>} = device_read(Bus, Reg),
    device_write(Bus, Reg, <<(RegState bxor Mask):8>>).

device_write(Bus, Register, Payload) ->
    ok = grisp_i2c:write(Bus, ?DEVICE_ADR, Register, Payload).

device_read(Bus, Register) ->
    device_read(Bus, Register, 0).

device_read(Bus, Register, Delay) ->
    [ok] = grisp_i2c:transfer(Bus, [{write, ?DEVICE_ADR, 0, <<Register:8>>}]),
    timer:sleep(Delay),
    [Response] = grisp_i2c:transfer(Bus, [{read, ?DEVICE_ADR, 0, 1}]),
    {ok, Response}.

read_registers(#state{ bus = Bus,  resolution = R }) ->
    LSB = abs(8 - R), % resolution can be 8, 12 or 14
    {ok, <<X_H:8>>} = device_read(Bus, ?XOUT_H),
    {ok, <<Y_H:8>>} = device_read(Bus, ?YOUT_H),
    {ok, <<Z_H:8>>} = device_read(Bus, ?ZOUT_H),
    {ok, <<X_L:LSB,_/bitstring>>} = device_read(Bus, ?XOUT_L),
    {ok, <<Y_L:LSB,_/bitstring>>} = device_read(Bus, ?YOUT_L),
    {ok, <<Z_L:LSB,_/bitstring>>} = device_read(Bus, ?ZOUT_L),
    <<X:(8+LSB)/signed>> = <<X_H:8,X_L:LSB>>,
    <<Y:(8+LSB)/signed>> = <<Y_H:8,Y_L:LSB>>,
    <<Z:(8+LSB)/signed>> = <<Z_H:8,Z_L:LSB>>,
    {X, Y, Z}.

convert(Val, #state{g_range = MaxVal, resolution = Bits}) ->
    Val * MaxVal / math:pow(2, Bits - 1).

set_range_and_resolution(Bus, G, R) ->
    % getting values for
    % RES, GSEL1, GSEL0, EN16G
    % These are interdependent
    Value = range_and_output_selection(G,R),
    set_bits(Bus, ?CTRL_REG1, Value).


% RES   Mode
%   0   low-power -> 8 bits
%   1   high-power -> 12 or 14 bits
%   GSEL1   GSEL0   EN16G   Range
%       0       0       0   ±2g
%       0       1       0   ±4g
%       1       0       0   ±8g
%       1       1       0   ±8g <- only 14 bits precision
%       0       0       1   ±16g \
%       0       1       1   ±16g ->--- these 3 look like equivalents
%       1       0       1   ±16g /
%       1       1       1   ±16g <- only 14 bits precision
range_and_output_selection(2, 8) -> 0;
range_and_output_selection(4, 8) -> ?GSEL0;
range_and_output_selection(8, 8) -> ?GSEL1;
range_and_output_selection(16, 8) -> ?EN16G;
range_and_output_selection(2, 12) -> ?RES;
range_and_output_selection(4, 12) -> ?GSEL0 bor ?RES;
range_and_output_selection(8, 12) -> ?GSEL1 bor ?RES;
range_and_output_selection(16, 12) -> ?EN16G bor ?RES;
range_and_output_selection(8, 14) -> ?GSEL1 bor ?GSEL0 bor ?RES;
range_and_output_selection(16, 14) -> ?GSEL1 bor ?GSEL0 bor ?EN16G bor ?RES;
range_and_output_selection(G, R) -> error(illegal_range_res_combo, {G, R}).


set_output_datarates(Bus, DR_lvl) ->
    ODR = select_ouput_data_rate(DR_lvl),
    device_write(Bus, ?DATA_CTRL_REG, <<ODR:8>>).

select_ouput_data_rate(1) ->    ?OSAA; % 0.781Hz
select_ouput_data_rate(2) ->    ?OSAA bor ?OSAD;
select_ouput_data_rate(3) ->    ?OSAA bor ?OSAC;
select_ouput_data_rate(4) ->    ?OSAA bor ?OSAC bor ?OSAD;
select_ouput_data_rate(5) ->    0;
select_ouput_data_rate(6) ->    ?OSAD;
select_ouput_data_rate(7) ->    ?OSAC;  % 50Hz
select_ouput_data_rate(8) ->    ?OSAC bor ?OSAD;
select_ouput_data_rate(9) ->    ?OSAB;
select_ouput_data_rate(10) ->   ?OSAB bor ?OSAD; % 400Hz
select_ouput_data_rate(11) ->   ?OSAB bor ?OSAC; % 800Hz
select_ouput_data_rate(12) ->   ?OSAB bor ?OSAC bor ?OSAD; % 1600Hz
select_ouput_data_rate(_) ->
    error(invalid_output_rate).

set_wake_up_function(Bus, false) ->
    unset_bits(Bus, ?INT_CTRL_REG1, ?IEN),
    unset_bits(Bus, ?CTRL_REG1, ?WUFE);
set_wake_up_function(Bus, Opts) ->
    Threshold = maps:get(threshold, Opts, 0.5),
    Delay = maps:get(delay, Opts, 1.0),
    Cooldown = maps:get(cooldown, Opts, 1.0),
    Datarate_lvl = maps:get(data_rate, Opts, 8),
    {Rate_bits, Hertz} = select_wuf_data_rate(Datarate_lvl),
    set_bits(Bus, ?CTRL_REG2, Rate_bits),
    <<T_H:8,T_L:8>> = <<(round(Threshold * 256)):16>>,
    set_bits(Bus, ?WAKEUP_THRESHOLD_L, T_L),
    set_bits(Bus, ?WAKEUP_THRESHOLD_H, T_H),
    set_bits(Bus, ?WAKEUP_COUNTER, round(Delay * Hertz)),
    set_bits(Bus, ?NA_COUNTER, round(Cooldown * Hertz)),
    set_bits(Bus, ?INT_CTRL_REG1, ?IEN), % enable int pin
    set_bits(Bus, ?CTRL_REG1, ?WUFE). % enable wake up function


% OWUFA   OWUFB   OWUFC Data Rate
%   0       0       0   0.781   Hz
%   0       0       1   1.563   Hz
%   0       1       0   3.125   Hz
%   0       1       1   6.25    Hz
%   1       0       0   12.5    Hz
%   1       0       1   25      Hz
%   1       1       0   50      Hz
%   1       1       1   100     Hz
select_wuf_data_rate(1) -> {0,                               0.781};
select_wuf_data_rate(2) -> {?OWUFC,                          1.563};
select_wuf_data_rate(3) -> {?OWUFB,                          3.125};
select_wuf_data_rate(4) -> {?OWUFB bor ?OWUFC,               6.25};
select_wuf_data_rate(5) -> {?OWUFA,                          12.5};
select_wuf_data_rate(6) -> {?OWUFA bor ?OWUFC,               25.0};
select_wuf_data_rate(7) -> {?OWUFA bor ?OWUFB,               50.0};
select_wuf_data_rate(8) -> {?OWUFA bor ?OWUFB bor ?OWUFC,    100.0};
select_wuf_data_rate(_) ->
    error(invalid_data_rate).

