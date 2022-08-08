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
-define(XOUT_L, 16#06).
-define(XOUT_H, 16#07).
-define(YOUT_L, 16#08).
-define(YOUT_H, 16#09).
-define(ZOUT_L, 16#0A).
-define(ZOUT_H, 16#0B).

-define(DEVICE_ID, 16#35).

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
-spec reset(Opts :: map()) -> ok.
reset(Opts) ->
    gen_server:call(?MODULE, {reset, Opts}).

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
    {reply, ok, State}.

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
    State = apply_opts(Opts, #state{bus = Bus}),
    set_bits(Bus, ?CTRL_REG1, ?PC1), % start operations
    State.

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

apply_opts(Opts, #state{bus = B} = S) ->
    G = maps:get(g_range, Opts, 2),
    R = maps:get(resolution, Opts, 8),
    set_range_and_resolution(B, G, R),
    S#state{g_range = G, resolution = R}.

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


