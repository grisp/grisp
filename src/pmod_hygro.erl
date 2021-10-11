% @doc API for the
% <a href="https://reference.digilentinc.com/reference/pmod/pmodhygro/start">
% PmodHYGRO
% </a>.
%
% Start the driver with
% ```
% 1> grisp:add_device(i2c, pmod_hygro).
% '''
% @end
-module(pmod_hygro).
-behaviour(gen_server).

% API
-export([start_link/2]).
-export([temp/0]).
-export([humid/0]).
-export([measurements/0]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(DEVICE_ADR, 16#40).
-define(REG_TEMPERATURE, 16#00).
-define(REG_MANUFACTURER_ID, 16#FE).
-define(REG_DEVICE_ID, 16#FF).

-define(DELAY_TIME, 15).

-define(MANUFACTURER_ID, 16#5449).
-define(DEVICE_ID, 16#1050).

%--- Records -------------------------------------------------------------------
%
-record(state, {bus}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot, _Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Slot, []).

% @doc Measure the temperature in °C.
%
% === Example ===
% ```
% 2> pmod_hygro:temp().
% [{temp,24.6746826171875}]
% '''
-spec temp() -> [{temp, float()}].
temp() ->
    gen_server:call(?MODULE, temp).

% @doc Measure the humidity in %.
%
% === Example ===
% ```
% 2> pmod_hygro:humid().
% [{humid,50.225830078125}]
% '''
-spec humid() -> [{humid, float()}].
humid() ->
    gen_server:call(?MODULE, humid).

% @doc Measure the temperature and humidity.
%
% === Example ===
% ```
% 2> pmod_hygro:measurements().
% [{temp,24.52362060546875},{humid,50.823974609375}]
% '''
-spec measurements() -> [{temp, float()}|{humid, float()}].
measurements() ->
    gen_server:call(?MODULE, measurements).

%--- Callbacks -----------------------------------------------------------------

% @private
init(i2c = Slot) ->
    Bus = grisp_ni2c:open(i2c1),
    verify_device(Bus),
    grisp_devices:register(Slot, ?MODULE),
    {ok, #state{bus = Bus}}.

% @private
handle_call(temp, _From, #state{bus = Bus} = State) ->
    {ok, <<T:14/unsigned-big, _:2>>} = device_request(Bus, ?REG_TEMPERATURE, ?DELAY_TIME, 2),
    Temp = evaluate_temp(T),
    {reply, [{temp, Temp}], State};
handle_call(humid, _From, #state{bus = Bus} = State) ->
    {ok, <<_:14, _:2, H:14/unsigned-big, _:2>>} = device_request(Bus, ?REG_TEMPERATURE, ?DELAY_TIME, 4),
    Humid = evaluate_humid(H),
    {reply, [{humid, Humid}], State};
handle_call(measurements, _From, #state{bus = Bus} = State) ->
    {ok, <<T:14/unsigned-big, _:2, H:14/unsigned-big, _:2>>} = device_request(Bus, ?REG_TEMPERATURE, ?DELAY_TIME, 4),
    Temp = evaluate_temp(T),
    Humid = evaluate_humid(H),
    {reply, [{temp, Temp}, {humid, Humid}], State}.

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
    {ok, <<ManufacturerID:16>>} = device_request(Bus, ?REG_MANUFACTURER_ID, 0, 2),
    {ok, <<DeviceID:16>>} = device_request(Bus, ?REG_DEVICE_ID, 0, 2),
    case {ManufacturerID, DeviceID} of
        {?MANUFACTURER_ID, ?DEVICE_ID} -> ok;
        Other -> error({device_mismatch, Other})
    end.

device_request(Bus, Register, Delay, BytesToRead) ->
    [ok] = grisp_ni2c:transfer(Bus, [{write, ?DEVICE_ADR, 0, <<Register:8>>}]),
    timer:sleep(Delay),
    [Response] = grisp_ni2c:transfer(Bus, [{read, ?DEVICE_ADR, 0, BytesToRead}]),
    {ok, Response}.

evaluate_temp(T) ->
    (T / 16384) * 165 - 40.

evaluate_humid(H) ->
    (H / 16384) * 100.
