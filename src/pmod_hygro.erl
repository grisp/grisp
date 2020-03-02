% @doc API for the
% <a href="https://reference.digilentinc.com/reference/pmod/pmodhygro/start">
% PmodHYGRO
% </a>.
%
% Start the server with
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

-define(DEVICE_ADR,    16#40).
-define(TEMP_REGISTER, 16#00).
-define(DELAY_TIME, 15).
%
%--- Records -------------------------------------------------------------------
%
-record(state, {}).

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
init(Slot) ->
    grisp_devices:register(Slot, ?MODULE),
    {ok, #state{}}.

% @private
handle_call(temp, _From, State) ->
    {ok, <<T:14/unsigned-big, _:2>>} = device_request(2),
    Temp = evaluate_temp(T),
    {reply, [{temp, Temp}], State};
handle_call(humid, _From, State) ->
    {ok, <<_:14, _:2, H:14/unsigned-big, _:2>>} = device_request(4),
    Humid = evaluate_humid(H),
    {reply, [{humid, Humid}], State};
handle_call(measurements, _From, State) ->
    {ok, <<T:14/unsigned-big, _:2, H:14/unsigned-big, _:2>>} = device_request(4),
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

device_request(BytesToRead) ->
    Response = grisp_i2c:msgs([?DEVICE_ADR, {write, <<?TEMP_REGISTER>>},
                               {sleep, ?DELAY_TIME},
                               {read, BytesToRead}]),
    {ok, Response}.

evaluate_temp(T) ->
    (T / 16384) * 165 - 40.

evaluate_humid(H) ->
    (H / 16384) * 100.
