% @private
-module(grisp_app).

-behavior(application).

% API
-export([start/2]).
-export([stop/1]).

%--- API -----------------------------------------------------------------------

start(_Type, _Args) ->
    {ok, Pid} = grisp_sup:start_link(),
    grisp_devices:setup(),
    initialize_leds(),
    {ok, Pid}.

stop(_State) -> ok.

%--- Internal ------------------------------------------------------------------

initialize_leds() ->
    LEDs = application:get_env(grisp, leds, #{}),
    maps:map(fun(LED, Params) ->
        [Mode|Args] = tuple_to_list(Params),
        apply(grisp_led, Mode, [LED|Args])
    end, LEDs).
