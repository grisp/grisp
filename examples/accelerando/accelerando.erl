-module(accelerando).

% API
-export([run/0]).

-compile(export_all).

%--- API -----------------------------------------------------------------------

run() ->
    application:start(grisp),
    spawn_link(fun() -> process() end).

%--- Internal ------------------------------------------------------------------

process() ->
    {X, Y, Z} = G = pmod_acl2:g(),
    Color = {color(X), color(Y), color(Z)},
    io:format("~p -> ~p~n", [G, Color]),
    grisp_led:color(1, Color),
    timer:sleep(100),
    process().

color(Val) -> trunc((abs(Val) / 2.0) + 0.8) rem 2.
