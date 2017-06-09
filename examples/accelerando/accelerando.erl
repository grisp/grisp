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
    {X, Y, Z} = pmod_acl2:g(),
    Color = {color(X), color(Y), color(Z)},
    grisp_led:color(1, Color),
    grisp_led:color(2, Color),
    timer:sleep(10),
    process().

color(Val) -> trunc((abs(Val) / 2.0) + 0.8) rem 2.
