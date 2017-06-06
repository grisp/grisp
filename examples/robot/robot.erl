-module(robot).

% API
-export([start/0]).
-export([forward/2]).
-export([backward/2]).
-export([turn/3]).

%--- API -----------------------------------------------------------------------

start() ->
    {ok, _} = application:ensure_all_started(grisp),
    P = grisp_gpio_drv:open(),
    commands(P, [{left, config}, {right, config}], 0),
    P.

forward(P, Time) -> commands(P, [{left, forward}, {right, forward}], Time).

backward(P, Time) -> commands(P, [{left, backward}, {right, backward}], Time).

turn(P, Time, left)  -> commands(P, [{left, backward}, {right, forward}], Time);
turn(P, Time, right) -> commands(P, [{left, forward}, {right, backward}], Time).

%--- Internal ------------------------------------------------------------------

commands(P, Commands, Time) ->
    [command(P, Side, Command) || {Side, Command} <- Commands],
    timer:sleep(Time),
    [command(P, Side, stop) || {Side, _} <- Commands].

command(P, right, Command) -> grisp_hb5:Command(P, gpio2);
% Left motor is mounted backwards, revers directions:
command(P, left, forward) -> grisp_hb5:backward(P, gpio1);
command(P, left, backward) -> grisp_hb5:forward(P, gpio1);
command(P, left, Command) -> grisp_hb5:Command(P, gpio1).
