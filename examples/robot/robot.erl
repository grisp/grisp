-module(robot).

% API
-export([start/0]).
-export([forward/1]).
-export([backward/1]).
-export([left/1]).
-export([right/1]).

%--- API -----------------------------------------------------------------------

start() -> commands([{left, config}, {right, config}], 0).

forward(Time) -> commands([{left, forward}, {right, forward}], Time).

backward(Time) -> commands([{left, backward}, {right, backward}], Time).

left(Time) -> commands([{left, backward}, {right, forward}], Time).

right(Time) -> commands([{left, forward}, {right, backward}], Time).

%--- Internal ------------------------------------------------------------------

commands(Commands, Time) ->
    [command(Side, Command) || {Side, Command} <- Commands],
    timer:sleep(Time),
    [command(Side, stop) || {Side, _} <- Commands].

command(right, Command) -> pmod_hb5:Command(gpio2);
% Left motor is mounted backwards, revers directions:
command(left, forward) -> pmod_hb5:backward(gpio1);
command(left, backward) -> pmod_hb5:forward(gpio1);
command(left, Command) -> pmod_hb5:Command(gpio1).
