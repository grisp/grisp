-module(robot).

% API
-export([start/0]).
-export([forward/2]).
-export([backward/2]).
-export([left/2]).
-export([right/2]).

%--- API -----------------------------------------------------------------------

start() ->
    State = {pmod_hb5:open({gpio1,top}), pmod_hb5:open({gpio1,bottom})},
    commands(State, [{left, config}, {right, config}], 0).

forward(State, Time) ->
    commands(State, [{left, forward}, {right, forward}], Time).

backward(State, Time) ->
    commands(State, [{left, backward}, {right, backward}], Time).

left(State, Time) ->
    commands(State, [{left, backward}, {right, forward}], Time).

right(State, Time) ->
    commands(State, [{left, forward}, {right, backward}], Time).

%--- Internal ------------------------------------------------------------------

commands(State, Commands, Time) ->
    [command(State, Side, Command) || {Side, Command} <- Commands],
    timer:sleep(Time),
    [command(State, Side, stop) || {Side, _} <- Commands].

command({_Left, Right}, right, Command) -> pmod_hb5:Command(Right);
% Left motor is mounted backwards, reverse directions:
command({Left, _Right}, left, forward) -> pmod_hb5:backward(Left);
command({Left, _Right}, left, backward) -> pmod_hb5:forward(Left);
command({Left, _Right}, left, Command) -> pmod_hb5:Command(Left).
