-module(robot_trigger).

-behavior(gen_event).

% API
-export([register/0]).

% Callbacks
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- API -----------------------------------------------------------------------

register() ->
    message("registering..."),
    gen_event:add_handler(grisp_gpio_events, ?MODULE, undefined).

%--- Callbacks -----------------------------------------------------------------

init(_Args) ->
    grisp_led:flash(2, blue, 100),
    message("registered"),
    {ok, undefined}.

handle_event({jumper_1, true}, State) ->
    message("jumper triggered"),
    spawn(fun() ->
        run_robot()
    end),
    {ok, State};
handle_event(Event, State) ->
    io:format("ROBOT: ignoring ~p~n", [Event]),
    {ok, State}.

handle_call(Request, _State) -> error({unknown_request, Request}).

handle_info(_Info, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> State.

terminate(_Arg, _State) ->
    message("terminated"),
    undefined.

%--- Internal ------------------------------------------------------------------

run_robot() ->
    message("starting..."),
    grisp_led:flash(2, green, 100),
    timer:sleep(2000),
    grisp_led:color(2, green),
    robot:start(),
    command(forward, 50),
    command(backward, 50),
    timer:sleep(2000),
    command(left, 50),
    command(right, 50),
    command(left, 50),
    timer:sleep(1000),
    command(forward, 200),
    command(right, 55),
    command(forward, 200),
    command(right, 55),
    command(forward, 200),
    command(right, 55),
    command(forward, 200),
    command(left, 200),
    message("done").

command(Command, Time) ->
    message(atom_to_list(Command)),
    robot:Command(Time),
    timer:sleep(100).

message(Message) -> io:format("ROBOT: ~s~n", [Message]).
