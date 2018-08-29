% @private
-module(grisp_gpio_jumper_printer).

-behavior(gen_event).

% API
-export([start/0]).
-export([stop/0]).

% Callbacks
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- API Functions -------------------------------------------------------------

start() -> grisp_gpio:add_handler(?MODULE, []).

stop() -> grisp_gpio:delete_handler(?MODULE).

%--- Callbacks -----------------------------------------------------------------

init(_Args) ->
    grisp_gpio:configure_slot(jumper, {
        {input, true, true, 10, 10, [default]},
        {input, true, true, 10, 10, [default]},
        {input, true, true, 10, 10, [default]},
        {input, true, true, 10, 10, [default]},
        {input, true, true, 10, 10, [default]}
    }),
    {ok, undefined}.

handle_event({{jumper, Idx}, Value}, State) ->
    io:format(user, "JUMPER ~b : ~w~n", [Idx, Value]),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(Request, _State) -> error({unknown_request, Request}).

handle_info(_Info, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> State.

terminate(_Arg, _State) -> undefined.
