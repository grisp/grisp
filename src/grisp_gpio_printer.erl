% @private
-module(grisp_gpio_printer).

-behavior(gen_event).

% Callbacks
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- Callbacks -----------------------------------------------------------------

init(_Args) -> {ok, undefined}.

handle_event(Event, State) ->
    io:format("~p~n", [Event]),
    {ok, State}.

handle_call(Request, _State) -> error({unknown_request, Request}).

handle_info(_Info, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> State.

terminate(_Arg, _State) -> undefined.
