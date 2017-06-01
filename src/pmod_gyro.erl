-module(pmod_gyro).

-behaviour(gen_server).

% API
-export([start_link/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- API -----------------------------------------------------------------------

start_link(Port) ->
    gen_server:start_link(?MODULE, Port, []).

%--- Callbacks -----------------------------------------------------------------

init(Port) -> {ok, Port}.

handle_call(Request, From, _State) -> error({unknown_request, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info(Info, _State) -> error({unknown_info, Info}).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
