-module(grisp_i2c).

-behavior(gen_server).

% API
-export([start_link/1]).
-export([do_stuff/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- Records -------------------------------------------------------------------

-record(state, {driver}).

%--- API -----------------------------------------------------------------------

start_link(DriverMod) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, DriverMod, []).

% FIXME: Placeholder API
do_stuff(Things) ->
    gen_server:call(?MODULE, {do_stuff, Things}).

%--- Callbacks -----------------------------------------------------------------

init(DriverMod) ->
    Ref = DriverMod:open(),
    {ok, #state{driver = {DriverMod, Ref}}}.

handle_call({do_stuff, Things}, _From, State) ->
    {DriverMod, Ref} = State#state.driver,
    Resp = DriverMod:command(Ref, Things),
    {reply, Resp, State}.

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info(Info, _State) -> error({unknown_info, Info}).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
