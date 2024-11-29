-module(routing_table).

-behaviour(gen_server).

%%% API
-export([
    start_link/1,  
    stop/0,
    addRoute/2,
    deleteRoute/1,
    getRoute/1,
    updateRoute/2,
    resetRouting_table/0
]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

%%% API functions

start_link(RoutingTable) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, RoutingTable, []).

stop() ->
    gen_server:stop(?MODULE).

addRoute(DestAddr, NextHAddr) ->
    gen_server:call(?MODULE, {add_route, DestAddr, NextHAddr}).

deleteRoute(DestAddr) ->
    gen_server:call(?MODULE, {delete_route, DestAddr}).

getRoute(DestAddr) ->
    gen_server:call(?MODULE, {get_route, DestAddr}).

updateRoute(DestAddr, NextHAddr) ->
    gen_server:call(?MODULE, {update_route, DestAddr, NextHAddr}).

resetRouting_table() ->
    gen_server:call(?MODULE, reset).

%%% gen_server callbacks
init(RoutingTable) ->
    {ok, RoutingTable}.


handle_call({add_route, DestAddr, NextHAddr}, _From, RoutingTable) ->
    NewTable = maps:put(DestAddr, NextHAddr, RoutingTable),
    {reply, ok, NewTable};

handle_call({delete_route, DestAddr}, _From, RoutingTable) ->
    NewTable = maps:remove(DestAddr, RoutingTable),
    {reply, ok, NewTable};

handle_call({get_route, DestAddr}, _From, RoutingTable) ->
    NextHAddr = maps:get(DestAddr, RoutingTable, undefined),
    {reply, NextHAddr, RoutingTable};

handle_call({update_route, DestAddr, NextHAddr}, _From, RoutingTable) ->
    NewTable = maps:put(DestAddr, NextHAddr, RoutingTable),
    {reply, ok, NewTable};

handle_call(reset, _From, _MapState) ->
    {reply, ok, #{}}.

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
