-module(ecp_tracer).
-export([start_link/0]).
-export([
    init/1, code_change/3, terminate/2,
    handle_call/3, handle_cast/2, handle_info/2
]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    pmod_mtds:register(default),
    {ok, Handle} = pmod_mtds:display_surface(),
    {ok, Handle}.

handle_call(_Msg, _From, _Handle) ->
    error(no_clause).

handle_cast(_Msg, _Handle) ->
    error(no_clause).

handle_info({touch, default, {down, 0}, Position, _Speed, _Weight}, Handle) ->
    pmod_mtds:move_to(Handle, Position),
    {noreply, Handle};
handle_info({touch, default, {_, 0}, Position, _Speed, _Weight}, Handle) ->
    pmod_mtds:line_to(Handle, Position),
    {noreply, Handle}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
