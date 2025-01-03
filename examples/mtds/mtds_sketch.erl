-module(mtds_sketch).
-moduledoc """
Demonstration of MTDS touch events.

To use:
> pmod_mtds:start_link(spi2).
> mtds_sketch:start_link().
""".
-export([start_link/0]).
-export([
    init/1, code_change/3, terminate/2,
    handle_call/3, handle_cast/2, handle_info/2
]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    pmod_mtds:register(),
    %% NOTE: This locks the display surface from being written to by other apps.
    pmod_mtds:surface_display().

handle_call(_Msg, _From, _Handle) ->
    error(no_clause).

handle_cast(_Msg, _Handle) ->
    error(no_clause).

%% When a finger first appears, move the cursor to that position.
handle_info({touch, _Window, {down, 0}, Position, _Speed, _Weight}, Handle) ->
    pmod_mtds:move_to(Handle, Position),
    {noreply, Handle};
%% Other events correspond to an existing finger moving around.  Draw a line.
handle_info({touch, _Window, {_, 0}, Position, _Speed, _Weight}, Handle) ->
    pmod_mtds:line_to(Handle, Position),
    {noreply, Handle}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, Handle) ->
    pmod_mtds:surface_release(Handle).
