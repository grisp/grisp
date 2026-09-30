% @doc Demonstration of MTDS touch events.
%
% To use:
% > grisp:add_device(spi2, pmod_mtds).
% > mtds_sketch:start_link().

-module(mtds_sketch).
-export([start_link/0]).
-export([
    init/1, code_change/3, terminate/2,
    handle_call/3, handle_cast/2, handle_info/2
]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    self() ! poll_touch,
    %% NOTE: This locks the display surface from being written to by other apps.
    pmod_mtds:surface_display().

handle_call(_Msg, _From, _Handle) ->
    error(no_clause).

handle_cast(_Msg, _Handle) ->
    error(no_clause).

handle_info(poll_touch, Handle) ->
    lists:foreach(fun(Event) -> draw_touch(Event, Handle) end,
                  pmod_mtds:touch_events()),
    erlang:send_after(16, self(), poll_touch),
    {noreply, Handle}.

%% When a finger first appears, move the cursor to that position.
draw_touch({touch, _Window, {down, 0}, Position, _Speed, _Weight}, Handle) ->
    pmod_mtds:move_to(Handle, Position),
    ok;
%% Other events correspond to an existing finger moving around.  Draw a line.
draw_touch({touch, _Window, {_, 0}, Position, _Speed, _Weight}, Handle) ->
    pmod_mtds:line_to(Handle, Position),
    ok;
draw_touch(_OtherFinger, _Handle) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, Handle) ->
    pmod_mtds:surface_release(Handle).
