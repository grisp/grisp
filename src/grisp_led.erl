-module(grisp_led).

-behaviour(gen_server).

% API
-export([start_link/0]).
-export([color/2]).
-export([off/1]).
-export([flash/3]).
-export([pattern/2]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- Records -------------------------------------------------------------------

-record(state, {port, leds}).

%--- Macros --------------------------------------------------------------------

-define(is_component(C), C >= 0 andalso C =< 1).

%--- API -----------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

color(Pos, Color) -> pattern(Pos, [{infinity, Color}]).

off(Pos) -> pattern(Pos, [{infinity, black}]).

flash(Pos, Color, Interval) ->
    pattern(Pos, [{Interval, Color}, {Interval, black}]).

pattern(Pos, Pattern) -> gen_server:cast(?MODULE, {pattern, Pos, Pattern}).

%--- Callbacks -----------------------------------------------------------------

init(undefined) ->
    % Port = open_port({spawn, "grisp_led_drv"}, [binary]),
    Port = undefined,
    {ok, #state{port = Port, leds = [
        {1, {[{infinity, black}], undefined}},
        {2, {[{infinity, black}], undefined}}
    ]}}.

handle_call(Request, From, _State) -> error({unknown_call, Request, From}).

handle_cast({pattern, Pos, NewPattern}, State) ->
    NewState = update_led(Pos, State, fun(Port, {_OldPattern, Timer}) ->
        tick_pattern(Port, Pos, {NewPattern, Timer})
    end),
    {noreply, NewState}.

handle_info({tick, Pos}, State) ->
    NewState = update_led(Pos, State, fun(Port, Led) ->
        tick_pattern(Port, Pos, Led)
    end),
    {noreply, NewState}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------

update_led(Pos, #state{port = Port, leds = Leds} = State, Fun) ->
    State#state{leds = mod(Pos, Port, Fun, Leds)}.

mod(Pos, Port, Fun, [{Pos, Led}|Rest]) -> [{Pos, Fun(Port, Led)}|Rest];
mod(Pos, Port, Fun, [Led|Rest])        -> [Led|mod(Pos, Port, Fun, Rest)];
mod(Pos, _Port, _Fun, [])              -> error({led_not_found, Pos}).

tick_pattern(Port, Pos, {[{infinity, Color} = Pattern|_Rest], Timer}) ->
    cancel_timer(Timer),
    write_color(Port, Pos, Color),
    {[Pattern], undefined};
tick_pattern(Port, Pos, {[{Time, Color} = Step|Rest], Timer}) ->
    cancel_timer(Timer),
    write_color(Port, Pos, Color),
    NewTimer = erlang:send_after(Time, self(), {tick, Pos}),
    {Rest ++ [Step], NewTimer}.

cancel_timer(undefined) -> ok;
cancel_timer(Timer)     -> erlang:cancel_timer(Timer).

write_color(Port, Pos, Color) ->
    {R, G, B} = translate(Color),
    % Port ! {self(), {command, <<Pos:8, R:8, G:8, B:8>>}}.
    io:format("WRITE COLOR ~p ~p {~p, ~p, ~p}~n", [Port, Pos, R, G, B]).

translate(Fun) when is_function(Fun) -> to_rgb(Fun());
translate(Value)                     -> to_rgb(Value).

to_rgb(black)   -> {0, 0, 0};
to_rgb(blue)    -> {0, 0, 1};
to_rgb(green)   -> {0, 1, 0};
to_rgb(aqua)    -> {0, 1, 1};
to_rgb(red)     -> {1, 0, 0};
to_rgb(magenta) -> {1, 0, 1};
to_rgb(yellow)  -> {1, 1, 0};
to_rgb(white)   -> {1, 1, 1};
to_rgb({R, G, B} = Color)
  when ?is_component(R) andalso ?is_component(G) andalso ?is_component(B) ->
    Color;
to_rgb(Color) ->
    error({invalid_color, Color}).
