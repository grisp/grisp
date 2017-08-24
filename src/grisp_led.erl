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

-record(state, {driver, leds}).

%--- Macros --------------------------------------------------------------------

-define(is_component(C), C >= 0 andalso C =< 1).

%--- API -----------------------------------------------------------------------

% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

color(Pos, Color) -> pattern(Pos, [{infinity, Color}]).

off(Pos) -> pattern(Pos, [{infinity, black}]).

flash(Pos, Color, Interval) ->
    pattern(Pos, [{Interval, Color}, {Interval, black}]).

pattern(Pos, Pattern) -> gen_server:cast(?MODULE, {pattern, Pos, Pattern}).

%--- Callbacks -----------------------------------------------------------------

% @private
init(undefined) ->
    {ok, #state{leds = [
        {1, {[{infinity, black}], undefined}},
        {2, {[{infinity, black}], undefined}}
    ]}}.

% @private
handle_call(Request, From, _State) -> error({unknown_call, Request, From}).

% @private
handle_cast({pattern, Pos, NewPattern}, State) ->
    NewState = update_led(Pos, State, fun({_OldPattern, Timer}) ->
        tick_pattern(Pos, {NewPattern, Timer})
    end),
    {noreply, NewState}.

% @private
handle_info({tick, Pos}, State) ->
    NewState = update_led(Pos, State, fun(Led) ->
        tick_pattern(Pos, Led)
    end),
    {noreply, NewState}.

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------

update_led(Pos, #state{leds = Leds} = State, Fun) ->
    State#state{leds = mod(Pos, Fun, Leds)}.

mod(Pos, Fun, [{Pos, Led}|Rest]) -> [{Pos, Fun(Led)}|Rest];
mod(Pos, Fun, [Led|Rest])        -> [Led|mod(Pos, Fun, Rest)];
mod(Pos, _Fun, [])               -> error({led_not_found, Pos}).

tick_pattern(Pos, {[{infinity, Color} = Pattern|_Rest], Timer}) ->
    cancel_timer(Timer),
    write_color(Pos, Color),
    {[Pattern], undefined};
tick_pattern(Pos, {[{Time, Color} = Step|Rest], Timer}) when Time >= 1 ->
    cancel_timer(Timer),
    write_color(Pos, Color),
    NewTimer = erlang:send_after(Time, self(), {tick, Pos}),
    {Rest ++ [Step], NewTimer}.

cancel_timer(undefined) -> ok;
cancel_timer(Timer)     -> erlang:cancel_timer(Timer).

write_color(Pos, Color) ->
    {R, G, B} = translate(Color),
    write_component(Pos, red, action(R)),
    write_component(Pos, green, action(G)),
    write_component(Pos, blue, action(B)).

action(0) -> clear;
action(1) -> set.

write_component(1, red, Action)   -> grisp_gpio:Action(led1_r);
write_component(1, green, Action) -> grisp_gpio:Action(led1_g);
write_component(1, blue, Action)  -> grisp_gpio:Action(led1_b);
write_component(2, red, Action)   -> grisp_gpio:Action(led2_r);
write_component(2, green, Action) -> grisp_gpio:Action(led2_g);
write_component(2, blue, Action)  -> grisp_gpio:Action(led2_b).

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
