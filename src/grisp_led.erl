-module(grisp_led).

-behavior(gen_server).

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

%--- Types ---------------------------------------------------------------------

-type position() :: 1 | 2.
% The position of the LED on the GRiSP board, either `1' or `2'.

-type color() :: color_name() | color_value().
% A color as either a shorthand name or a specific RGB value.

-type color_name() ::
    off |
    black |
    blue |
    green |
    aqua |
    red |
    magenta |
    yellow |
    white.
% A color name. `off' and `black' are shorthands for turning off the LED.

-type color_value() :: {0 | 1, 0 | 1, 0 | 1}.
% A color value, specifying the individual R, G and B components as 1's or 0's
% where 1 means on and 0 means off.

-type time() :: pos_integer() | infinity.
% A time interval for a color in milliseconds. Must be 1 or above, or
% alternatively `infinity'.

-type pattern() :: [{time(), color()}].
% A list of intervals and colors to show during those intervals.

%--- Records -------------------------------------------------------------------

-record(state, {driver, leds}).

%--- Macros --------------------------------------------------------------------

-define(is_component(C), C >= 0 andalso C =< 1).

%--- API -----------------------------------------------------------------------

% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

% @doc Set the color of an LED.
%
% <h5>Examples</h5>
% ```
% 1> grisp_led:color(1, red)
% ok
% 2> grisp_led:color(2, {0, 1, 0})
% ok
% '''
-spec color(position(), color()) -> ok.
color(Pos, Color) -> pattern(Pos, [{infinity, Color}]).

% @doc Turn of an LED.
% @equiv grisp_led:color(Pos, off)
-spec off(position()) -> ok.
off(Pos) -> pattern(Pos, [{infinity, off}]).

% @doc Flash an LED in an on/off pattern with the specified color.
%
% <h5>Examples</h5>
% ```
% 1> grisp_led:flash(2, blue, 500).
% ok
% '''
%
% @equiv grisp_led:pattern(Position, [{Time, Color}, {Time, off}])
-spec flash(position(), color(), time()) -> ok.
flash(Pos, Color, Interval) ->
    pattern(Pos, [{Interval, Color}, {Interval, off}]).

% @doc Animate an LED with a pattern of colors and intervals.
%
% <h5>Examples</h5>
% ```
% 1> grisp_led:flash(1, [{300, green}, {500, yellow}, {700, red}, {infinity, off}]).
% ok
% 2> Rainbow = [{300, {R, G, B}} || R <- [0,1], G <- [0,1], B <- [0,1], {R, G, B} =/= {0, 0, 0}].
% [{300,{0,0,1}},
%  {300,{0,1,0}},
%  {300,{0,1,1}},
%  {300,{1,0,0}},
%  {300,{1,0,1}},
%  {300,{1,1,0}},
%  {300,{1,1,1}}]
% 3> grisp_led:flash(2, Rainbow).
% ok
% '''
-spec pattern(position(), pattern()) -> ok.
pattern(Pos, Pattern) -> gen_server:cast(?MODULE, {pattern, Pos, Pattern}).

%--- Callbacks -----------------------------------------------------------------

% @private
init(undefined) ->
    {ok, #state{leds = [
        {1, {[{infinity, off}], undefined}},
        {2, {[{infinity, off}], undefined}}
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

to_rgb(black)   -> to_rgb(off);
to_rgb(off)     -> {0, 0, 0};
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
