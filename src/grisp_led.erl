-module(grisp_led).
-include("grisp_docs.hrl").

-behavior(gen_server).

% API
-export([start_link/0]).
-export([color/2]).
-export([off/1]).
-export([flash/3]).
-export([pattern/2]).
-export([read/1]).

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

-type pattern() :: [{time(), color() | fun(() -> color()) }].
% A list of intervals and colors to show during those intervals.

%--- Macros --------------------------------------------------------------------

-define(is_component(C), C >= 0 andalso C =< 1).

%--- API -----------------------------------------------------------------------

?doc(false).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

?doc("""
Set the color of an LED.

### Examples
```
1> grisp_led:color(1, red).
ok
2> grisp_led:color(2, {0, 1, 0}).
ok
```
""").
-spec color(position(), color()) -> ok.
color(Pos, Color) -> pattern(Pos, [{infinity, Color}]).

?doc("""
Turn off an LED.
""").
?doc(#{equiv => grisp_led:color(Pos, off)}).
-spec off(position()) -> ok.
off(Pos) -> pattern(Pos, [{infinity, off}]).

?doc("""
Flash an LED in an on/off pattern with the specified color.

### Examples
```
1> grisp_led:flash(2, blue, 500).
ok
```
""").
?doc(#{equiv => grisp_led:pattern(Position, [{Time, Color}, {Time, off}])}).
-spec flash(position(), color(), time()) -> ok.
flash(Pos, Color, Interval) ->
    pattern(Pos, [{Interval, Color}, {Interval, off}]).

?doc("""
Animate an LED with a pattern of colors and intervals.

## Examples
```
1> grisp_led:pattern(1, [{300, green}, {500, yellow}, {700, red}, {infinity, off}]).
ok
2> Rainbow = [{300, {R, G, B}} || R <- [0,1], G <- [0,1], B <- [0,1], {R, G, B} =/= {0, 0, 0}].
[{300,{0,0,1}},
 {300,{0,1,0}},
 {300,{0,1,1}},
 {300,{1,0,0}},
 {300,{1,0,1}},
 {300,{1,1,0}},
 {300,{1,1,1}}]
3> grisp_led:pattern(2, Rainbow).
ok
```

The color can also be specified using functions as generators
instead of explicitly stating the color :

```
2> Random = fun() -> {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1} end.
#Fun<erl_eval.20.128620087>
3> grisp_led:pattern(1, [{100, Random}]).
```

As well as by composing lists of intervals and pattern functions :

```
4> Funs = [ fun() -> {X rem 2, rand:uniform(2) - 1 , 1} end || X <- lists:seq(1,10) ].
[#Fun<erl_eval.20.128620087>, ...
5> Intervals = lists:seq(1000,1900,100).
[1000,1100,1200,1300,1400,1500,1600,1700,1800,1900]
6> Result = lists:zip(Intervals, Funs).
[{1000,#Fun<erl_eval.20.128620087>},...
7> grisp_led:pattern(1, Result).
```
""").
-spec pattern(position(), pattern()) -> ok.
pattern(Pos, Pattern) -> gen_server:cast(?MODULE, {pattern, Pos, Pattern}).

read(Pos) -> gen_server:call(?MODULE, {read, Pos}).

%--- Callbacks -----------------------------------------------------------------

?doc(false).
init(undefined) ->
    LED1 = #{
        timer => undefined,
        pins => #{
            r => grisp_gpio:open(led1_r),
            g => grisp_gpio:open(led1_g),
            b => grisp_gpio:open(led1_b)
        }
    },
    LED2 = #{
        timer => undefined,
        pins => #{
            r => grisp_gpio:open(led2_r),
            g => grisp_gpio:open(led2_g),
            b => grisp_gpio:open(led2_b)
        }
    },
    {ok, #{
        1 => LED1#{pattern => [{infinity, color_get(LED1)}]},
        2 => LED2#{pattern => [{infinity, color_get(LED2)}]}
    }}.

?doc(false).
handle_call({read, Pos}, _From, State) ->
    LED = maps:get(Pos, State),
    {reply, color_get(LED), State}.

?doc(false).
handle_cast({pattern, Pos, NewPattern}, State) ->
    NewState = maps:update_with(Pos, fun(LED) ->
        tick_pattern(Pos, LED#{pattern := NewPattern})
    end, State),
    {noreply, NewState}.

?doc(false).
handle_info({tick, Pos}, State) ->
    NewState = maps:update_with(Pos, fun(LED) ->
        tick_pattern(Pos, LED)
    end, State),
    {noreply, NewState}.

?doc(false).
code_change(_OldVsn, State, _Extra) -> {ok, State}.

?doc(false).
terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------

tick_pattern(_Pos, LED = #{pattern := [{infinity, Color} = Step|_], timer := Timer, pins := Pins}) ->
    cancel_timer(Timer),
    color_set(Color, Pins),
    LED#{
        pattern := [Step],
        timer := undefined
    };
tick_pattern(Pos, LED = #{pattern := [{Time, Color} = Step|Rest], timer := Timer, pins := Pins}) ->
    cancel_timer(Timer),
    color_set(Color, Pins),
    LED#{
        pattern := Rest ++ [Step],
        timer := erlang:send_after(Time, self(), {tick, Pos})
    }.

cancel_timer(undefined) -> ok;
cancel_timer(Timer) -> erlang:cancel_timer(Timer).

color_set(Color, #{r := RP, g := GP, b := BP}) ->
    {R, G, B} = translate(Color),
    grisp_gpio:set(RP, R),
    grisp_gpio:set(GP, G),
    grisp_gpio:set(BP, B).

color_get(#{pins := #{r := RP, g := GP, b := BP}}) ->
    {grisp_gpio:get(RP), grisp_gpio:get(GP), grisp_gpio:get(BP)}.

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
