-module(grisp_pins).

-export([index/1]).
-export([map_type/1]).
-export([attr/1]).
-export([map_attr/1]).
-export([bool/1]).

index(gpio1_1)    -> 0;
index(gpio1_2)    -> 1;
index(gpio1_3)    -> 2;
index(gpio1_4)    -> 3;
index(gpio2_1)    -> 4;
index(gpio2_2)    -> 5;
index(gpio2_3)    -> 6;
index(gpio2_4)    -> 7;
index(led1_r)     -> 8;
index(led1_g)     -> 9;
index(led1_b)     -> 10;
index(led2_r)     -> 11;
index(led2_g)     -> 12;
index(led2_b)     -> 13;
index(jumper_1)   -> 14;
index(jumper_2)   -> 15;
index(jumper_3)   -> 16;
index(jumper_4)   -> 17;
index(jumper_5)   -> 18;
index(spi1_pin9)  -> 19;
index(spi1_pin10) -> 20;
index(ss1)        -> 21;
index(ss2)        -> 22;
index(Any) when is_integer(Any) ->
    Any.

%% See: rtems/c/src/lib/libbsp/arm/atsam/libraries/libchip/include/pio.h
map_type(periph_a) -> 0;
map_type(periph_b) -> 1;
map_type(periph_c) -> 2;
map_type(periph_d) -> 3;
map_type(input)    -> 4;
map_type(output_0) -> 5; % default value 0
map_type(output_1) -> 6. % default value 1

attr(default) -> 0;
attr(pullup) -> 2#1 bsl 0;
attr(deglitch) -> 2#1 bsl 1;
attr(opendrain) -> 2#1 bsl 2;
attr(debounce) -> 2#1 bsl 3;
attr(it_aime) -> 2#1 bsl 4;
attr(it_re_or_hl) -> 2#1 bsl 5;
attr(it_edge) -> 2#1 bsl 6;
attr(it_low_level) -> attr(it_aime);
attr(it_high_level) -> attr(it_re_or_hl) bor attr(it_aime);
attr(it_fall_edge) -> attr(it_edge) bor attr(it_aime);
attr(it_rise_edge) -> attr(it_re_or_hl) bor attr(it_edge) bor attr(it_aime).

map_attr(Attributes) when is_list(Attributes) -> map_attr(Attributes, 0);
map_attr(Attributes) -> attr(Attributes).
map_attr([H|T], Buf) -> map_attr(T, attr(H) bor Buf);
map_attr([], Buf) -> Buf.

bool(<<0>>) -> false;
bool(<<1>>) -> true;
bool(Val)   -> Val.
