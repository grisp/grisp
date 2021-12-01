-module(grisp_ngpio).

-include("grisp_nif.hrl").

% API
-export([open/1]).
-export([open/2]).
-export([set/2]).
-export([get/1]).

% Callbacks
-export([on_load/0]).
-on_load(on_load/0).

% Macros
-define(DEFAULT_OPTS, #{mode => {output, 0}}).

% Attributes
-compile({no_auto_import, [get/1]}).

%--- API -----------------------------------------------------------------------

open(Pin) -> open(Pin, #{}).

open(Pin, UserOpts) ->
    #{mode := Mode} = maps:merge(?DEFAULT_OPTS, UserOpts),
    gpio_open_nif(pin(Pin), Mode).

set(Pin, Value) when is_integer(Value) -> gpio_set_nif(Pin, Value).

get(Pin) -> gpio_get_nif(Pin).

%--- Callbacks -----------------------------------------------------------------

on_load() -> ok = erlang:load_nif(atom_to_list(?MODULE), 0).

%--- Internal ------------------------------------------------------------------

gpio_open_nif(_Attributes, _Mode) -> ?NIF_STUB.

gpio_set_nif(_Pin, _Value) -> ?NIF_STUB.

gpio_get_nif(_Pin) -> ?NIF_STUB.

pin(Pin) -> pin(grisp_hw:platform(), Pin).

% erlfmt-ignore
pin(grisp_base, gpio1_1)    -> #{index => 0};
pin(grisp_base, gpio1_2)    -> #{index => 1};
pin(grisp_base, gpio1_3)    -> #{index => 2};
pin(grisp_base, gpio1_4)    -> #{index => 3};
pin(grisp_base, gpio2_1)    -> #{index => 4};
pin(grisp_base, gpio2_2)    -> #{index => 5};
pin(grisp_base, gpio2_3)    -> #{index => 6};
pin(grisp_base, gpio2_4)    -> #{index => 7};
pin(grisp_base, led1_r)     -> #{index => 8};
pin(grisp_base, led1_g)     -> #{index => 9};
pin(grisp_base, led1_b)     -> #{index => 10};
pin(grisp_base, led2_r)     -> #{index => 11};
pin(grisp_base, led2_g)     -> #{index => 12};
pin(grisp_base, led2_b)     -> #{index => 13};
pin(grisp_base, jumper_1)   -> #{index => 14};
pin(grisp_base, jumper_2)   -> #{index => 15};
pin(grisp_base, jumper_3)   -> #{index => 16};
pin(grisp_base, jumper_4)   -> #{index => 17};
pin(grisp_base, jumper_5)   -> #{index => 18};
pin(grisp_base, spi1_pin7)  -> #{index => 19};
pin(grisp_base, spi1_pin8)  -> #{index => 20};
pin(grisp_base, spi1_pin9)  -> #{index => 21};
pin(grisp_base, spi1_pin10) -> #{index => 22};
pin(grisp_base, spi1_pin1)  -> #{index => 23};
pin(grisp_base, spi2_pin1)  -> #{index => 24};
pin(grisp_base, uart_1_cts) -> #{index => 25};
pin(grisp_base, uart_2_txd) -> #{index => 26};
pin(grisp_base, uart_3_rxd) -> #{index => 27};
pin(grisp_base, uart_4_rts) -> #{index => 28};
pin(grisp2, gpio1_1)        -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 0};
pin(grisp2, gpio1_2)        -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 1};
pin(grisp2, gpio1_3)        -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 2};
pin(grisp2, gpio1_4)        -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 3};
pin(grisp2, gpio1_7)        -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 4};
pin(grisp2, gpio1_8)        -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 5};
pin(grisp2, gpio1_9)        -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 6};
pin(grisp2, gpio1_10)       -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 7};
pin(grisp2, gpio_1_3)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 0};
pin(grisp2, gpio_1_4)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 1};
pin(grisp2, gpio_1_5)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 2};
pin(grisp2, gpio_1_6)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 3};
pin(grisp2, gpio_2_3)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 4};
pin(grisp2, gpio_2_4)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 5};
pin(grisp2, gpio_2_5)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 6};
pin(grisp2, gpio_2_6)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 7};
pin(grisp2, gpio_2_7)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 8};
pin(grisp2, gpio_2_8)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 9};
pin(grisp2, gpio_2_9)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 10};
pin(grisp2, gpio_2_10)      -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 11};
pin(grisp2, gpio_2_11)      -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 12};
pin(grisp2, gpio_2_12)      -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 13};
pin(grisp2, gpio_2_13)      -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 14};
pin(grisp2, gpio_2_14)      -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 15};
pin(grisp2, led1_r)         -> #{path => <<"/leds/grisp-rgb1-red\0">>,   property => <<"gpios\0">>,       index => 0};
pin(grisp2, led1_g)         -> #{path => <<"/leds/grisp-rgb1-green\0">>, property => <<"gpios\0">>,       index => 0};
pin(grisp2, led1_b)         -> #{path => <<"/leds/grisp-rgb1-blue\0">>,  property => <<"gpios\0">>,       index => 0};
pin(grisp2, led2_r)         -> #{path => <<"/leds/grisp-rgb2-red\0">>,   property => <<"gpios\0">>,       index => 0};
pin(grisp2, led2_g)         -> #{path => <<"/leds/grisp-rgb2-green\0">>, property => <<"gpios\0">>,       index => 0};
pin(grisp2, led2_b)         -> #{path => <<"/leds/grisp-rgb2-blue\0">>,  property => <<"gpios\0">>,       index => 0};
pin(grisp2, jumper_1)       -> #{path => <<"/jumper-keys\0">>,           property => <<"grisp,gpios\0">>, index => 0};
pin(grisp2, jumper_2)       -> #{path => <<"/jumper-keys\0">>,           property => <<"grisp,gpios\0">>, index => 1};
pin(grisp2, jumper_3)       -> #{path => <<"/jumper-keys\0">>,           property => <<"grisp,gpios\0">>, index => 2};
pin(grisp2, jumper_4)       -> #{path => <<"/jumper-keys\0">>,           property => <<"grisp,gpios\0">>, index => 3};
pin(grisp2, jumper_5)       -> #{path => <<"/jumper-keys\0">>,           property => <<"grisp,gpios\0">>, index => 4};
pin(Platform, Pin) ->
    error({unknown_pin, Platform, Pin}).
