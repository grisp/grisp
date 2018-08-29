-module(grisp_gpio_drv_emu).

% API
-export([open/0]).
-export([command/2]).
-export([led/2]).  %% for access in mocking

%--- Macros --------------------------------------------------------------------

-define(CMD_START_POLLING,  1).
-define(CMD_STOP_POLLING,   2).
-define(CMD_PIN_CONFIG,    10).
-define(CMD_PIN_GET,       11).
-define(CMD_PIN_CLEAR,     12).
-define(CMD_PIN_SET,       13).

%--- API -----------------------------------------------------------------------

open() -> undefined.

command(_State, <<?CMD_START_POLLING:8>>) -> <<>>;
command(_State, <<?CMD_STOP_POLLING:8>>) -> <<>>;
command(_State, <<Command:8, Pin:8, Type:8, Attr:8, HTrig:8, LTrig:8,
                  PollInterval:32/unsigned-little-integer,
                  DebouncingCounter:32/unsigned-little-integer>>) ->
    Event = {command(Command), type(Type), attr(Attr), int_to_bool(HTrig),
             int_to_bool(LTrig), PollInterval, DebouncingCounter},
    grisp_device_emu:broadcast({gpio, index(Pin), Event});
command(_State, <<Command, Pin>>) when Pin >= 8 andalso Pin =< 13 ->
    led(index(Pin), command(Command));
command(_State, <<Command, Pin>>) ->
    grisp_device_emu:broadcast({gpio, index(Pin), command(Command)}).

%--- Internal ------------------------------------------------------------------

led(Color, Status) ->
    io:format(user, "~s ~s~n", [uppercase(Color), uppercase(Status)]).

%% TODO: In R20 use string:uppercase instead of string:to_upper
uppercase(Atom) when is_atom(Atom) ->
    uppercase(atom_to_list(Atom));
uppercase(String) ->
    string:to_upper(String).

index(0)  -> gpio1_1;
index(1)  -> gpio1_2;
index(2)  -> gpio1_3;
index(3)  -> gpio1_4;
index(4)  -> gpio2_1;
index(5)  -> gpio2_2;
index(6)  -> gpio2_3;
index(7)  -> gpio2_4;
index(8)  -> led1_r;
index(9)  -> led1_g;
index(10) -> led1_b;
index(11) -> led2_r;
index(12) -> led2_g;
index(13) -> led2_b;
index(14) -> jumper_1;
index(15) -> jumper_2;
index(16) -> jumper_3;
index(17) -> jumper_4;
index(18) -> jumper_5;
index(19) -> spi1_pin9;
index(20) -> spi1_pin10;
index(21) -> ss1;
index(22) -> ss2;
index(23) -> uart_1_cts;
index(24) -> uart_2_txd;
index(25) -> uart_3_rxd;
index(26) -> uart_4_rts.

type(0) -> periph_a;
type(1) -> periph_b;
type(2) -> periph_c;
type(3) -> periph_d;
type(4) -> input;
type(5) -> output_0;
type(6) -> output_1.

attr(0) -> [default];
attr(1) -> [pullup].

command(?CMD_PIN_CONFIG) -> configure;
command(?CMD_PIN_GET)    -> get;
command(?CMD_PIN_CLEAR)  -> clear;
command(?CMD_PIN_SET)    -> set.

int_to_bool(0) -> false;
int_to_bool(1) -> true.