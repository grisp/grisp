-module(grisp_gpio_drv_emu).

% API
-export([open/0]).
-export([command/2]).
-export([led/2]).  %% for access in mocking

%--- API -----------------------------------------------------------------------

open() -> undefined.

command(_State, <<Pin, Command>>) when Pin >= 8 andalso Pin =< 13 ->
    led(index(Pin), command(Command));
command(_State, <<Pin, Command>>) ->
    grisp_device_emu:broadcast({gpio, index(Pin), command(Command)});
command(_State, <<Pin, Command, Type, Attr>>) ->
    grisp_device_emu:broadcast({gpio, index(Pin), {command(Command), type(Type), attr(Attr)}}).

%--- Internal ------------------------------------------------------------------

led(Color, Status) ->
    io:format("~s ~s~n", [uppercase(Color), uppercase(Status)]).

%% TODO: In R20 use string:uppercase instead of string:to_upper
uppercase(Atom) when is_atom(Atom) ->
    uppercase(atom_to_list(Atom));
uppercase(String) ->
    string:to_upper(String).

index(8) -> led1_r;
index(9) -> led1_g;
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
index(21) -> ss1.

type(0) -> periph_a;
type(1) -> periph_b;
type(2) -> periph_c;
type(3) -> periph_d;
type(4) -> input;
type(5) -> output_0;
type(6) -> output_1.

attr(0) -> [default].

command(1) -> configure;
command(2) -> get;
command(3) -> clear;
command(4) -> set.
