-module(grisp_gpio_drv).

% API
-export([open/0, configure/3, configure/4,
	 configure_gpio/3, get/2, clear/2, set/2, set/3, clear/3,
         enable_spi_ss1/1, disable_spi_ss1/1]).

%--- API -----------------------------------------------------------------------

open() -> open_port({spawn_driver, "grisp_gpio_drv"}, [binary]).

configure(Port, Pin_index, Type) ->
    configure(Port, Pin_index, Type, [default]).

configure(Port, Pin_index, Type, Attr) ->
    Port ! {self(), {command, <<(index(Pin_index)):8, 1:8, 
				(map_type(Type)):8, (map_attr(Attr)):8>>}}.

configure_gpio(Port, gpio1, {T1, T2, T3, T4}) ->
    configure(Port, 0, T1),
    configure(Port, 1, T2),
    configure(Port, 2, T3),
    configure(Port, 3, T4);
configure_gpio(Port, gpio2, {T1, T2, T3, T4}) ->
    configure(Port, 4, T1),
    configure(Port, 5, T2),
    configure(Port, 6, T3),
    configure(Port, 7, T4).

get(Port, Pin_index) ->
    Port ! {self(), {command, <<(index(Pin_index)):8, 2:8>>}}.

clear(Port, Pin_index) ->
    Port ! {self(), {command, <<(index(Pin_index)):8, 3:8>>}}.

set(Port, Pin_index) ->
    Port ! {self(), {command, <<(index(Pin_index)):8, 4:8>>}}.

set(Port, gpio1, Pin) when 1 =< Pin, Pin =< 4 ->    
    set(Port, Pin - 1 + index(gpio1));          % 0 .. 3     
set(Port, gpio2, Pin) when 1 =< Pin, Pin =< 4 ->    
    set(Port, Pin - 1 + index(gpio2)).          % 4 .. 7     

clear(Port, gpio1, Pin) when 1 =< Pin, Pin =< 4 ->    
    clear(Port, Pin - 1 + index(gpio1));	% 0 .. 3     
clear(Port, gpio2, Pin) when 1 =< Pin, Pin =< 4 ->    
    clear(Port, Pin - 1 + index(gpio2)).	% 4 .. 7     

disable_spi_ss1(Port) ->
    configure(Port, ss1, output_1).

enable_spi_ss1(Port) ->
    configure(Port, ss1, periph_c).

map_type(periph_a) ->
    0;
map_type(periph_b) ->
    1;
map_type(periph_c) ->
    2;
map_type(periph_d) ->
    3;
map_type(input) ->
    4;
map_type(output_0) ->				% default value 0
    5;
map_type(output_1) ->				% default value 1
    6.

map_attr([default]) ->
    0.

index(gpio1) ->
    0;
index(gpio2) ->
    4;
index(led1_r) ->
    8;
index(led1_g) ->
    9;
index(led1_b) ->
    10;
index(led2_r) ->
    11;
index(led2_g) ->
    12;
index(led2_b) ->
    13;
index(jumper_1) ->
    14;
index(jumper_2) ->
    15;
index(jumper_3) ->
    16;
index(jumper_4) ->
    17;
index(jumper_5) ->
    18;
index(spi1_pin9) ->
    19;
index(spi1_pin10) ->
    20;
index(ss1) ->
    21;
index(ss2) ->
    22;
index(Any) when is_integer(Any) ->
    Any.

