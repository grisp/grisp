-module(grisp_gpio_drv_emu).

% API
-export([open/0]).
-export([command/2]).

%--- API -----------------------------------------------------------------------

open() -> undefined.

command(_State, <<8, 3>>)  -> io:format("LED1 RED OFF~n");
command(_State, <<8, 4>>)  -> io:format("LED1 RED ON~n");
command(_State, <<9, 3>>)  -> io:format("LED1 GREEN OFF~n");
command(_State, <<9, 4>>)  -> io:format("LED1 GREEN ON~n");
command(_State, <<10, 3>>) -> io:format("LED1 BLUE OFF~n");
command(_State, <<10, 4>>) -> io:format("LED1 BLUE ON~n");
command(_State, <<11, 3>>) -> io:format("LED2 RED OFF~n");
command(_State, <<11, 4>>) -> io:format("LED2 RED ON~n");
command(_State, <<12, 3>>) -> io:format("LED2 GREEN OFF~n");
command(_State, <<12, 4>>) -> io:format("LED2 GREEN ON~n");
command(_State, <<13, 3>>) -> io:format("LED2 BLUE OFF~n");
command(_State, <<13, 4>>) -> io:format("LED2 BLUE ON~n");
command(_State, Command) ->
    grisp_device_emu:broadcast({gpio, Command}).
