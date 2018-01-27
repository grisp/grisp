% @private
-module(grisp_ir_drv).

-import(grisp_pins, [index/1, 
		     map_type/1,
		     attr/1,
		     map_attr/1,
		     bool/1]).

% API
-export([open/0]).
-export([register_ir/3]).
-export([activate_ir/2, activate_ir/3]).
-export([disable_ir/2]).
-export([remove_ir/2]).
-export([close/1]).

%--- Macros --------------------------------------------------------------------

-define(PORT_COMMAND_TIMEOUT, 1000).

%--- API -----------------------------------------------------------------------

open() -> open_port({spawn_driver, "grisp_ir_drv"}, [binary]).

register_ir(Port, Pin, Attr) ->
    Command = <<(index(Pin)):8, 1:8, (map_type(input)):8, (map_attr(Attr)):8>>,
    command(Port, Command).

activate_ir(Port, Pin) -> activate_ir(Port, Pin, 0).

activate_ir(Port, Pin, N) when N >= 0 ->
    Command = <<(index(Pin)):8, 2:8, N:8>>,
    command(Port, Command).

disable_ir(Port, Pin) ->
    Command = <<(index(Pin)), 3:8>>,
    command(Port, Command).

remove_ir(Port, Pin) ->
    Command = <<(index(Pin)):8, 4:8>>,
    command(Port, Command).

close(Port) ->
    Port ! {self(), close},
    receive
	{Port, closed} -> closed
    after ?PORT_COMMAND_TIMEOUT ->
	    exit({ir_driver_timeout, close})
    end.

command(Port, Command) ->
    Port ! {self(), {command, Command}},
    receive
        {Port, {data, Resp}} ->
	    Resp
    after ?PORT_COMMAND_TIMEOUT ->
	    exit({ir_driver_timeout, Command})
    end.   
		
