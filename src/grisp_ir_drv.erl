% @private
-module(grisp_ir_drv).

-import(grisp_pins, [index/1, 
		     map_type/1,
		     attr/1,
		     map_attr/1,
		     bool/1]).

% API
-export([open/0]).
-export([configure_ir/3]).
-export([remove_ir/2]).

%--- Macros --------------------------------------------------------------------

-define(PORT_COMMAND_TIMEOUT, 1000).

%--- API -----------------------------------------------------------------------

open() -> open_port({spawn_driver, "grisp_ir_drv"}, [binary]).

configure_ir(Port, Pin, Attr) ->
    Command = <<(index(Pin)):8, 1:8, (map_type(input)):8, (map_attr(Attr)):8>>,
    command(Port, Command).

remove_ir(Port, Pin) ->
    Command = <<(index(Pin)):8, 2:8>>,
    command(Port, Command).

command(Port, Command) ->
    Port ! {self(), {command, Command}},
    receive
        {Port, {data, Resp}} ->
	    Resp
    after ?PORT_COMMAND_TIMEOUT ->
	    exit({ir_driver_timeout, Command})
    end.   
		
