% @private
-module(grisp_gpio_drv).

% API
-export([open/0]).
-export([command/2]).

%--- Macros --------------------------------------------------------------------

-define(PORT_COMMAND_TIMEOUT, 1000).

%--- API -----------------------------------------------------------------------

open() -> open_port({spawn_driver, "grisp_gpio_drv"}, [binary]).

command(Port, Command) ->
    Port ! {self(), {command, Command}},
    receive
        {Port, {data, Resp}} ->
            Resp
    after ?PORT_COMMAND_TIMEOUT ->
            exit({gpio_driver_timeout, Command})
    end.
