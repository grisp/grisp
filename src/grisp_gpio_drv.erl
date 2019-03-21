% @private
-module(grisp_gpio_drv).

% API
-export([open/0]).
-export([command/2]).

%--- Macros --------------------------------------------------------------------

-define(PORT_COMMAND_TIMEOUT, 1000).
-define(RESP_MSG_CODE, $R).

%--- API -----------------------------------------------------------------------

open() -> open_port({spawn_driver, "grisp_gpio_drv"}, [binary]).

command(Port, Command) ->
    Port ! {self(), {command, Command}},
    receive
        {Port, {data, <<?RESP_MSG_CODE, Resp/binary>>}} ->
            Resp
    after ?PORT_COMMAND_TIMEOUT ->
            exit({gpio_ex_driver_timeout, Command})
    end.
