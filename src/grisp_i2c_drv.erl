% @private
-module(grisp_i2c_drv).

% API
-export([open/0]).
-export([command/2]).

%--- Macros --------------------------------------------------------------------

-define(PORT_COMMAND_TIMEOUT, 2000).

%--- API -----------------------------------------------------------------------

open() -> open_port({spawn_driver, "grisp_i2c_drv"}, [binary]).

command(Port, Command) ->
    Port ! {self(), {command, <<Command/binary>>}},
    receive
        {Port, {data, Resp}} ->
            Resp
    after ?PORT_COMMAND_TIMEOUT ->
            exit({i2c_driver_timeout, Command})
    end.
