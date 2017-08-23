% @private
-module(grisp_spi_drv).

% API
-export([open/0]).
-export([command/3]).

%--- Macros --------------------------------------------------------------------

-define(PORT_COMMAND_TIMEOUT, 1000).

%--- API -----------------------------------------------------------------------

open() -> open_port({spawn_driver, "grisp_spi_drv"}, [binary]).

command(Port, Slot, Command) ->
    Port ! {self(), {command, <<(slave_select(Slot)), Command/binary>>}},
    receive
        {Port, {data, Resp}} ->
            Resp
    after ?PORT_COMMAND_TIMEOUT ->
            exit({spi_driver_timeout, Command})
    end.

%--- Internal ------------------------------------------------------------------

slave_select(spi1) -> 2;
slave_select(spi2) -> 3.
