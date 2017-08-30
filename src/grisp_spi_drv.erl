% @private
-module(grisp_spi_drv).

% API
-export([open/0]).
-export([command/4]).

-define(CPOL_LOW, 0).
-define(CPOL_HIGH, 1).
-define(CPHA_LEADING, 0).
-define(CPHA_TRAILING, 2).

%--- Macros --------------------------------------------------------------------

-define(PORT_COMMAND_TIMEOUT, 1000).

%--- API -----------------------------------------------------------------------

open() -> open_port({spawn_driver, "grisp_spi_drv"}, [binary]).

command(Port, Slot, Mode, Command) ->
    Port ! {self(), {command, <<(slave_select(Slot)), (mode(Mode)), Command/binary>>}},
    receive
        {Port, {data, Resp}} ->
            Resp
    after ?PORT_COMMAND_TIMEOUT ->
            exit({spi_driver_timeout, Command})
    end.

%--- Internal ------------------------------------------------------------------

slave_select(spi1) -> 2;
slave_select(spi2) -> 3.

mode(#{cpol := low,  cpha := leading})  -> ?CPOL_LOW  bor ?CPHA_LEADING;
mode(#{cpol := low,  cpha := trailing}) -> ?CPOL_LOW  bor ?CPHA_TRAILING;
mode(#{cpol := high, cpha := leading})  -> ?CPOL_HIGH bor ?CPHA_LEADING;
mode(#{cpol := high, cpha := trailing}) -> ?CPOL_HIGH bor ?CPHA_TRAILING.
