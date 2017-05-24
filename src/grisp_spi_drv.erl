-module(grisp_spi_drv).

% API
-export([open/0]).
-export([command/3]).

%--- API -----------------------------------------------------------------------

open() -> open_port({spawn_driver, "grisp_spi_drv"}, [binary]).

command(Port, Slot, Command) ->
    Port ! {self(), {command, <<(slave_select(Slot)), Command/binary>>}}.

%--- Internal ------------------------------------------------------------------

slave_select(spi1) -> 2;
slave_select(spi2) -> 3.
