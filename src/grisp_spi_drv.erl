-module(grisp_spi_drv).

% API
-export([open/0]).
-export([command/2]).

%--- API -----------------------------------------------------------------------

open() -> open_port({spawn_driver, "grisp_spi_drv"}, [binary]).

command(Port, Command) -> Port ! {self(), {command, Command}}.
