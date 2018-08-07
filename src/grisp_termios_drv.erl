% @private
-module(grisp_termios_drv).

% API
-export([open/0]).
-export([command/2]).

%--- API -----------------------------------------------------------------------

open() -> open_port({spawn_driver, "grisp_termios_drv"}, [binary]).

command(Port, Command) ->
    port_command(Port, Command),
    ok.
