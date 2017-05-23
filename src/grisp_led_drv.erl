-module(grisp_led_drv).

% API
-export([open/0]).
-export([command/2]).

%--- API -----------------------------------------------------------------------

% TODO: Use spawn_driver here?
open() -> open_port({spawn, "grisp_led_drv"}, [binary]).

command(Port, Command) -> Port ! {self(), {command, Command}}.
