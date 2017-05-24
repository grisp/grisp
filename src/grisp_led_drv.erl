-module(grisp_led_drv).

% API
-export([open/0]).
-export([command/3]).

%--- API -----------------------------------------------------------------------

open() -> open_port({spawn_driver, "grisp_led_drv"}, [binary]).

command(Port, Pos, Command) ->
    Port ! {self(), {command, <<Pos:8, Command/binary>>}}.
