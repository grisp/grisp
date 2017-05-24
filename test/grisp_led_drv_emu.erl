-module(grisp_led_drv_emu).

% API
-export([open/0]).
-export([command/3]).

%--- API -----------------------------------------------------------------------

open() -> led_emu.

command(led_emu, Pos, <<R, G, B>>) ->
    io:format("LED~p: {~p,~p,~p}\n", [Pos, R, G, B]).
