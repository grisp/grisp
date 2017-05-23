-module(grisp_led_drv_emu).

% API
-export([open/0]).
-export([command/2]).

%--- API -----------------------------------------------------------------------

open() -> led_emu.

command(led_emu, <<LED, R, G, B>>) ->
    io:format("LED~p: {~p,~p,~p}\n", [LED, R, G, B]).
