-module(grisp_gpio_drv_emu).

% API
-export([open/0]).
-export([command/2]).

%--- API -----------------------------------------------------------------------

open() -> undefined.

command(_State, Command) ->
    grisp_device_emu:broadcast({gpio, Command}).
