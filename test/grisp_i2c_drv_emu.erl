-module(grisp_i2c_drv_emu).

% API
-export([open/0]).
-export([command/2]).

%--- API -----------------------------------------------------------------------

open() -> undefined.

% FIXME: Placeholder API
command(undefined, Things) ->
    grisp_device_emu:broadcast({i2c, Things}),
    <<0>>.
