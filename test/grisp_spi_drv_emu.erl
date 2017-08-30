-module(grisp_spi_drv_emu).

% API
-export([open/0]).
-export([command/4]).

%--- API -----------------------------------------------------------------------

open() ->
    {ok, _} = grisp_device_emu:start_link(),
    undefined.

command(_State, Slot, Mode, Command) ->
    grisp_device_emu:message(Slot, {spi, Mode, Command}).
