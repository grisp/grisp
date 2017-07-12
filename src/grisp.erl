-module(grisp).

% API
-export([add_device/2]).
-export([remove_device/1]).
-export([devices/0]).
-export([device/1]).

%--- API -----------------------------------------------------------------------

add_device(Slot, Driver) -> grisp_devices:add_device(Slot, Driver).

remove_device(Device) -> grisp_devices:remove_device(Device).

devices() -> grisp_devices:list().

device(Slot) -> grisp_devices:slot(Slot).
