-module(grisp).

% API
-export([add_device/2]).
-export([remove_device/1]).
-export([devices/0]).
-export([device/1]).

-include("grisp.hrl").

%--- Types ---------------------------------------------------------------------

-export_type([slot/0, device/0]).

-type slot() :: atom().
% Designates a hardware slot on the GRiSP board. This should be the lower case
% version of the slot names printed on the board itself.
%
% E.g. `spi1' for <em>SPI1</em> or `gpio2' for <em>GPIO2</em>.

-opaque device() :: #device{}.
% An object representing the running device.

%--- API -----------------------------------------------------------------------

-spec add_device(slot(), module()) -> device().
add_device(Slot, Driver) -> grisp_devices:add_device(Slot, Driver).

-spec remove_device(device()) -> ok.
remove_device(Device) -> grisp_devices:remove_device(Device).

-spec devices() -> [device()].
devices() -> grisp_devices:list().

-spec device(slot()) -> device().
device(Slot) -> grisp_devices:slot(Slot).
