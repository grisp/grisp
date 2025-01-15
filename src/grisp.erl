-module(grisp).
-include("grisp_docs.hrl").

?moduledoc("""
GRiSP Runtime API.

This module contains the main API for working with the GRiSP runtime.
""").

% API
-export([add_device/2]).
-export([add_device/3]).
-export([remove_device/1]).
-export([devices/0]).
-export([device/1]).

-include("grisp_internal.hrl").

%--- Types ---------------------------------------------------------------------

-export_type([slot/0, device/0]).

?doc("""
Designates a hardware slot on the GRiSP board. This should be the lower case
version of the slot names printed on the board itself.

E.g. `spi1` for _SPI1_ or `gpio2` for _GPIO2_.
""").
-type slot() :: atom().

?doc("""
An object representing the running device.
""").
-opaque device() :: #device{}.

%--- API -----------------------------------------------------------------------

?doc(#{equiv => add_device/3}).
-spec add_device(slot(), module()) -> device().
add_device(Slot, Driver) -> add_device(Slot, Driver, #{}).

?doc("""
Add and start the device instance for a connected device.

This starts a device driver instance of the specified module and connected to
the specified slot. No hardware validation is possible to check that the
correct device is actually connected, but many drivers run an initialization
check for specific device IDs and other similar characteristics which will
fail if no device or the wrong device is connected.

The set of options is passed to the driver and is specific to each driver.

Returns the created device instance.
""").
-spec add_device(slot(), module(), map()) -> device().
add_device(Slot, Driver, Opts) -> grisp_devices:add_device(Slot, Driver, Opts).

?doc("""
Remove and stop the device instance for a device.

This will gracefully stop the device instance and remove it from the runtime.
This includes running any termination code inside the device driver module
(see the specific driver module for your device for more information).
""").

-spec remove_device(device()) -> ok.
remove_device(Device) -> grisp_devices:remove_device(Device).

?doc("""
List all running devices instances.

Returns a list of all running device instances as device objects.
""").
-spec devices() -> [device()].
devices() -> grisp_devices:list().

?doc("""
Return the device instance at the specified slot (if any).

### Errors
* `{no_device_connected, Slot}`.
""").
-spec device(slot()) -> device().
device(Slot) -> grisp_devices:instance(Slot).
