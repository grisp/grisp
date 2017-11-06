% @doc GRiSP Runtime API.
%
% This module contains the main API for working with the GRiSP runtime.
-module(grisp).

% API
-export([add_device/2]).
-export([add_device/3]).
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

% @doc See {@link add_device/3}.
%
% @equiv add_device(Slot, Driver, #{})
-spec add_device(slot(), module()) -> device().
add_device(Slot, Driver) -> add_device(Slot, Driver, #{}).

% @doc Add and start the device instance for a connected device.
%
% This starts a device driver instance of the specified module and connected to
% the specified slot. No hardware validation is possible to check that the
% correct device is actually connected, but many drivers run an initialization
% check for specific device IDs and other similar characteristics which will
% fail if no device or the wrong device is connected.
%
% The set of options is passed to the driver and is specific to each driver.
%
% Returns the created device instance.
-spec add_device(slot(), module(), map()) -> device().
add_device(Slot, Driver, Opts) -> grisp_devices:add_device(Slot, Driver, Opts).

% @doc Remove and stop the device instance for a device.
%
% This will gracefully stop the device instance and remove it from the runtime.
% This includes running any termination code inside the device driver module
% (see the specific driver module for your device for more information).
-spec remove_device(device()) -> ok.
remove_device(Device) -> grisp_devices:remove_device(Device).

% @doc List all running devices instances.
%
% Returns a list of all running device instances as device objects.
-spec devices() -> [device()].
devices() -> grisp_devices:list().

% @doc Return the device instance at the specified slot (if any).
%
% <h4>Errors</h4>
% <ul>
%   <li>`{no_device_connected, Slot}'.</li>
% </ul>
-spec device(slot()) -> device().
device(Slot) -> grisp_devices:slot(Slot).
