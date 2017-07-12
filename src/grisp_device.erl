-module(grisp_device).

-include("grisp.hrl").

% API
-export([slot/1]).
-export([default/1]).
-export([setup/1]).
-export([teardown/1]).

%--- API -----------------------------------------------------------------------

slot(Slot) ->
    case ets:lookup(?MODULE, Slot) of
        [] -> error({no_device_connected, Slot});
        [Device] -> Device
    end.

default(Driver) ->
    case ets:match_object(?MODULE, #device{driver = Driver, _ = '_'}, 1) of
        {[], _}       -> error({no_device_present, Driver});
        {[Device], _} -> Device
    end.

setup(Configuration) ->
    ets:new(?MODULE, [
        named_table,
        ordered_set,
        {keypos, #device.slot},
        {read_concurrency, true}
    ]),

    % TODO: Validate ports
    [init_device(Slot, Driver) || {Slot, Driver} <- Configuration],
    [grisp_devices].

teardown(Tables) ->
    [ets:delete(T) || T <- Tables].

%--- Internal ------------------------------------------------------------------

init_device(Slot, Driver) ->
    Pid = grisp_device_sup:start_child(Slot, Driver),
    Device = #device{slot = Slot, driver = Driver, instance = Pid},
    ets:insert(?MODULE, Device).
