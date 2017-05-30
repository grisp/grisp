-module(grisp_device).

-include("grisp.hrl").

% API
-export([slot/1]).
-export([default/1]).
-export([setup/1]).
-export([teardown/1]).

%--- API -----------------------------------------------------------------------

slot(Slot) ->
    case ets:lookup(grisp_devices, Slot) of
        [] -> error({no_device_connected, Slot});
        [Device] -> Device
    end.

default(Driver) ->
    % TODO: This means 2 ETS lookups for every default driver call, optimize?
    case ets:lookup(grisp_devices_default, Driver) of
        []               -> error({no_device_present, Driver});
        [{Driver, Slot}] -> slot(Slot)
    end.

setup(Configuration) ->
    ets:new(grisp_devices, [
        named_table,
        {keypos, #device.slot},
        {read_concurrency, true}
    ]),
    ets:new(grisp_devices_default, [
        named_table,
        {read_concurrency, true}
    ]),

    % TODO: Validate ports
    % FIXME: Use maps for devices in later Erlang versions
    Default = lists:foldl(fun({Slot, Driver}, Default) ->
        Pid = grisp_device_sup:start_child(Slot, Driver),
        Device = #device{slot = Slot, driver = Driver, instance = Pid},
        ets:insert(grisp_devices, Device),
        % FIXME: 19+: maps:update_with(Driver, fun(V) -> V end, Device, Default)
        case proplists:is_defined(Driver, Default) of
            true  -> Default;
            false -> [{Driver, Slot}|Default]
        end
    end, [], Configuration),

    [ets:insert(grisp_devices_default, D) || D <- Default],

    [grisp_devices, grisp_devices_default].

teardown(Tables) ->
    [ets:delete(T) || T <- Tables].
