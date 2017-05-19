-module(grisp_device).

-include("grisp.hrl").

% API
-export([port/1]).
-export([default/1]).
-export([setup/1]).
-export([teardown/1]).

%--- API -----------------------------------------------------------------------

port(Port) ->
    case ets:lookup(grisp_devices, Port) of
        [] -> error({no_device_connected, Port});
        [Device] -> Device
    end.

default(Driver) ->
    case ets:lookup(grisp_devices_default, Driver) of
        [] -> error({no_device_present, Driver});
        [Port] -> port(Port)
    end.

setup(Configuration) ->
    ets:new(grisp_devices, [
        named_table,
        {keypos, #device.port},
        {read_concurrency, true}
    ]),
    ets:new(grisp_devices_default, [
        named_table,
        {read_concurrency, true}
    ]),

    % TODO: Validate ports
    % FIXME: Use maps for devices in later Erlang versions
    Default = lists:foldl(fun({Port, Driver}, Default) ->
        Device = #device{port = Port, driver = Driver},
        ets:insert(grisp_devices, Device),
        % FIXME: 19+: maps:update_with(Driver, fun(V) -> V end, Device, Default)
        case proplists:is_defined(Driver, Default) of
            true  -> Default;
            false -> [{Driver, Device}|Default]
        end
    end, [], Configuration),

    [ets:insert(grisp_devices_default, D) || D <- Default],

    [grisp_devices, grisp_devices_default].

teardown(Tables) ->
    [ets:delete(T) || T <- Tables].
