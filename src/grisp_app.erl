-module(grisp_app).

-behaviour(application).

-include("grisp.hrl").

% API
-export([start/2]).
-export([stop/1]).

%--- API -----------------------------------------------------------------------

start(_Type, _Args) ->
    Devices = application:get_env(grisp, devices, []),
    {ok, Sup} = grisp_sup:start_link(),
    DeviceState = grisp_devices:setup(Devices),
    {ok, Sup, DeviceState}.

stop(DeviceState) ->
    grisp_devices:teardown(DeviceState).
