% @private
-module(grisp_devices_sup).

-behavior(supervisor).

-include("grisp.hrl").

% API
-export([start_link/1]).
-export([start_child/3]).
-export([terminate_child/1]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

start_child(Slot, Driver, Opts) ->
    Child = #{id => Slot, start => {Driver, start_link, [Slot, Opts]}},
    {ok, Pid} = supervisor:start_child(?MODULE, Child),
    Pid.

terminate_child(Device) ->
    ok = supervisor:terminate_child(?MODULE, Device#device.slot),
    ok = supervisor:delete_child(?MODULE, Device#device.slot).

%--- Callbacks -----------------------------------------------------------------

init([]) -> {ok, {#{strategy => one_for_one}, []}}.
