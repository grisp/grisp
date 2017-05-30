-module(grisp_device_sup).

-behaviour(supervisor).

% API
-export([start_link/0]).
-export([start_child/2]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Slot, Driver) ->
    Child = {
        Slot,
        {Driver, start_link, [Slot]},
        permanent,
        1000,
        worker,
        [Driver]
    },
    {ok, Pid} = supervisor:start_child(?MODULE, Child),
    Pid.

%--- Callbacks -----------------------------------------------------------------

init([]) ->
    {ok, {{one_for_one, 0, 1}, []}}.
