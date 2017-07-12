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
    Child = #{id => Slot, start => {Driver, start_link, [Slot]}},
    {ok, Pid} = supervisor:start_child(?MODULE, Child),
    Pid.

%--- Callbacks -----------------------------------------------------------------

init([]) -> {ok, {#{strategy => one_for_one}, []}}.
