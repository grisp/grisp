-module(grisp_sup).

-behaviour(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%--- Callbacks -----------------------------------------------------------------

init([]) ->
    {ok, {{one_for_one, 0, 1}, [
        % FIXME: Erlang 19+: #{id => spi, start => {grisp_spi, start_link, []}},
        {spi, {grisp_spi, start_link, []}, permanent, infinity, worker, [grisp_spi]},
        % FIXME: Erlang 19+: #{id => devices, start => {grisp_device_sup, start_link, []}, type => supervisor}
        {devices, {grisp_device_sup, start_link, []}, permanent, infinity, supervisor, [grisp_device_sup]}
    ]}}.
