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
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    Children = [
        worker(grisp_spi, [driver(spi, grisp_spi_drv)]),
        worker(grisp_gpio, [driver(gpio, grisp_gpio_drv)]),
        worker(grisp_gpio_events, gen_event, [{local, grisp_gpio_events}]),
        worker(grisp_gpio_poller, []),
        worker(grisp_led, []),
        supervisor(grisp_devices_sup, []),
        worker(grisp_devices, [])
    ],
    {ok, {SupFlags, Children}}.

%--- Internal ------------------------------------------------------------------

worker(ID, A)    -> worker(ID, ID, A).
worker(ID, M, A) -> #{id => ID, start => {M, start_link, A}}.

supervisor(ID, A) ->
    #{id => ID, start => {ID, start_link, A}, type => supervisor}.

driver(Driver, DefaultMod) ->
    Drivers = application:get_env(grisp, drivers, []),
    proplists:get_value(Driver, Drivers, DefaultMod).
