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
    Drivers = application:get_env(grisp, drivers, []),
    SPIDriver = proplists:get_value(spi, Drivers, grisp_spi_drv),
    GPIODriver = proplists:get_value(gpio, Drivers, grisp_gpio_drv),
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    Children = [
        worker(grisp_spi, [SPIDriver]),
        worker(grisp_gpio, [GPIODriver]),
        worker(grisp_gpio_events, gen_event, [{local, grisp_gpio_events}]),
        worker(grisp_gpio_poller, []),
        worker(grisp_led, []),
        supervisor(grisp_device_sup, [])
    ],
    {ok, {SupFlags, Children}}.

%--- Internal ------------------------------------------------------------------

worker(ID, A)    -> worker(ID, ID, A).
worker(ID, M, A) -> #{id => ID, start => {M, start_link, A}}.

supervisor(ID, A) ->
    #{id => ID, start => {ID, start_link, A}, type => supervisor}.
