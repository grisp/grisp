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
    SPIDriver = proplists:get_value(spi, application:get_env(grisp, drivers, []), grisp_spi_drv),
    GPIODriver = proplists:get_value(gpio, application:get_env(grisp, drivers, []), grisp_gpio_drv),
    LEDDriver = proplists:get_value(led, application:get_env(grisp, drivers, []), grisp_led_drv),
    {ok, {{one_for_one, 0, 1}, [
        % FIXME: Erlang 19+: #{id => spi, start => {grisp_spi, start_link, []}},
        {spi, {grisp_spi, start_link, [SPIDriver]}, permanent, infinity, worker, [grisp_spi, grisp_spi_drv]},
        % FIXME: Erlang 19+: #{id => gpio, start => {grisp_gpio, start_link, []}},
        {gpio, {grisp_gpio, start_link, [GPIODriver]}, permanent, infinity, worker, [grisp_gpio, grisp_gpio_drv]},
        % FIXME: Erlang 19+: #{id => led, start => {grisp_led, start_link, []}},
        {led, {grisp_led, start_link, [LEDDriver]}, permanent, infinity, worker, [grisp_led, grisp_led_drv]},
        % FIXME: Erlang 19+: #{id => devices, start => {grisp_device_sup, start_link, []}, type => supervisor}
        {devices, {grisp_device_sup, start_link, []}, permanent, infinity, supervisor, [grisp_device_sup]}
    ]}}.
