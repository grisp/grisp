% @private
-module(grisp_sup).

-behavior(supervisor).

% API
-export([start_link/0]).
-export([start_link/2]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() ->
    {BoardWorkers, InternalWorkers} = case grisp_hw:platform() of
        grisp2 ->
            {[], []};
        grisp_base ->
            GenEventOpts = [{local, grisp_gpio_events}],
            {
                [
                    worker(grisp_gpio, [driver(gpio, grisp_gpio_drv)])
                ],
                [
                    worker(grisp_gpio_events, gen_event, GenEventOpts),
                    worker(grisp_gpio_poller, [])
                ]
            }
    end,
    Children = [
        supervisor(grisp_board_sup, [
            worker(grisp_spi, [driver(spi, grisp_nspi)])
        ] ++ BoardWorkers),
        supervisor(grisp_devices_sup, grisp_devices_sup, []),
        supervisor(grisp_internal_sup, InternalWorkers ++ [
            worker(grisp_led, []),
            worker(grisp_devices, []),
            worker(grisp_onewire, [])
        ])
    ],
    start_link(?MODULE, Children).

start_link(Name, Children) ->
    supervisor:start_link({local, Name}, ?MODULE, Children).

%--- Callbacks -----------------------------------------------------------------

init(Children) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    {ok, {SupFlags, Children}}.

%--- Internal ------------------------------------------------------------------

worker(ID, A)    -> worker(ID, ID, A).
worker(ID, M, A) -> #{id => ID, start => {M, start_link, A}}.

supervisor(ID, Children) when is_list(Children) ->
    supervisor(ID, ?MODULE, [Children]).

supervisor(ID, Module, Args) ->
    #{
        id => ID,
        start => {Module, start_link, [ID] ++ Args},
        type => supervisor
    }.

driver(Driver, DefaultMod) ->
    Drivers = application:get_env(grisp, drivers, []),
    proplists:get_value(Driver, Drivers, DefaultMod).
