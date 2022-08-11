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
    ExtraInternalWorkers = case grisp_hw:platform() of
        igrisp -> [
            worker(grisp_eeprom_som, grisp_eeprom, [som]),
            worker(grisp_eeprom_board, grisp_eeprom, [board]),
            worker(grisp_barebox, [som])
        ];
        grisp2 -> [
            worker(grisp_onewire, []),
            worker(grisp_eeprom_som, grisp_eeprom, [som]),
            worker(grisp_eeprom_board, grisp_eeprom, [board]),
            worker(grisp_barebox, [som]),
            worker(grisp_gpio_events, gen_event, [{local, grisp_gpio_events}]),
            worker(grisp_gpio_poller, []),
            worker(grisp_led, [])
        ];
        grisp_base -> [
            worker(grisp_gpio_events, gen_event, [{local, grisp_gpio_events}]),
            worker(grisp_gpio_poller, []),
            worker(grisp_led, [])
        ]
    end,
    Children = [
        supervisor(grisp_devices_sup, grisp_devices_sup, []),
        supervisor(grisp_internal_sup, [
            worker(grisp_devices, [])
            | ExtraInternalWorkers
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

worker(ID, A) -> worker(ID, ID, A).
worker(ID, M, A) -> #{id => ID, start => {M, start_link, A}}.

supervisor(ID, Children) when is_list(Children) ->
    supervisor(ID, ?MODULE, [Children]).

supervisor(ID, Module, Args) ->
    #{
        id => ID,
        start => {Module, start_link, [ID] ++ Args},
        type => supervisor
    }.
