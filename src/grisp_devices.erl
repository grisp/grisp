-module(grisp_devices).

-behavior(gen_server).

-include("grisp.hrl").

% API
-export([start_link/0]).
-export([slot/1]).
-export([default/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).


%--- API -----------------------------------------------------------------------

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

slot(Slot) ->
    case ets:lookup(?MODULE, Slot) of
        [] -> error({no_device_connected, Slot});
        [Device] -> Device
    end.

default(Driver) ->
    case ets:match_object(?MODULE, #device{driver = Driver, _ = '_'}, 1) of
        {[], _}       -> error({no_device_present, Driver});
        {[Device], _} -> Device
    end.

%--- Callbacks -----------------------------------------------------------------

init(undefined) ->
    Devices = application:get_env(grisp, devices, []),
    ets:new(?MODULE, [
        named_table,
        ordered_set,
        {keypos, #device.slot},
        {read_concurrency, true}
    ]),

    % TODO: Validate ports
    [init_device(Slot, Driver) || {Slot, Driver} <- Devices],
    {ok, undefined}.

handle_call(Request, From, _State) -> error({unknown_request, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info(Info, _State) -> error({unknown_info, Info}).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) ->
    ets:delete(?MODULE).

%--- Internal ------------------------------------------------------------------

init_device(Slot, Driver) ->
    Pid = grisp_devices_sup:start_child(Slot, Driver),
    Device = #device{slot = Slot, driver = Driver, instance = Pid},
    ets:insert(?MODULE, Device).
