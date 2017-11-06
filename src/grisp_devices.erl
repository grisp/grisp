% @private
-module(grisp_devices).

-behavior(gen_server).

% API
-export([start_link/0]).
-export([setup/0]).
-export([add_device/3]).
-export([remove_device/1]).
-export([list/0]).
-export([slot/1]).
-export([default/1]).
-export([register/2]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("grisp.hrl").

%--- API -----------------------------------------------------------------------

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

setup() ->
    Devices = application:get_env(grisp, devices, []),
    % TODO: Validate ports
    [apply(grisp, add_device, tuple_to_list(Dev)) || Dev <- Devices],
    ok.

add_device(Slot, Driver, Opts) ->
    grisp_devices_sup:start_child(Slot, Driver, Opts),
    slot(Slot).

remove_device(Device) ->
    grisp_devices_sup:terminate_child(Device).

list() ->
    ets:tab2list(?MODULE).

slot(Slot) ->
    case ets:lookup(?MODULE, Slot) of
        [] -> error({no_device_connected, Slot});
        [Device] -> Device
    end.

default(Driver) ->
    case ets:match_object(?MODULE, #device{driver = Driver, _ = '_'}, 1) of
        {[Device], _} -> Device;
        _             -> error({no_device_present, Driver})
    end.

register(Slot, Driver) ->
    gen_server:call(?MODULE, {register, Slot, Driver, self()}).

%--- Callbacks -----------------------------------------------------------------

init(undefined) ->
    ets:new(?MODULE, [
        named_table,
        ordered_set,
        {keypos, #device.slot},
        {read_concurrency, true}
    ]),
    {ok, undefined}.

handle_call({register, Slot, Driver, Pid}, _From, State) ->
    register_device(Slot, Driver, Pid),
    {reply, ok, State}.

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    ets:match_delete(?MODULE, #device{pid = Pid, monitor = Ref, _ = '_'}),
    {noreply, State};
handle_info(Info, _State) ->
    error({unknown_info, Info}).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) ->
    ets:delete(?MODULE).

%--- Internal ------------------------------------------------------------------

register_device(Slot, Driver, Pid) ->
    Ref = erlang:monitor(process, Pid),
    Device = #device{slot = Slot, driver = Driver, pid = Pid, monitor = Ref},
    ets:insert_new(?MODULE, Device).
