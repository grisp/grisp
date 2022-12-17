% @private
-module(grisp_devices).

-behavior(gen_server).

% API
-export([start_link/0]).
-export([setup/0]).
-export([add_device/3]).
-export([remove_device/1]).
-export([list/0]).
-export([instance/1]).
-export([slots/1]).
-export([default/1]).
-export([register/2]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("grisp_internal.hrl").

-define(DEFAULT_OPTS, #{access => public}).

%--- API -----------------------------------------------------------------------

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

setup() ->
    Devices = application:get_env(grisp, devices, []),
    % TODO: Validate ports
    [apply(grisp, add_device, tuple_to_list(Dev)) || Dev <- Devices],
    ok.

add_device(Slot, Driver, UserOpts) ->
    Opts = maps:merge(?DEFAULT_OPTS, UserOpts#{owner => self()}),
    call({add, Slot, Driver, Opts}).

remove_device(Device) ->
    % TODO: Cleanup needed? Cancel monitors? Notify owner?
    grisp_devices_sup:terminate_child(Device).

list() ->
    ets:tab2list(?MODULE).

instance(Slot) ->
    Self = self(),
    case ets:lookup(?MODULE, Slot) of
        [] -> error({no_device_connected, Slot});
        [#device{access = public} = Device] -> Device;
        [#device{access = exclusive, owner = Self} = Device] -> Device;
        [#device{owner = Owner}] -> error({protected, Owner})
    end.

slots(Driver) ->
    Pattern = #device{driver = Driver, slot = '$1', _ = '_'},
    lists:flatten(ets:match(grisp_devices, Pattern)).

default(Driver) ->
    Self = self(),
    case ets:match_object(?MODULE, #device{driver = Driver, _ = '_'}, 1) of
        [#device{access = public} = Device] -> Device;
        [#device{owner = Self} = Device] -> Device;
        [#device{owner = Owner}] -> error({protected, Owner});
        [] -> error({no_device_present, Driver});
        _ -> error(multiple_devices)
    end.

register(Slot, Driver) ->
    call({register, Slot, Driver, self()}).

%--- Callbacks -----------------------------------------------------------------

init(undefined) ->
    ets:new(?MODULE, [
        named_table,
        ordered_set,
        {keypos, #device.slot},
        {read_concurrency, true}
    ]),
    {ok, undefined}.

handle_call({add, Slot, Driver, Opts}, From, State) ->
    #{owner := Owner, access := Access} = Opts,
    DriverOpts = maps:without([owner, access], Opts),
    spawn(fun() -> grisp_devices_sup:start_child(Slot, Driver, DriverOpts) end),
    Device = #device{
        slot = Slot,
        driver = Driver,
        driver_opts = DriverOpts,
        access = Access,
        owner = Owner,
        owner_monitor = mon(Owner, owner),
        status = {initializing, From}
    },
    case ets:insert_new(?MODULE, Device) of
        true -> {noreply, State};
        % TODO: Cleanup needed if device exists? Cancel monitors?
        false -> error(not_implemented)
    end;
handle_call({register, Slot, Driver, Pid}, _From, State) ->
    case ets:lookup(?MODULE, Slot) of
        [#device{driver = Driver, status = {initializing, From}} = Device] ->
            NewDevice = Device#device{
                pid = Pid, monitor = mon(Pid, driver), status = ready
            },
            ets:insert(?MODULE, NewDevice),
            gen_server:reply(From, NewDevice),
            {reply, ok, State};
        _ ->
            {reply, {error, not_in_driver_process}, State}
    end.

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info({{'DOWN', driver}, Ref, process, Pid, _Reason}, State) ->
    ets:match_delete(?MODULE, #device{pid = Pid, monitor = Ref, _ = '_'}),
    {noreply, State};
handle_info({{'DOWN', owner}, Ref, process, Owner, _Reason}, State) ->
    Pattern = #device{owner = Owner, owner_monitor = Ref, _ = '_'},
    Devices = ets:match_object(?MODULE, Pattern),
    [grisp_devices_sup:terminate_child(D) || D <- Devices],
    ets:match_delete(?MODULE, Pattern),
    {noreply, State};
handle_info(Info, _State) ->
    error({unknown_info, Info}).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ets:delete(?MODULE).

%--- Internal ------------------------------------------------------------------

call(Call) ->
    case gen_server:call(?MODULE, Call) of
        {error, Error} -> error(Error);
        Result -> Result
    end.

mon(Pid, Tag) -> erlang:monitor(process, Pid, [{tag, {'DOWN', Tag}}]).
