-module(grisp_device_emu).

-behavior(gen_server).

% API
-export([start_link/0]).
-export([message/2]).
-export([broadcast/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- API -----------------------------------------------------------------------

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

message(Slot, Message) ->
    gen_server:call(?MODULE, {message, Slot, Message}).

broadcast(Message) ->
    gen_server:call(?MODULE, {broadcast, Message}).

%--- Callbacks -----------------------------------------------------------------

init(undefined) ->
    Devices = application:get_env(grisp, devices, []),
    {ok, [init_emulator(Device) || Device <- Devices]}.

handle_call({message, Slot, Message}, _From, State) when is_atom(Slot) ->
    {Emu, EmuState} = proplists:get_value(Slot, State),
    {Data, NewEmuState} = Emu:message(EmuState, Message),
    {reply, Data, lists:keyreplace(Slot, 1, State, {Slot, {Emu, NewEmuState}})};
handle_call({broadcast, Message}, _From, State) ->
    NewState = [
        {Slot, {Emu, Emu:broadcast(EmuState, Message)}}
        || {Slot, {Emu, EmuState}} <- State
    ],
    {reply, ok, NewState}.

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info(Info, _State) -> error({unknown_info, Info}).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------

init_emulator({Slot, Driver}) ->
    init_emulator({Slot, Driver, #{}});
init_emulator({Slot, Driver, _Opts}) ->
    Emu = list_to_atom(atom_to_list(Driver) ++ "_emu"),
    {Slot, {Emu, Emu:init()}}.
