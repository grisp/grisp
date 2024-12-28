-module(pmod_mtds).
-moduledoc """
GRiSP device driver for Digilent Pmod MTDS.

For manuals and a C++ reference driver, see
https://github.com/Digilent/vivado-library/tree/master/ip/Pmods/PmodMTDS_v1_0
""".
-export([clear/1]).  % public, interactions
-export([start_link/1]).  % public, server management
-export([  % private: gen_server callbacks
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2
]).
-behavior(gen_server).
-include("grisp_internal.hrl").  % for device record definition

%% milliseconds between querying for touch events
-define(TOUCH_POLL_PERIOD, 100).




% display is 240x320 — maybe 200x300 in accessible coords?






%%%
%%% Public interface for MTDS
%%%

-type color() :: {R :: float(), G :: float(), B :: float()}.

-doc "Blanks the MTDS to the indicated color.".
-spec clear(color()) -> ok.
clear(Color) ->
    {R, G, B} = Color,
    #device{pid = PID} = grisp_devices:default(?MODULE),
    Payload = mtds_from_color(R, G, B),
    {ok, <<>>} = gen_server:call(PID, {command, 16#1, 16#b, Payload}),
    ok.

%%%
%%% Public interface for driver
%%%

-doc "Launch the MTDS driver.".
-spec start_link(grisp_spi:bus()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Interface) ->
    gen_server:start_link(?MODULE, [Interface], []).

%%%
%%% gen_server wrapper for driver
%%%

-type window() :: integer().

-doc "Internal state of the MTDS driver.".
-record(state, {
    %% link with MTDS
    bus, buffer = << >>,
    %% windowing system
    window_refs = #{16#C4400000 => default}, listeners = #{}
}).
-type state() :: #state{
    %% link with MTDS
    bus :: grisp_spi:ref(),
    buffer :: binary(),
    %% windowing system
    window_refs :: #{window() => reference()},
    listeners :: #{reference() | default => sets:set(pid())}
}.

-doc false.
%%% Initializes the bus object to form a driver state, but defers actual
%%% device initialization to a timeout (see handle_info/2) so as not to get in
%%% the way of any supervisor tree.
init([Interface]) ->
    %% NOTE: MTDS wants to put freq in 3.5–4 MHz, much faster than GRiSP's 0.1 MHz.
    ok = grisp_devices:register(Interface, ?MODULE),
    {ok, Interface, 0}.

-doc false.
%%% Handles a call.
handle_call({command, Class, Command, Parameters}, _From, State) ->
    {ok, NewState, Reply} = command(State, {Class, Command}, Parameters),
    {reply, {ok, Reply}, NewState}.

-doc false.
%%% Handles a cast.
handle_cast(_Request, State) ->
    {noreply, State}.

-doc false.
%%% Handles raw messages and timeout events.
%% Poll for touch events.
handle_info(timeout, State = #state{}) ->
    {PolledState, Replies} = poll_touch_events(State),
    lists:foreach(
        fun(Reply) ->
            io:format("Event: ~w~n", [Reply])
        end,
        Replies
    ),
    {noreply, PolledState, ?TOUCH_POLL_PERIOD};
%% Complete initialization.
handle_info(timeout, Interface) when is_atom(Interface) ->
    Bus = grisp_spi:open(Interface),
    State = #state{bus = Bus},
    %% NOTE: Reference driver toggles the reset pin, but we don't have access.
    {ok, SyncedState} = sync(State),
    {ok, StartedState, _Reply} = command(SyncedState, {2#01, 16#2}),
    {noreply, StartedState, ?TOUCH_POLL_PERIOD}.

-doc false.
%%% Handles a code update.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-doc false.
%%% Handles teardown.
terminate(_Reason, _State) ->
    ok.

%%%
%%% Underlying device comms
%%%

%% Various protocol constants.
-define(SPI_MODE, #{clock => {low, leading}}).

-define(HEADER_COMMAND, 2#01).
-define(HEADER_STATUS, 2#10).

-define(CLASS_UTILITY, 2#01).

-define(CONTROL_READ,  16#1).
-define(CONTROL_START, 16#2).
-define(CONTROL_SYNC,  16#3).

-define(CONTROL_READY,   16#20).
-define(CONTROL_SYNCING, 16#25).

-define(STATUS_OK, 16#0).

-type class() :: 0..3 .
-type command() :: 0..63 .

-doc "Package a command sequence into a binary payload.".
-spec command_payload(integer(), integer(), binary()) -> binary().
command_payload(Class, Command, Parameters) ->
    Size = size(Parameters),
    <<
        ?HEADER_COMMAND:2,
        Class:6,  % this is which command
        Command:8,
        Size:16/little,
        Parameters/binary
    >>.

-doc "Send a command to MTDS and confirm delivery.".
-spec command(state(), {class(), command()}, binary()) ->
    {ok, state(), binary()} | {error, any()}.
command(State, {Class, Command}) ->
    command(State, {Class, Command}, << >>).
command(State, {Class, Command}, Parameters) ->
    _DataPackets = [],

    Payload = command_payload(Class, Command, Parameters),
    CommandState = transfer(State, Payload),
    BusyState = drop(wait_until(CommandState, <<16#21>>)),
    DoneState = drop(wait_until(BusyState, <<16#22>>)),

    % retrieve status
    QueryPayload = command_payload(?CLASS_UTILITY, 16#1, << >>),
    StatusState = transfer(DoneState, QueryPayload),
    PreheaderState = wait_until(StatusState, <<?HEADER_STATUS:2>>),
    {ok, PostheaderState, Header} = read(PreheaderState, 4),
    case Header of
        <<?HEADER_STATUS:2, Class:6, Command:8, ?STATUS_OK:8, ByteCount:8>> ->
            read(PostheaderState, ByteCount);
        _ ->
            {error, bad_status}
    end.

-doc "Re-initializes the MTDS link protocol.".
-spec sync(state()) -> {ok, state()} | {error, any()}.
sync(State) ->
    %% NOTE: These magic numbers are pulled from the MTDS library.  There is not
    %%       a lot of principle; set them how you like.
    WipedState = State#state{buffer = << >>},
    {ok, SyncedState} = sync_enter(WipedState, 10, 768),
    {ok, StartedState} = sync_exit(SyncedState, 5),
    {ok, StartedState#state{buffer = << >>}}.

-doc "Puts the MTDS into the synchronizing state.".
-spec sync_enter(state(), integer(), integer()) -> {ok, state()} | {error, any()}.
sync_enter(State, _SuccessesNeeded = 0, _TrialsToGo) ->
    {ok, State};
sync_enter(_State, _SuccessesNeeded, _TrialsToGo = 0) ->
    {error, no_sync};
sync_enter(State, SuccessesNeeded, TrialsToGo) ->
    TransferredState = transfer(State, <<?CONTROL_SYNC>>),
    PeeledState = TransferredState#state{buffer = << >>},
    case TransferredState#state.buffer of
        <<?CONTROL_SYNCING>> ->
            sync_enter(PeeledState, SuccessesNeeded - 1, TrialsToGo - 1);
        _ ->
            sync_enter(PeeledState, SuccessesNeeded,     TrialsToGo - 1)
    end.

-doc "Returns the MTDS from the synchronizing state to the ready state.".
-spec sync_exit(state(), integer()) -> {ok, state()} | {error, any()}.
sync_exit(_State, _TrialsToGo = 0) ->
    {error, no_sync};
sync_exit(State, TrialsToGo) ->
    TransferredState = transfer(State, <<?CONTROL_START>>),
    PeeledState = TransferredState#state{buffer = << >>},
    case TransferredState#state.buffer of
        <<?CONTROL_READY>>   -> {ok, PeeledState};
        <<?CONTROL_SYNCING>> -> sync_exit(PeeledState, TrialsToGo - 1)
    end.

-doc "".
%% TODO: Could limit the recursion here if it interferes with draw commands.
poll_touch_events(State) ->
    {ok, QueriedState, Status} = command(State, {?CLASS_UTILITY, 16#11}),
    case Status of
        <<0:32/little>> -> {QueriedState, []};
        _ ->
            {ok, PolledState, RawResult} = command(QueriedState, {?CLASS_UTILITY, 16#14}),
            Result = parse_touch_event(RawResult, PolledState),
            {UltimateState, Results} = poll_touch_events(PolledState),
            {UltimateState, [Result | Results]}
    end.

-doc "".
parse_touch_event(RawResult, State) ->
    <<
        _Timestamp:32/little,
        RawWindowHandle:32/little,
        X:16/little-signed,
        MessageKind:16/little,
        Y:16/little-signed,
        Weight:8,
        Speed:8
    >> = RawResult,
    Maneuver = {
        case (MessageKind - 16#10) rem 3 of
            0 -> down;
            1 -> move;
            2 -> up
        end,
        (MessageKind - 16#10) div 3
    },
    %% TODO: let fail? or maybe filter dispossessed messages?
    WindowHandle = case State#state.window_refs of
        #{RawWindowHandle := Ref} -> {RawWindowHandle, Ref};
        _ -> {RawWindowHandle, none}
    end,
    {touch, WindowHandle, Maneuver, {X, Y}, Weight, Speed}.

-doc "Poll MTDS until we see some Goal prefix.".
-spec wait_until(state(), bitstring()) -> state().
wait_until(State = #state{buffer = << >>}, Goal) ->
    NewState = transfer(State, <<?CONTROL_READ>>),
    wait_until(NewState, Goal);
wait_until(State = #state{buffer = Buffer}, Goal) ->
    GoalSize = bit_size(Goal),
    case Buffer of
        <<Goal:GoalSize/bitstring, _Rest/bitstring>> ->
            State;
        <<_Char:8, Rest/binary>> ->
            wait_until(State#state{buffer = Rest}, Goal)
    end.

-doc "Poll AtLeast bytes from the MTDS.".
-spec read(state(), integer()) -> {ok, state(), binary()}.
read(State = #state{buffer = Buffer}, AtLeast) when size(Buffer) >= AtLeast ->
    #state{buffer = <<Read:AtLeast/binary, Rest/binary>>} = State,
    {ok, State#state{buffer = Rest}, Read};
read(State = #state{buffer = Buffer}, AtLeast) ->
    ToRead = AtLeast - size(Buffer),
    FatState = transfer(State, binary:copy(<<?CONTROL_READ>>, ToRead)),
    read(FatState, AtLeast).

-doc "Perform a synchronous transfer with the MTDS.".
-spec transfer(state(), binary()) -> state().
transfer(State = #state{bus = Bus, buffer = Buffer}, Binary) ->
    [Response] = grisp_spi:transfer(Bus, [{?SPI_MODE, Binary}]),
    % io:format("transfer; in: ~s ; out: ~s ~n", [
    %     hex_from_binary(Binary),
    %     hex_from_binary(Response)
    % ]),
    State#state{buffer = <<Buffer/binary, Response/binary>>}.

-doc "Discard the next unread reply byte from the MTDS.".
-spec drop(state()) -> state().
drop(State = #state{buffer = <<_Char:8, Rest/binary>>}) ->
    State#state{buffer = Rest};
drop(State) ->
    State.

%%%
%%% Utilities
%%%

-doc "Convert an RGB triple in [0.0, 1.0] to an MTDS-compatible color payload.".
-spec mtds_from_color(R :: float(), G :: float(), B :: float()) -> binary().
mtds_from_color(R, G, B) ->
    RR = round(R * 2#11111),
    GG = round(G * 2#111111),
    BB = round(B * 2#11111),
    <<BigEndian:16>> = <<RR:5, GG:6, BB:5>>,
    <<BigEndian:16/little>>.

-doc "Convert an MTDS-compatible color payload to an RGB triple in [0.0, 1.0].".
-spec color_from_mtds(binary()) -> {R :: float(), G :: float(), B :: float()}.
color_from_mtds(Payload) ->
    <<Packed:16/little>> = Payload,
    <<RR:5, GG:6, BB:5>> = <<Packed>>,
    {RR / 2#11111, GG / 2#111111, BB / 2#11111}.

hex_from_binary(Binary) ->
    <<
        <<
            <<
                <<Y>>
                ||  <<Nibble:4>> <= <<Byte>>,
                    Y <- integer_to_list(Nibble, 16)
            >>/binary,
            ","
        >>
        || <<Byte:8>> <= Binary
    >>.
