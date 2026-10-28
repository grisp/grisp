-module(pmod_mtds).
-include("grisp_docs.hrl").

?moduledoc("""
GRiSP device driver for Digilent Pmod MTDS.

> **NOTE:** This driver does **not** replicate all of the functionality found
in the reference C++ driver.  I hope it replicates enough that interested
developers have templates for any further features they might want to add!
Also, a warning: the C++ driver defines some constants (and even commands??)
which are **not** supported by the device firmware.

For manuals and a C++ reference driver, see
https://github.com/Digilent/vivado-library/tree/master/ip/Pmods/PmodMTDS_v1_0
""").

% Erlang-facing touch event API
-export([configure_touch/2]).
-export([set_debug/1]).
-export([touch_events/0]).

% MTDS commands
-export([bitmap/3]).
-export([bitmap_draw/5]).
-export([bitmap_release/1]).
-export([clear/1]).
-export([color_fg/2]).
-export([color_bg/2]).
-export([draw_rop/2]).
-export([font/1]).
-export([font/2]).
-export([line_to/2]).
-export([move_to/2]).
-export([stock_font/1]).
-export([surface/0]).
-export([surface_display/0]).
-export([surface_release/1]).
-export([surface_bitmap/2]).
-export([text/3]).

% server management
-export([start_link/2]).

% gen_server callbacks
-export([init/1]).
-export([terminate/2]).
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).

-behavior(gen_server).
-include_lib("kernel/include/logger.hrl").
-include("grisp_internal.hrl").  % for device record definition
-include("pmod_mtds.hrl").  % protocol constants

% Stop draining after this many microseconds. An in-flight SPI command cannot
% be interrupted, so this is a best-effort deadline rather than a hard limit.
-define(TOUCH_DRAIN_BUDGET_US, 16000).
% Restore fine movement when the queue is empty or drains in less than half
% the time budget. A timed-out drain immediately doubles the delta instead.
-define(TOUCH_FAST_DRAIN_US, ?TOUCH_DRAIN_BUDGET_US div 2).
% Minimum movement threshold in pixels. Callers may configure an initial value.
-define(BASE_TOUCH_MOVE_DELTA, 2).
% Maximum movement threshold; doubling under overload stops here.
-define(MAX_TOUCH_MOVE_DELTA, 16).

%
% Types
%

% Color specified as RGB in [0.0, 1.0].
-type color() :: {R :: float(), G :: float(), B :: float()}.

% Various handle types.
-type surface() :: ?REGION_SURFACE .. (?REGION_SURFACE + ?HANDLE_SPACE).
-type bitmap()  :: ?REGION_BITMAP  .. (?REGION_BITMAP  + ?HANDLE_SPACE).
-type font()    :: ?REGION_FONT    .. (?REGION_FONT    + ?HANDLE_SPACE).
-type window()  :: ?REGION_WINDOW  .. (?REGION_WINDOW  + ?HANDLE_SPACE).

-type position() :: {X :: integer(), Y :: integer()}.
-type touch_move_delta() :: {DeltaX :: 1..32767, DeltaY :: 1..32767}.

% Shape of touch event messages sent to registered listeners.
-type touch_event() :: {
    touch,
    WindowHandle :: window(),
    Type :: {up | down | move, 0..4},
    Position :: position(),
    Weight :: integer(),
    Speed :: integer()
}.

% Names of built-in fonts.
-type stock_font() :: console.

% Opcode enum.
-type command() :: 0..(16#3FFF) .

%
% Public interface for MTDS
%

?doc("Enable or disable MTDS diagnostic logging.").
-spec set_debug(boolean()) -> ok.
set_debug(Enabled) when is_boolean(Enabled) ->
    #device{pid = PID} = grisp_devices:default(?MODULE),
    gen_server:call(PID, {set_debug, Enabled});
set_debug(_Enabled) ->
    erlang:error(badarg).

?doc("Configure touch sensitivity and movement-event resolution.").
-spec configure_touch(1..7, touch_move_delta()) -> ok.
configure_touch(Sensitivity, {DeltaX, DeltaY})
        when is_integer(Sensitivity), Sensitivity >= 1, Sensitivity =< 7,
             is_integer(DeltaX), DeltaX >= 1, DeltaX =< 32767,
             is_integer(DeltaY), DeltaY >= 1, DeltaY =< 32767 ->
    #device{pid = PID} = grisp_devices:default(?MODULE),
    gen_server:call(PID, {configure_touch, Sensitivity, DeltaX, DeltaY});
configure_touch(_Sensitivity, _MoveDelta) ->
    erlang:error(badarg).

?doc("Return all touch events popped within the drain time budget, in order.").
-spec touch_events() -> [touch_event()].
touch_events() ->
    #device{pid = PID} = grisp_devices:default(?MODULE),
    gen_server:call(PID, touch_events).

?doc("Blanks the MTDS to the indicated color.").
-spec clear(Color :: color()) -> ok.
clear(_Color = {R, G, B}) ->
    % NOTE: The "GDI" subsystem insulates the user from the 16-bit realities
    %       of the MTDS, but for whatever reason this routine does not.
    RR = round(R * 2#11111),
    GG = round(G * 2#111111),
    BB = round(B * 2#11111),
    <<BigEndian:16>> = <<RR:5, GG:6, BB:5>>,
    {ok, <<>>} = send_command(?UTILITY_CLEAR, <<BigEndian:16/little>>),
    ok.

?doc("Set the foreground draw color of a surface.").
-spec color_fg(surface(), color()) -> ok.
color_fg(Handle, _Color = {R, G, B}) ->
    Parameter = <<
        Handle:32/little,
        (round(B*255)):8,
        (round(G*255)):8,
        (round(R*255)):8,
        0:8
    >>,
    {ok, <<>>} = send_command(?GRAPHICS_DS_FG_SET, Parameter),
    ok.

?doc("Set the background draw color of a surface.").
-spec color_bg(surface(), color()) -> ok.
color_bg(Handle, _Color = {R, G, B}) ->
    Parameter = <<
        Handle:32/little,
        (round(B*255)):8,
        (round(G*255)):8,
        (round(R*255)):8,
        0:8
    >>,
    {ok, <<>>} = send_command(?GRAPHICS_DS_BG_SET, Parameter),
    ok.

?doc("Set the bitwise operator used to combine pen + bitmap when drawing.").
-spec draw_rop(surface(), integer()) -> ok.
% TODO: Hide the raw opcodes.
draw_rop(Handle, OpCode) ->
    Parameter = <<Handle:32/little, OpCode:16/little>>,
    {ok, <<>>} = send_command(?GRAPHICS_DS_ROP_SET, Parameter),
    ok.

?doc("Retrieve a surface handle to the global display.").
-spec surface_display() -> {ok, surface()}.
surface_display() ->
    {ok, <<Handle:32/little, _/binary>>} = send_command(?GRAPHICS_DS_DISPLAY),
    {ok, Handle}.

?doc("Generate a new surface (with no underlying bitmap).").
-spec surface() -> {ok, surface()}.
surface() ->
    {ok, <<Handle:32/little, _Rest/binary>>} = send_command(?GRAPHICS_DS_NEW),
    {ok, Handle}.

?doc("Release a surface handle.").
-spec surface_release(surface()) -> ok.
surface_release(Handle) ->
    {ok, <<>>} = send_command(?GRAPHICS_DS_RELEASE, <<Handle:32/little>>),
    ok.

?doc("Allocate a new bitmap (usually to back a surface).").
-spec bitmap(integer(), integer(), monochrome | color) -> {ok, bitmap()}.
bitmap(X, Y, Color) ->
    EncodedColor = case Color of
        monochrome -> 1;
        color -> 16
    end,
    Parameter = <<X:16/little, Y:16/little, EncodedColor:16/little>>,
    {ok, Reply} = send_command(?GRAPHICS_BITMAP_NEW, Parameter),
    <<Handle:32/little, _Rest/binary>> = Reply,
    {ok, Handle}.

?doc("Release a bitmap.").
-spec bitmap_release(bitmap()) -> ok.
bitmap_release(Handle) ->
    {ok, <<>>} = send_command(?GRAPHICS_BITMAP_RELEASE, <<Handle:32/little>>),
    ok.

?doc("Tether a surface to a backing bitmap.").
-spec surface_bitmap(surface(), bitmap()) -> ok.
surface_bitmap(DSHandle, BitmapHandle) ->
    Parameter = <<DSHandle:32/little, BitmapHandle:32/little>>,
    {ok, <<>>} = send_command(?GRAPHICS_DS_BITMAP_SET, Parameter),
    ok.

?doc("Set the active font for a surface.").
-spec font(surface(), font()) -> ok.
font(Handle, Font) ->
    Parameter = <<Handle:32/little, Font:32/little>>,
    {ok, <<>>} = send_command(?GRAPHICS_DS_FONT_SET, Parameter),
    ok.

?doc("Retrieve the current font of a surface.").
-spec font(surface()) -> {ok, font()}.
font(Handle) ->
    Parameter = <<Handle:32/little>>,
    {ok, <<Font:32/little>>} = send_command(?GRAPHICS_DS_FONT_GET, Parameter),
    {ok, Font}.

?doc("Write text on a surface at a given position.").
-spec text(surface(), iolist(), position()) -> ok.
text(DSHandle, Text, _Pos = {X, Y}) ->
    EncodedText = iolist_to_binary(Text),
    TotalSize = size(EncodedText),
    Parameter = <<
        DSHandle:32/little,
        X:16/little,
        Y:16/little,
        TotalSize:16/little
    >>,
    {ok, <<>>} = send_command(?GRAPHICS_TEXT, Parameter, EncodedText),
    ok.

?doc("Write the contents of one bitmap onto another.").
-spec bitmap_draw(
    surface(), position(), surface(), position(), {integer(), integer()}
) -> ok.
bitmap_draw(DestDS, {DX, DY}, SourceDS, {SX, SY}, {Width, Height}) ->
    Parameter = <<
        DestDS:32/little,
        DX:16/little-signed,
        DY:16/little-signed,
        Width:16/little-signed,
        Height:16/little-signed,
        SourceDS:32/little,
        SX:16/little-signed,
        SY:16/little-signed
    >>,
    {ok, <<>>} = send_command(?GRAPHICS_DS_DRAW, Parameter),
    ok.

?doc("Move the cursor position on a surface.").
-spec move_to(surface(), position()) -> ok.
move_to(Handle, _Position = {X, Y}) ->
    Parameter = <<Handle:32/little, X:16/little, Y:16/little>>,
    {ok, <<>>} = send_command(?GRAPHICS_DS_MOVE, Parameter),
    ok.

?doc("Draw a line on a surface from the current cursor position to a new one.").
-spec line_to(surface(), position()) -> ok.
line_to(Handle, _Position = {X, Y}) ->
    Parameter = <<Handle:32/little, X:16/little, Y:16/little>>,
    {ok, <<>>} = send_command(?GRAPHICS_DS_LINE_TO, Parameter),
    ok.

%
% Public interface for driver
%

?doc("Launch the MTDS driver.").
-spec start_link(grisp_spi:bus(), any()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(Interface, _Opts) ->
    gen_server:start_link(?MODULE, [Interface], []).

%
% gen_server wrapper for driver
%

?doc("""
Internal state of the MTDS driver.

MTDS is connected to the GRiSP host by SPI, a synchronous protocol, so that we
can only send and receive bytes in pairs.  However, messages to+from the device
are often slightly staggered --- after all, the device needs to spend time
processing a request before it can formulate and transmit a reply --- so we
retain bytes received from the bus in a buffer for asynchronous processing.
""").
-record(state, {
    % link with MTDS
    bus,
    buffer = << >>,
    move_delta = {?BASE_TOUCH_MOVE_DELTA, ?BASE_TOUCH_MOVE_DELTA},
    debug = false
}).
-type state() :: #state{
    % link with MTDS
    bus :: grisp_spi:ref(),
    buffer :: binary(),
    move_delta :: {integer(), integer()},
    debug :: boolean()
}.

% Initializes the bus object and syncs with the MTDS. The caller schedules polls.
?doc(false).
init([Interface]) ->
    % NOTE: MTDS wants to put freq in 3.5–4 MHz, whereas GRiSP runs at 0.1 MHz.
    ok = grisp_devices:register(Interface, ?MODULE),

    Bus = grisp_spi:open(Interface),
    ok = reset(Interface),
    State = #state{bus = Bus},
    {ok, SyncedState} = sync(State),
    {ok, StartedState, _Reply} = command(SyncedState, ?UTILITY_INIT),

    {ok, StartedState}.

% Reset is active-low on pin 8 of the MTDS Pmod interface.  Holding the line
% low for 1 ms follows the reference driver.  The MTDS firmware needs two
% seconds to boot after reset before protocol synchronization can begin.
?doc(false).
-spec reset(grisp_spi:bus()) -> ok.
reset(spi2) ->
    Reset = grisp_gpio:open(spi2_pin8, #{mode => {output, 0}}),
    timer:sleep(1),
    ok = grisp_gpio:set(Reset, 1),
    timer:sleep(2000),
    ok;
reset(_Interface) ->
    ok.

% Process a command directive.
?doc(false).
handle_call({command, CommandPair, Parameters, Packet}, _From, State) ->
    {ok, NewState, Reply} = command(State, CommandPair, Parameters, Packet),
    TrackedState = case {CommandPair, Parameters} of
        {?UTILITY_TOUCH_MOVE_DELTA,
         <<DeltaX:16/little-signed, DeltaY:16/little-signed>>} ->
            NewState#state{
                move_delta = {DeltaX, DeltaY}
            };
        _ -> NewState
    end,
    {reply, {ok, Reply}, TrackedState};
handle_call({configure_touch, Sensitivity, DeltaX, DeltaY}, _From, State) ->
    {ok, SensitivityState, <<>>} = command(
        State, ?UTILITY_TOUCH_SENSITIVITY, <<Sensitivity:32/little>>
    ),
    {ok, DeltaState, <<>>} = command(
        SensitivityState,
        ?UTILITY_TOUCH_MOVE_DELTA,
        <<DeltaX:16/little-signed, DeltaY:16/little-signed>>
    ),
    {reply, ok, DeltaState#state{move_delta = {DeltaX, DeltaY}}};
handle_call({set_debug, Enabled}, _From, State) ->
    {reply, ok, State#state{debug = Enabled}};
% Return raw, ordered events to the caller; only the drain deadline controls
% how many are read. Change the MTDS event rate before replying.
handle_call(touch_events, _From, State) ->
    Started = erlang:monotonic_time(microsecond),
    Deadline = Started + ?TOUCH_DRAIN_BUDGET_US,
    {DrainedState, Events, Outcome} = poll_touch_events(State, Deadline, []),
    Elapsed = erlang:monotonic_time(microsecond) - Started,
    {DeltaX, DeltaY} = DrainedState#state.move_delta,
    DesiredDelta = case Outcome of
        timed_out -> {
            min(?MAX_TOUCH_MOVE_DELTA, DeltaX * 2),
            min(?MAX_TOUCH_MOVE_DELTA, DeltaY * 2)
        };
        _ when Events =:= [] orelse Elapsed < ?TOUCH_FAST_DRAIN_US ->
            {
                max(?BASE_TOUCH_MOVE_DELTA, DeltaX div 2),
                max(?BASE_TOUCH_MOVE_DELTA, DeltaY div 2)
            };
        _ -> {DeltaX, DeltaY}
    end,
    AdjustedState = set_touch_move_delta(DrainedState, DesiredDelta),
    log_touch_poll_result(
        DrainedState#state.debug, Outcome, Started, Elapsed, Events,
        {DeltaX, DeltaY}, DesiredDelta
    ),
    {reply, Events, AdjustedState}.

log_touch_poll_result(true, timed_out, Started, DrainTime, Events, PreviousDelta, NewDelta) ->
    TotalTime = erlang:monotonic_time(microsecond) - Started,
    ?LOG_DEBUG(
        "MTDS touch poll timed out: drain=~B us total=~B us events=~B move_delta=~p -> ~p",
        [DrainTime, TotalTime, length(Events), PreviousDelta, NewDelta]
    );
log_touch_poll_result(_Debug, _Outcome, _Started, _DrainTime, _Events, _PreviousDelta, _NewDelta) ->
    ok.

set_touch_move_delta(State = #state{move_delta = Delta}, Delta) -> State;
set_touch_move_delta(State, {DeltaX, DeltaY} = Delta) ->
    Parameters = <<DeltaX:16/little-signed, DeltaY:16/little-signed>>,
    {ok, AdjustedState, <<>>} = command(
        State, ?UTILITY_TOUCH_MOVE_DELTA, Parameters
    ),
    AdjustedState#state{move_delta = Delta}.

?doc(false).
handle_cast(_Request, State) ->
    {noreply, State}.

?doc(false).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

?doc(false).
terminate(_Reason, _State) ->
    ok.

%
% Underlying device comms
%

?doc("Package a command sequence into a binary payload.").
-spec command_payload(command(), binary()) -> binary().
command_payload(Command, Parameters) ->
    Size = size(Parameters),
    <<
        ?HEADER_COMMAND:2,
        Command:14,
        Size:16/little,
        Parameters/binary
    >>.

?doc("Send a command to MTDS and confirm delivery.").
-spec command(state(), command(), binary(), binary()) ->
    {ok, state(), binary()} | {error, state(), {status, integer()}} |
    {error, any()}.
command(State, Command) ->
    command(State, Command, << >>).
command(State, Command, Parameters) ->
    command(State, Command, Parameters, << >>).
command(State, Command, Parameters, DataPacket) ->
    Payload = command_payload(Command, Parameters),
    CommandState = transfer(State, Payload),
    BusyState = drop(wait_until(CommandState, <<?CONTROL_BUSY>>)),

    {ok, PacketState} = send_data(BusyState, Command, DataPacket),

    DoneState = drop(wait_until(PacketState, <<?CONTROL_DONE>>)),

    % retrieve status
    QueryPayload = command_payload(?UTILITY_READ_STATUS, << >>),
    StatusState = transfer(DoneState, QueryPayload),
    PreheaderState = wait_until(StatusState, <<?HEADER_STATUS:2>>),
    {ok, PostheaderState, Header} = read(PreheaderState, 4),
    case Header of
        <<?HEADER_STATUS:2, Command:14, ?STATUS_OK:8, ByteCount:8>> ->
            read(PostheaderState, ByteCount);
        <<?HEADER_STATUS:2, ?UTILITY_EVENT_POP:14, Status:8, ByteCount:8>>
                when Command =:= ?UTILITY_EVENT_POP ->
            {ok, EmptyState, _Payload} = read(PostheaderState, ByteCount),
            {error, EmptyState, {status, Status}};
        % TODO: could add more nuanced error accounting here,
        %       see eg ProtoDefs.h:staCmdNotSupported
        _ ->
            {error, bad_status}
    end.

?doc("""
Write a data packet.  Generally follows a command and allows for data of
flexible length.
""").
-spec send_data(state(), command(), binary()) -> {ok, state()}.
send_data(State, _Command, << >>) ->
    {ok, State};
send_data(State, Command, Packet) ->
    {Chunk, Rest} = case Packet of
        <<A:?PACKET_OUT_MAX_SIZE/binary, B/binary>> -> {A, B};
        _ -> {Packet, << >>}
    end,
    ReadyState = drop(wait_until(State, <<?CONTROL_READY_FOR_DATA>>)),
    ChunkSize = size(Chunk),
    TransferredState = transfer(ReadyState, <<
        ?HEADER_DATA:2,
        ?DATA_OUT:6,
        Command:8,  % deliberately discards the "class" high-order bits!
        ChunkSize:16/little,
        Chunk/binary
    >>),
    send_data(TransferredState, Command, Rest).

?doc("Re-initializes the MTDS link protocol.").
-spec sync(state()) -> {ok, state()} | {error, any()}.
sync(State) ->
    % NOTE: These magic numbers are pulled from the MTDS library.  There is not
    %       a lot of principle; set them how you like.
    WipedState = State#state{buffer = << >>},
    {ok, SyncedState} = sync_enter(WipedState, 10, 768),
    {ok, StartedState} = sync_exit(SyncedState, 5),
    {ok, StartedState#state{buffer = << >>}}.

?doc("Puts the MTDS into the synchronizing state.").
-spec sync_enter(state(), integer(), integer()) ->
    {ok, state()} | {error, any()}.
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

?doc("Returns the MTDS from the synchronizing state to the ready state.").
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

?doc("Drain the MTDS queue until empty or the time budget is reached.").
-spec poll_touch_events(state(), integer(), [touch_event()]) ->
    {state(), [touch_event()], drained | timed_out | {empty_status, integer()}}.
poll_touch_events(State, Deadline, Events) ->
    case erlang:monotonic_time(microsecond) >= Deadline of
        true -> {State, lists:reverse(Events), timed_out};
        false -> poll_next_touch_event(State, Deadline, Events)
    end.

poll_next_touch_event(State, Deadline, Events) ->
    case command(State, ?UTILITY_EVENT_POP) of
        {error, PolledState, {status, Status}} ->
            {PolledState, lists:reverse(Events), {empty_status, Status}};
        {ok, PolledState, RawResult} ->
            handle_touch_pop(PolledState, Deadline, Events, RawResult)
    end.

handle_touch_pop(PolledState, Deadline, Events, RawResult) ->
    case RawResult of
        <<>> -> {PolledState, lists:reverse(Events), drained};
        <<0:128>> -> {PolledState, lists:reverse(Events), drained};
        <<_Prefix:10/binary, 0:16/little, _Suffix:4/binary>> ->
            {PolledState, lists:reverse(Events), drained};
        <<_Prefix:10/binary, Kind:16/little, _Suffix:4/binary>>
                when Kind >= 16#10, Kind =< 16#1E ->
            Event = parse_touch_event(RawResult),
            poll_touch_events(PolledState, Deadline, [Event | Events]);
        _ ->
            case PolledState#state.debug of
                true -> ?LOG_DEBUG("MTDS touch pop returned an unexpected payload: ~p", [RawResult]);
                false -> ok
            end,
            {PolledState, lists:reverse(Events), drained}
    end.

?doc("Parses an individual MTDS message into a touch_event() object.").
-spec parse_touch_event(binary()) -> touch_event().
parse_touch_event(RawResult) ->
    <<
        _Timestamp:32/little,
        WindowHandle:32/little,
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
    {touch, WindowHandle, Maneuver, {X, Y}, Weight, Speed}.

?doc("Poll MTDS, discarding until we see some Goal prefix.").
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

?doc("Poll AtLeast bytes from MTDS.").
-spec read(state(), integer()) -> {ok, state(), binary()}.
read(State = #state{buffer = Buffer}, AtLeast) when size(Buffer) >= AtLeast ->
    #state{buffer = <<Read:AtLeast/binary, Rest/binary>>} = State,
    {ok, State#state{buffer = Rest}, Read};
read(State = #state{buffer = Buffer}, AtLeast) ->
    ToRead = AtLeast - size(Buffer),
    FatState = transfer(State, binary:copy(<<?CONTROL_READ>>, ToRead)),
    read(FatState, AtLeast).

?doc("Perform a synchronous transfer with the MTDS.").
-spec transfer(state(), binary()) -> state().
transfer(State = #state{bus = Bus, buffer = Buffer}, Binary) ->
    [Response] = grisp_spi:transfer(Bus, [{?SPI_MODE, Binary}]),
    % io:format("Out: ~w; In: ~w~n", [Binary, Response]),  % useful debug
    State#state{buffer = <<Buffer/binary, Response/binary>>}.

?doc("Discard the next unread reply byte from the MTDS.").
-spec drop(state()) -> state().
drop(State = #state{buffer = <<_Char:8, Rest/binary>>}) ->
    State#state{buffer = Rest};
drop(State) ->
    State.

%
% Utilities
%

?doc("Shorthand to send a command directive to the MTDS driver.").
-spec send_command(command(), binary(), binary()) -> {ok, binary()}.
send_command(Selector) ->
    send_command(Selector, << >>).
send_command(Selector, Parameter) ->
    send_command(Selector, Parameter, << >>).
send_command(Selector, Parameter, DataPacket) ->
    #device{pid = PID} = grisp_devices:default(?MODULE),
    gen_server:call(PID, {command, Selector, Parameter, DataPacket}).

?doc("Convert a human-readable font atom to a font() instance.").
-spec stock_font(stock_font()) -> font().
stock_font(Font) ->
    case Font of
        console     -> ?REGION_FONT + ?STOCK_FONT + 0
        % % NOTE: These appear to be unimplemented in the MTDS firmware.
        % extra_small -> ?REGION_FONT + ?STOCK_FONT + 1;
        % small       -> ?REGION_FONT + ?STOCK_FONT + 2;
        % medium      -> ?REGION_FONT + ?STOCK_FONT + 3;
        % large       -> ?REGION_FONT + ?STOCK_FONT + 4;
        % extra_large -> ?REGION_FONT + ?STOCK_FONT + 5
    end.
