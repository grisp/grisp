-module(pmod_mtds).
-moduledoc """
GRiSP device driver for Digilent Pmod MTDS.

**NOTE:** This driver does **not** replicate all of the functionality found in
    the reference C++ driver.  I hope it replicates enough that interested
    developers have templates for any further features they might want to add!
    Also, a warning: the C++ driver defines some constants (and even comamnds??)
    which are **not** supported by the device firmware.

For manuals and a C++ reference driver, see
https://github.com/Digilent/vivado-library/tree/master/ip/Pmods/PmodMTDS_v1_0 .
""".
-export([register/0, register/1]).  % public, touch events
-export([  % public, MTDS commands
    clear/1, color_fg/2, color_bg/2, draw_rop/2,
    surface_display/0, surface/0, surface_release/1, bitmap/3, bitmap_release/1,
    surface_bitmap/2, font/1, font/2, text/3, bitmap_draw/5,
    move_to/2, line_to/2,
    stock_font/1
]).
-export([start_link/1]).  % public, server management
-export([  % private: gen_server callbacks
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2
]).
-export([poll_loop/1]).  % private
-behavior(gen_server).
-include("grisp_internal.hrl").  % for device record definition

-define(TOUCH_POLL_PERIOD, 100).  % milliseconds between touch event queries

%%%
%%% Types
%%%

-doc "Color specified as RGB in [0.0, 1.0].".
-type color() :: {R :: float(), G :: float(), B :: float()}.

%% Various handle tyles.
-type window() :: integer().
-type surface() :: integer().
-type bitmap() :: integer().
-type font() :: integer().
-type position() :: {X :: integer(), Y :: integer()}.

-doc "Shape of touch event messages sent to registered listeners.".
-type touch_event() :: {
    touch,
    WindowHandle :: window(),
    Type :: {up | down | move, 0..4},
    Position :: position(),
    Weight :: integer(),
    Speed :: integer()
}.

-doc "Names of built-in fonts.".
-type stock_font() :: console.

-doc "Toplevel categorization of what kind of thing we're asking MTDS to do.".
-type class_atom() :: utility | graphics.

%%%
%%% Public interface for MTDS
%%%

-doc "Registers the caller for touch events for the indicated window.".
-spec register(window()) -> ok.
register() ->
    pmod_mtds:register(16#C4400000).
register(Window) ->
    #device{pid = PID} = grisp_devices:default(?MODULE),
    gen_server:call(PID, {register, self(), Window}).

-doc "Blanks the MTDS to the indicated color.".
-spec clear(Color :: color()) -> ok.
clear(_Color = {R, G, B}) ->
    %% NOTE: The "GDI" subsystem insulates the user from the 16-bit realities
    %%       of the MTDS, but for whatever reason this routine does not.
    RR = round(R * 2#11111),
    GG = round(G * 2#111111),
    BB = round(B * 2#11111),
    <<BigEndian:16>> = <<RR:5, GG:6, BB:5>>,
    {ok, <<>>} = send_command({utility, 16#b}, <<BigEndian:16/little>>),
    ok.

-doc "Set the foreground draw color of a surface.".
-spec color_fg(surface(), color()) -> ok.
color_fg(Handle, _Color = {R, G, B}) ->
    Parameter = <<
        Handle:32/little,
        (round(B*255)):8,
        (round(G*255)):8,
        (round(R*255)):8,
        0:8
    >>,
    {ok, <<>>} = send_command({graphics, 16#10}, Parameter),
    ok.

-doc "Set the background draw color of a surface.".
-spec color_bg(surface(), color()) -> ok.
color_bg(Handle, _Color = {R, G, B}) ->
    Parameter = <<
        Handle:32/little,
        (round(B*255)):8,
        (round(G*255)):8,
        (round(R*255)):8,
        0:8
    >>,
    {ok, <<>>} = send_command({graphics, 16#12}, Parameter),
    ok.

-doc "Set the bitwise operator used to combine pen + bitmap when drawing.".
-spec draw_rop(surface(), integer()) -> ok.
%% TODO: Hide the raw opcodes.
draw_rop(Handle, OpCode) ->
    Parameter = <<Handle:32/little, OpCode:16/little>>,
    {ok, <<>>} = send_command({graphics, 16#1c}, Parameter),
    ok.

-doc "Retrieve a surface handle to the global display.".
-spec surface_display() -> {ok, surface()}.
surface_display() ->
    {ok, <<Handle:32/little, _Rest/binary>>} = send_command({graphics, 16#1}),
    {ok, Handle}.

-doc "Generate a new surface (with no underlying bitmap).".
-spec surface() -> {ok, surface()}.
surface() ->
    {ok, <<Handle:32/little, _Rest/binary>>} = send_command({graphics, 16#2}),
    {ok, Handle}.

-doc "Release a surface handle.".
-spec surface_release(surface()) -> ok.
surface_release(Handle) ->
    {ok, <<>>} = send_command({graphics, 16#3}, <<Handle:32/little>>),
    ok.

-doc "Allocate a new bitmap (usually to back a surface).".
-spec bitmap(integer(), integer(), monochrome | color) -> {ok, bitmap()}.
bitmap(X, Y, Color) ->
    EncodedColor = case Color of
        monochrome -> 1;
        color -> 16
    end,
    Parameter = <<X:16/little, Y:16/little, EncodedColor:16/little>>,
    {ok, Reply} = send_command({graphics, 16#9}, Parameter),
    <<Handle:32/little, _Rest/binary>> = Reply,
    {ok, Handle}.

-doc "Release a bitmap.".
-spec bitmap_release(bitmap()) -> ok.
bitmap_release(Handle) ->
    {ok, <<>>} = send_command({graphics, 16#a}, <<Handle:32/little>>),
    ok.

-doc "Tether a surface to a backing bitmap.".
-spec surface_bitmap(surface(), bitmap()) -> ok.
surface_bitmap(DSHandle, BitmapHandle) ->
    Parameter = <<DSHandle:32/little, BitmapHandle:32/little>>,
    {ok, <<>>} = send_command({graphics, 16#22}, Parameter),
    ok.

-doc "Set the active font for a surface.".
-spec font(surface(), font()) -> ok.
font(Handle, Font) ->
    Parameter = <<Handle:32/little, Font:32/little>>,
    {ok, <<>>} = send_command({graphics, 16#20}, Parameter),
    ok.

-doc "Retrieve the current font of a surface.".
-spec font(surface()) -> {ok, font()}.
font(Handle) ->
    Parameter = <<Handle:32/little>>,
    {ok, <<Font:32/little>>} = send_command({graphics, 16#21}, Parameter),
    {ok, Font}.

%% TODO: This crashes with error code 0x81.  Don't know why or what that means.
% -doc "Write text on a surface at the current cursor position.".
% text(DSHandle, Text) ->
%     EncodedText = iolist_to_binary(Text),
%     TotalSize = size(EncodedText),
%     Parameter = <<DSHandle:32/little, TotalSize:16/little>>,
%     {ok, <<>>} = send_command({graphics, 16#40}, Parameter, EncodedText),
%     ok.

-doc "Write text on a surface at a given position.".
-spec text(surface(), iolist(), position()) -> ok.
text(DSHandle, Text, _Pos = {X, Y}) ->
    EncodedText = iolist_to_binary(Text),
    TotalSize = size(EncodedText),
    Parameter = <<DSHandle:32/little, X:16/little, Y:16/little, TotalSize:16/little>>,
    {ok, <<>>} = send_command({graphics, 16#41}, Parameter, EncodedText),
    ok.

-doc "Write the contents of one bitmap onto another.".
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
    {ok, <<>>} = send_command({graphics, 16#4a}, Parameter),
    ok.

-doc "Move the cursor position on a surface.".
-spec move_to(surface(), position()) -> ok.
move_to(Handle, _Position = {X, Y}) ->
    Parameter = <<Handle:32/little, X:16/little, Y:16/little>>,
    {ok, <<>>} = send_command({graphics, 16#33}, Parameter),
    ok.

-doc "Draw a line on a surface from the current cursor position to a new one.".
-spec line_to(surface(), position()) -> ok.
line_to(Handle, _Position = {X, Y}) ->
    Parameter = <<Handle:32/little, X:16/little, Y:16/little>>,
    {ok, <<>>} = send_command({graphics, 16#35}, Parameter),
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

-doc "Internal state of the MTDS driver.".
-record(state, {
    %% link with MTDS
    bus, buffer = << >>,
    %% windowing system
    listeners = #{}
}).
-type state() :: #state{
    %% link with MTDS
    bus :: grisp_spi:ref(),
    buffer :: binary(),
    %% windowing system
    %% TODO: I don't know what handle re-use guarantees MTDS provides.  If
    %%       Window A is released and a newly allocated Window B reoccupies its
    %%       handle, then events destined for Window A might be incorrectly
    %%       assigned to Window B.  We could handle this on the Erlang side by
    %%       translating through a dictionary of references.
    listeners :: #{window() => sets:set(pid())}
}.

-doc false.
%%% Initializes the bus object to form a driver state, but defers actual
%%% device initialization to a timeout (see handle_info/2) so as not to get in
%%% the way of any supervisor tree.
init([Interface]) ->
    %% NOTE: MTDS wants to put freq in 3.5–4 MHz, whereas GRiSP runs at 0.1 MHz.
    ok = grisp_devices:register(Interface, ?MODULE),
    {ok, Interface, 0}.

-doc false.
%%% Handles a call.
%% Process a command directive.
handle_call({command, {Class, Command}, Parameters, Packet}, _From, State) ->
    {ok, NewState, Reply} = command(State, {Class, Command}, Parameters, Packet),
    {reply, {ok, Reply}, NewState};
%% Process a touch enregistration.
handle_call({register, PID, Window}, _From, State) ->
    #state{listeners = Listeners} = State,
    NewListeners = case Listeners of
        #{Window := Registrants} ->
            Listeners#{Window => sets:add_element(PID, Registrants)};
        _ ->
            Listeners#{Window => sets:from_list([PID])}
    end,
    NewState = State#state{listeners = NewListeners},
    {reply, ok, NewState};
%% Poll for touch events, forward to registrants.
handle_call(poll_touch_events, _From, State = #state{}) ->
    {NewState, Replies} = poll_touch_events(State),
    lists:foreach(
        fun(Reply = {touch, WindowRef, _Action, _Position, _Speed, _Weight}) ->
            case NewState#state.listeners of
                #{WindowRef := Listeners} ->
                    lists:foreach(
                        fun(Listener) -> Listener ! Reply end,
                        sets:to_list(Listeners)
                    );
                _ -> ok
            end
        end,
        Replies
    ),
    {reply, ok, NewState}.

-doc false.
%%% Handles a cast.
handle_cast(_Request, State) ->
    {noreply, State}.

-doc false.
%%% Handles raw messages (and timeout events).
%% Complete initialization.
handle_info(timeout, Interface) when is_atom(Interface) ->
    Bus = grisp_spi:open(Interface),
    State = #state{bus = Bus},
    %% NOTE: Reference driver toggles the reset pin, but we don't have access.
    {ok, SyncedState} = sync(State),
    {ok, StartedState, _Reply} = command(SyncedState, {2#01, 16#2}),
    spawn_link(?MODULE, poll_loop, [self()]),  % set up touch poll
    {noreply, StartedState}.

-doc false.
%%% Handles a code update.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-doc false.
%%% Handles teardown.
terminate(_Reason, _State) ->
    ok.

-doc false.
%%% Prompts the main server to poll for touch events.
poll_loop(PID) ->
    receive
        stop -> ok
    after ?TOUCH_POLL_PERIOD ->
        gen_server:call(PID, poll_touch_events),
        poll_loop(PID)
    end.

%%%
%%% Underlying device comms
%%%

%% Various protocol constants.
-define(SPI_MODE, #{clock => {low, leading}}).

-define(HEADER_COMMAND, 2#01).
-define(HEADER_STATUS,  2#10).
-define(HEADER_DATA,    2#11).

-define(CONTROL_READ,  16#1).
-define(CONTROL_START, 16#2).
-define(CONTROL_SYNC,  16#3).

-define(CONTROL_READY,          16#20).
-define(CONTROL_BUSY,           16#21).
-define(CONTROL_DONE,           16#22).
-define(CONTROL_READY_FOR_DATA, 16#23).
-define(CONTROL_SYNCING,        16#25).

-define(STATUS_OK, 16#0).

-define(CLASS_UTILITY,  2#01).
-define(CLASS_GRAPHICS, 2#10).

-define(PACKET_OUT_MAX_SIZE, 512).

-type class() :: 0..3 .
-type command() :: 0..63 .

%% Device-side memory region constants.
%%
%% It would be nice to place these down with the other device-specific
%% constants, but these are needed to set up the default touch region.
-define(REGION_FONT,   16#C3000000).
-define(REGION_WINDOW, 16#C4000000).

-define(STOCK_FONT,   16#00300000).
-define(STOCK_WINDOW, 16#00400000).

-doc "Package a command sequence into a binary payload.".
-spec command_payload(integer(), integer(), binary()) -> binary().
command_payload(Class, Command, Parameters) ->
    Size = size(Parameters),
    <<
        ?HEADER_COMMAND:2,
        Class:6,
        Command:8,
        Size:16/little,
        Parameters/binary
    >>.

-doc "Send a command to MTDS and confirm delivery.".
-spec command(state(), {class(), command()}, binary(), binary()) ->
    {ok, state(), binary()} | {error, any()}.
command(State, {Class, Command}) ->
    command(State, {Class, Command}, << >>).
command(State, {Class, Command}, Parameters) ->
    command(State, {Class, Command}, Parameters, << >>).
command(State, {Class, Command}, Parameters, DataPacket) ->
    Payload = command_payload(Class, Command, Parameters),
    CommandState = transfer(State, Payload),
    BusyState = drop(wait_until(CommandState, <<?CONTROL_BUSY>>)),

    {ok, PacketState} = send_data(BusyState, Command, DataPacket),

    DoneState = drop(wait_until(PacketState, <<?CONTROL_DONE>>)),

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

-doc """
Write a data packet.  Generally follows a command and allows for data of
flexible length.
""".
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
        16#1:6,  % data out
        Command:8,
        ChunkSize:16/little,
        Chunk/binary
    >>),
    send_data(TransferredState, Command, Rest).

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

-doc "Drains the MTDS message queue of touch events.".
%% TODO: Could limit the recursion here if it interferes with draw commands.
-spec poll_touch_events(state()) -> {state(), [touch_event()]}.
poll_touch_events(State) ->
    {ok, QueriedState, Status} = command(State, {?CLASS_UTILITY, 16#11}),
    case Status of
        <<0:32/little>> -> {QueriedState, []};
        _ ->
            {ok, PolledState, RawResult} = command(
                QueriedState, {?CLASS_UTILITY, 16#14}
            ),
            Result = parse_touch_event(RawResult),
            {UltimateState, Results} = poll_touch_events(PolledState),
            {UltimateState, [Result | Results]}
    end.

-doc "Parses an individual MTDS message into a touch_event() object.".
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
    %% io:format("Out: ~w; In: ~w~n", [Binary, Response]),  % useful debug
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

-doc "Shorthand to send a command directive to the MTDS driver.".
-spec send_command({class_atom(), command()}, binary(), binary()) ->
    {ok, binary()}.
send_command(Selector) ->
    send_command(Selector, << >>).
send_command(Selector, Parameter) ->
    send_command(Selector, Parameter, << >>).
send_command({ClassAtom, Command}, Parameter, DataPacket) ->
    #device{pid = PID} = grisp_devices:default(?MODULE),
    Class = class_from_atom(ClassAtom),
    gen_server:call(PID, {command, {Class, Command}, Parameter, DataPacket}).

-doc "Convert a human-readable class atom into a class() instance.".
-spec class_from_atom(class_atom()) -> class().
class_from_atom(utility)  -> ?CLASS_UTILITY;
class_from_atom(graphics) -> ?CLASS_GRAPHICS.

-doc "Convert a human-readable font atom to a font() instance.".
-spec stock_font(stock_font()) -> font().
stock_font(Font) ->
    case Font of
        console     -> ?REGION_FONT + ?STOCK_FONT + 0
        %% NOTE: These appear to be unimplemented.
        % ;
        % extra_small -> ?REGION_FONT + ?STOCK_FONT + 1;
        % small       -> ?REGION_FONT + ?STOCK_FONT + 2;
        % medium      -> ?REGION_FONT + ?STOCK_FONT + 3;
        % large       -> ?REGION_FONT + ?STOCK_FONT + 4;
        % extra_large -> ?REGION_FONT + ?STOCK_FONT + 5
    end.
