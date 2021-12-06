% @doc GRiSP RTEMS functions.
%
% NIF mappings to RTEMS functions.
-module(grisp_rtems).

-include("grisp_nif.hrl").

% API
-export([clock_get_ticks_per_second/0]).
-export([clock_get_ticks_since_boot/0]).
-export([clock_get_tod/0]).
-export([clock_set/1]).
-export([unmount/1]).
-export([write_file_to_device/2]).
-export([write_file_to_device/4]).
-export([pwrite/3]).

% Callbacks
-export([on_load/0]).
-on_load(on_load/0).

-define(DEFAULT_READ_CHUNK_SIZE,  4 * 1024 * 1024). %% 4MiB
-define(DEFAULT_WRITE_CHUNK_SIZE, 4 * 1024 * 1024). %% 4MiB

%--- Types ---------------------------------------------------------------------

-type time_of_day() :: {calendar:datetime(), Ticks::non_neg_integer()}.
-export_type([time_of_day/0]).

%--- API -----------------------------------------------------------------------

% @doc Gets the number of clock ticks per second configured for the
% application.
%
% <b>Reference:</b> <a href="https://docs.rtems.org/branches/master/c-user/clock/directives.html#rtems-clock-get-ticks-per-second">rtems_clock_get_ticks_per_second</a>
-spec clock_get_ticks_per_second() -> integer().
clock_get_ticks_per_second() -> erlang:nif_error("NIF library not loaded").

% @doc Gets the number of clock ticks since some time point during the system
% initialization or the last overflow of the clock tick counter.
%
% <b>Reference:</b> <a href="https://docs.rtems.org/branches/master/c-user/clock/directives.html#rtems-clock-get-ticks-since-boot">rtems_clock_get_ticks_since_boot</a>
-spec clock_get_ticks_since_boot() -> integer().
clock_get_ticks_since_boot() -> erlang:nif_error("NIF library not loaded").

% @doc Gets the time of day associated with the current `CLOCK_REALTIME'.
%
% <b>Reference:</b> <a href="https://docs.rtems.org/branches/master/c-user/clock/directives.html#rtems-clock-get-tod">rtems_clock_get_tod</a>
-spec clock_get_tod() -> time_of_day().
clock_get_tod() ->
    {Year, Month, Day, Hour, Minute, Second, Ticks} = clock_get_tod_nif(),
    {{{Year, Month, Day}, {Hour, Minute, Second}}, Ticks}.

% @doc Sets the `CLOCK_REALTIME' to the time of day.
%
% <b>Reference:</b> <a href="https://docs.rtems.org/branches/master/c-user/clock/directives.html#rtems-clock-set">rtems_clock_set</a>
-spec clock_set(time_of_day()) -> integer().
clock_set({{{Year, Month, Day}, {Hour, Minute, Second}}, Ticks}) ->
    clock_set_nif({Year, Month, Day, Hour, Minute, Second, Ticks}).

% @doc Unmounts the file system instance at the specified mount path.
%
% <b>Reference:</b> <a href="https://docs.rtems.org/doxygen/branches/master/group__FileSystemTypesAndMount.html#ga4c8f87fc991f94992e0da1f87243f9e0">rtems_unmount</a>
-spec unmount(iodata()) -> ok | {error, list()}.
unmount(Path) ->
    unmount_nif([Path, 0]).

% @private
-spec write_file_to_device(string(), string())
    -> {ok, integer()} | {error, list()} | {error, atom(), term()}.
write_file_to_device(FilePath, DevicePath) ->
    write_file_to_device(FilePath, DevicePath, ?DEFAULT_READ_CHUNK_SIZE, ?DEFAULT_WRITE_CHUNK_SIZE).

% @private
-spec write_file_to_device(string(), string(), non_neg_integer(), non_neg_integer())
    -> {ok, integer()} | {error, list()} | {error, atom(), term()}.
write_file_to_device(FilePath, DevicePath, ReadChunkSize, WriteChunkSize) ->
    case file:open(FilePath, [read, raw, binary]) of
        {ok, Fd} ->
            try
                read_write_loop(Fd, DevicePath, ReadChunkSize, WriteChunkSize, 0)
            after
                file:close(Fd)
            end;
        Error ->
            Error
    end.

% @doc Perform a raw `pwrite' syscall to a device.
%
% The function is approximately equivalent to the C code:
% ```
% fd = open(device_path, O_RDWR);
% pwrite(fd, buffer.data, buffer.size, offset);
% '''
%
% <b>Reference:</b> <a href="https://linux.die.net/man/2/pwrite">pwrite</a>
-spec pwrite(binary() | iolist(), binary() | iolist(), integer()) -> {ok, integer()} | {error, atom(), list()}.
pwrite(DevicePath, Buffer, Offset) ->
    pwrite_nif([DevicePath, 0], [Buffer], Offset).

%--- Callbacks -----------------------------------------------------------------

% @private
on_load() -> ok = erlang:load_nif(atom_to_list(?MODULE), 0).

%--- Internal ------------------------------------------------------------------

clock_set_nif(_TimeOfDay) -> ?NIF_STUB.

clock_get_tod_nif() -> ?NIF_STUB.

unmount_nif(_Path) -> ?NIF_STUB.

pwrite_nif(_DevicePath, _Buffer, _Offset) -> ?NIF_STUB.


read_write_loop(Fd, DevicePath, ReadChunkSize, WriteChunkSize, BytesReadTotal) ->
    case file:pread(Fd, BytesReadTotal, ReadChunkSize) of
        {ok, ReadChunk} ->
            case write_loop(DevicePath, ReadChunk, WriteChunkSize, BytesReadTotal, 0) of
                {ok, ChunkBytesWritten} ->
                    read_write_loop(Fd, DevicePath, ReadChunkSize, WriteChunkSize, BytesReadTotal + ChunkBytesWritten);
                Error ->
                    {error, BytesReadTotal, Error}
            end;
        eof ->
            {ok, BytesReadTotal};
        Error ->
            {error, BytesReadTotal, Error}
    end.

write_loop(DevicePath, Chunk, WriteChunkSize, Offset, ChunkBytesWritten) ->
    case byte_size(Chunk) =< WriteChunkSize of
        true ->
            case pwrite(DevicePath, Chunk, Offset) of
                {ok, BytesWritten} ->
                    {ok, ChunkBytesWritten + BytesWritten};
                Error ->
                    Error
            end;
        false ->
            <<WriteChunk:WriteChunkSize/binary, ChunkRest/binary>> = Chunk,
            case pwrite(DevicePath, WriteChunk, Offset) of
                {ok, BytesWritten} ->
                    write_loop(DevicePath, ChunkRest, WriteChunkSize, Offset + BytesWritten, ChunkBytesWritten + BytesWritten);
                Error ->
                    Error
            end
    end.
