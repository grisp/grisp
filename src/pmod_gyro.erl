-module(pmod_gyro).
-moduledoc """
[PmodGYRO](https://reference.digilentinc.com/reference/pmod/pmodgyro/reference-manual)
module that gets the gyroscopes data via SPI.

Start the driver with
```
1> grisp:add_device(spi1, pmod_gyro).
```
""".

-behaviour(gen_server).

-include("grisp_internal.hrl").
-include("pmod_gyro.hrl").

% API
-export([start_link/2]).
-export([read/0]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(SPI_MODE, #{clock => {low, leading}}).

%--- API -----------------------------------------------------------------------

-doc(false).
start_link(Slot, Opts) ->
    gen_server:start_link(?MODULE, [Slot, Opts], []).

-doc """
Read the gyroscopes X, Y and Z values in degrees per second.

## Example
```
 2> pmod_gyro:read().
 {249.28279313922965,-26.078862235243843,12.764756149667337}
```
""".
-spec read() -> {X::float(), Y::float(), Z::float()}.
read() ->
    Dev = grisp_devices:default(?MODULE),
    case gen_server:call(Dev#device.pid, read) of
        {error, Reason} -> error(Reason);
        Result          -> Result
    end.

%--- Callbacks -----------------------------------------------------------------

-doc(false).
init([Slot, Opts]) ->
    Bus = grisp_spi:open(Slot),
    verify_device(Bus),
    Res = maps:get(resolution, Opts, 250),
    ResOpt = case Res of
                 250   -> 2#00000000;
                 500   -> 2#00010000;
                 2000  -> 2#00100000;
                 _     -> error({invalid_option, Res})
             end,
    % Set the resolution
    <<>> = write(Bus, ?CTRL_REG4, ResOpt),
    % Enable the device and axis sensors
    <<>> = write(Bus, ?CTRL_REG1, 2#00001111),
    grisp_devices:register(Slot, ?MODULE),
    {ok, #{bus => Bus, unit_degree => (32766 / Res)}}.

-doc(false).
handle_call(read, _From, #{bus := Bus, unit_degree := UnitDeg} = State) ->
    <<X:16/signed-little, Y:16/signed-little, Z:16/signed-little>>
        = read(Bus, ?OUT_X_L, 6),
    {reply, {X / UnitDeg, Y / UnitDeg, Z / UnitDeg}, State};
handle_call(Request, From, _State) ->
    error({unknown_request, Request, From}).

-doc(false).
handle_cast(Request, _State) -> error({unknown_cast, Request}).

-doc(false).
handle_info(Info, _State) -> error({unknown_info, Info}).

-doc(false).
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-doc(false).
terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------

verify_device(Bus) ->
    case read(Bus, ?WHO_AM_I, 1) of
        <<?DEVID>> -> ok;
        Other -> error({device_mismatch, {who_am_i, Other}})
    end.

read(Bus, Reg, Pad) ->
    transfer(Bus, {?SPI_MODE, <<?RW_READ:1, ?MS_INCR:1, Reg:6>>, 1, Pad}).

write(Bus, Reg, Value) ->
    transfer(Bus, {?SPI_MODE, <<?RW_WRITE:1, ?MS_SAME:1, Reg:6, Value:8>>, 2, 0}).

transfer(Bus, Message) ->
    [Response] = grisp_spi:transfer(Bus, [Message]),
    Response.
