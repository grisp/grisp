%% -----------------------------------------------------------------------------
%% @doc
%% <a href="https://reference.digilentinc.com/reference/pmod/pmodgyro/reference-manual">
%% PmodGYRO
%% </a>
%% module that gets the gyroscopes data via SPI.
%%
%% Start the server with
%%  ```
%%  1> grisp:add_device(spi1, pmod_gyro).
%%  '''
%% @end
%% -----------------------------------------------------------------------------
-module(pmod_gyro).

-behaviour(gen_server).

-include("grisp.hrl").
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

-define(SPI_MODE, #{cpol => low, cpha => leading}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot, Opts) ->
    gen_server:start_link(?MODULE, [Slot, Opts], []).

%% @doc Read the gyroscopes X, Y and Z values in degrees per second.
%%
%% === Example ===
%% ```
%%  2> pmod_gyro:read().
%%  {249.28279313922965,-26.078862235243843,12.764756149667337}
%% '''
-spec read() -> {X::float(), Y::float(), Z::float()}.
read() ->
    Dev = grisp_devices:default(?MODULE),
    case gen_server:call(Dev#device.pid, read) of
        {error, Reason} -> error(Reason);
        Result          -> Result
    end.

%--- Callbacks -----------------------------------------------------------------

% @private
init([Slot, Opts]) ->
    verify_device(Slot),
    Res = maps:get(resolution, Opts, 250),
    ResOpt = case Res of
                 250   -> 2#00000000;
                 500   -> 2#00010000;
                 2000  -> 2#00100000;
                 _     -> error({invalid_option, Res})
             end,
    %% set the resolution
    <<>> = grisp_spi:send_recv(Slot, ?SPI_MODE, <<?RW_WRITE:1, ?MS_SAME:1, ?CTRL_REG4:6, ResOpt:8>>, 2, 0),
    %% enable the device and axis sensors
    <<>> = grisp_spi:send_recv(Slot, ?SPI_MODE, <<?RW_WRITE:1, ?MS_SAME:1, ?CTRL_REG1:6, 2#00001111:8>>, 2, 0),
    grisp_devices:register(Slot, ?MODULE),
    {ok, #{slot => Slot, unit_degree => (32766 / Res)}}.

% @private
handle_call(read, _From, #{slot := Slot, unit_degree := UnitDeg} = State) ->
    <<X:16/signed-little,
      Y:16/signed-little,
      Z:16/signed-little>> = grisp_spi:send_recv(Slot, ?SPI_MODE,
                                                 <<?RW_READ:1, ?MS_INCR:1, ?OUT_X_L:6>>,
                                                 1, 6),
    {reply, {X / UnitDeg, Y / UnitDeg, Z / UnitDeg}, State};
handle_call(Request, From, _State) ->
    error({unknown_request, Request, From}).

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------

verify_device(Slot) ->
    case grisp_spi:send_recv(Slot, ?SPI_MODE, <<?RW_READ:1, ?MS_SAME:1, ?WHO_AM_I:6>>, 1, 1) of
        <<?DEVID>> -> ok;
        Other      -> error({device_mismatch, {who_am_i, Other}})
    end.
