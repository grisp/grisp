-module(pmod_gyro).

-behavior(gen_server).

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
start_link(Slot, _Opts) ->
    gen_server:start_link(?MODULE, Slot, []).

read() ->
    Dev = grisp_devices:default(?MODULE),
    case gen_server:call(Dev#device.pid, read) of
        {error, Reason} -> error(Reason);
        Result          -> Result
    end.

%--- Callbacks -----------------------------------------------------------------

% @private
init(Slot) ->
    verify_device(Slot),
    <<>> = grisp_spi:send_recv(Slot, ?SPI_MODE, <<?RW_WRITE:1, ?MS_SAME:1, ?CTRL_REG1:6, 2#00001111:8>>, 2, 0),
    grisp_devices:register(Slot, ?MODULE),
    {ok, Slot}.

% @private
handle_call(read, _From, Slot) ->
    <<Reply:16/signed-little>> = grisp_spi:send_recv(Slot, ?SPI_MODE, <<?RW_READ:1, ?MS_INCR:1, ?OUT_X_L:6>>, 1, 2),
    {reply, Reply, Slot};
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
