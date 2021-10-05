% @doc SPI driver API.
%
% @type spi_slot() = spi1 | spi2
% @type spi_mode() = #{cpol => low,  cpha => leading} |
%                    #{cpol => low,  cpha => trailing} |
%                    #{cpol => high, cpha => leading} |
%                    #{cpol => high, cpha => trailing}
% @end
-module(grisp_spi).

-behavior(gen_server).

% API
-export([start_link/1]).
-export([send_recv/5]).
-export([send_recv/3]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

% TODO: Enable custom pin pulling for reading SPI
% TODO: Implement multi-read interface for reading multiple registers
%       (including custom pin pulling per register?)

%--- Records -------------------------------------------------------------------

-record(state, {driver}).

%--- Macros --------------------------------------------------------------------

% Make sure to keep this at sync with the #define RES_MAX_SIZE
% in grisp_spi_drv.c
-define(RES_MAX_SIZE, 256).

%--- API -----------------------------------------------------------------------

% @private
start_link(DriverMod) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, DriverMod, []).

% @doc Send request and receive response.
%
% The send data is `Req' appended with `Pad' times `<<16#ff>>', i.e, `Pad * 8'
% bits of value 1.
%
% The parameter `Skip' denotes the number of bytes skipped at the beginning of
% the received response.
%
% @spec send_recv(spi_slot(), spi_mode(), binary(), integer(), integer()) ->
% Response::binary()
send_recv(Slot, Mode, Req, Skip, Pad) ->
    P = binary:copy(<<16#ff>>, Pad),
    Resp = send_recv(Slot, Mode, <<Req/binary, P/binary>>),
    <<_:Skip/binary, R/binary>> = Resp,
    R.

% @doc Send request and receive response.
%
% @spec send_recv(spi_slot(), spi_mode(), binary()) -> Response::binary()
send_recv(Slot, Mode, Req) when byte_size(Req) < ?RES_MAX_SIZE ->
    gen_server:call(?MODULE, {send_recv, Slot, Mode, Req}).

%--- Callbacks -----------------------------------------------------------------

% @private
init(DriverMod) ->
    Ref = DriverMod:open_nif(),
    {ok, #state{driver = {DriverMod, Ref}}}.

% @private
handle_call({send_recv, Slot, Mode, Req}, _From, State) ->
    {DriverMod, Ref} = State#state.driver,
    Resp = DriverMod:ioctl(Ref, Slot, Mode, Req),
    {reply, Resp, State}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.
