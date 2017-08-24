-module(grisp_spi).

-behavior(gen_server).

% API
-export([start_link/1]).
-export([send_recv/4]).
-export([send_recv/2]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- Records -------------------------------------------------------------------

-record(state, {driver}).

%--- Macros --------------------------------------------------------------------

%% Make sure to keep this at sync with the #define RES_MAX_SIZE
%% in grisp_spi_drv.c
-define(RES_MAX_SIZE, 256).

%--- API -----------------------------------------------------------------------

% @private
start_link(DriverMod) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, DriverMod, []).

send_recv(Slot, Req, Skip, Pad) ->
    P = binary:copy(<<16#ff>>, Pad),
    Resp = send_recv(Slot, <<Req/binary, P/binary>>),
    <<_:Skip/binary, R/binary>> = Resp,
    R.

send_recv(Slot, Req) when byte_size(Req) < ?RES_MAX_SIZE ->
    gen_server:call(?MODULE, {send_recv, Slot, Req}).

%--- Callbacks -----------------------------------------------------------------

% @private
init(DriverMod) ->
    Ref = DriverMod:open(),
    {ok, #state{driver = {DriverMod, Ref}}}.

% @private
handle_call({send_recv, Slot, Req}, _From, State) ->
    {DriverMod, Ref} = State#state.driver,
    Resp = DriverMod:command(Ref, Slot, Req),
    {reply, Resp, State}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.
