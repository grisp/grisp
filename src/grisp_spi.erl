-module(grisp_spi).

-behaviour(gen_server).

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
-define(PORT_COMMAND_TIMEOUT, 1000).

%--- API -----------------------------------------------------------------------

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

init(DriverMod) ->
    Ref = DriverMod:open(),
    {ok, #state{driver = {DriverMod, Ref}}}.

handle_call({send_recv, Slot, Req}, _From, State) ->
    {DriverMod, Ref} = State#state.driver,
    DriverMod:command(Ref, Slot, Req),
    receive
        {Ref, {data, Resp}} ->
            {reply, Resp, State}
    after ?PORT_COMMAND_TIMEOUT ->
            exit(timeout)
    end;
handle_call(Request, From, _State) ->
    error({unknown_request, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info(Info, _State) -> error({unknown_info, Info}).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
