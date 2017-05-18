-module(grisp_spi).

-behaviour(gen_server).

% API
-export([start_link/0]).
-export([send_recv/3]).
-export([send_recv/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- Records -------------------------------------------------------------------

-record(state, {port}).

%--- Macros --------------------------------------------------------------------

%% Make sure to keep this at sync with the #define RES_MAX_SIZE
%% in grisp_spi_drv.c
-define(RES_MAX_SIZE, 256).
-define(PORT_COMMAND_TIMEOUT, 1000).

%--- API -----------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

send_recv(Req, Skip, Pad) ->
    P = binary:copy(<<16#ff>>, Pad),
    {ok, Resp} = send_recv(<<Req/binary, P/binary>>),
    <<_:Skip/binary, R/binary>> = Resp,
    {ok, R}.

send_recv(Req) when byte_size(Req) < ?RES_MAX_SIZE ->
    gen_server:call(?MODULE, {send_recv, Req}).

%--- Callbacks -----------------------------------------------------------------

init(undefined) ->
    Port = undefined,
    % Port = open_port({spawn, "grisp_spi_drv"}, [binary]),  % FIXME: Use spawn_driver here?
    {ok, #state{port = Port}}.

handle_call({send_recv, Req}, _From, #state{port = Port} = State) ->
    Port ! {self(), {command, Req}},
    receive
        {Port, {data, Resp}} ->
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
