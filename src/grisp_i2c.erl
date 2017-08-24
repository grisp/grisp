-module(grisp_i2c).

-behavior(gen_server).

-include("grisp_i2c.hrl").

% API
-export([start_link/1]).
-export([msgs/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- Records -------------------------------------------------------------------

-record(state, {driver}).

%--- API -----------------------------------------------------------------------

% @private
start_link(DriverMod) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, DriverMod, []).

msgs(Msgs) ->
    Enc_msgs = encode_msgs(Msgs),
    gen_server:call(?MODULE, {msgs, Enc_msgs}).

%--- Callbacks -----------------------------------------------------------------

% @private
init(DriverMod) ->
    Ref = DriverMod:open(),
    {ok, #state{driver = {DriverMod, Ref}}}.

% @private
handle_call({msgs, Enc_msgs}, _From, State) ->
    {DriverMod, Ref} = State#state.driver,
    Resp = DriverMod:command(Ref, Enc_msgs),
    {reply, Resp, State}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------

encode_msgs(Msgs) ->
    encode_msgs(Msgs, undefined, <<>>, <<>>).

encode_msgs([Adr|Rest], _, W, M) when is_integer(Adr) ->
    encode_msgs(Rest, Adr, W, M);
encode_msgs([{Cmd, Data}|Rest], Adr, W, M) ->
    encode_msgs([{Cmd, Data, 0}|Rest], Adr, W, M);
encode_msgs([{write, Data, Flags}|Rest], Adr, W, M) ->
    Offset = byte_size(W),
    Len = byte_size(Data),
    encode_msgs(Rest, Adr, <<W/binary, Data/binary>>,
		<<M/binary, Adr:16, Flags:16, Len:16, Offset:16>>);
encode_msgs([{read, Len, Flags}|Rest], Adr, W, M) when is_integer(Len) ->
    F = Flags bor ?I2C_M_RD,
    encode_msgs(Rest, Adr, W, <<M/binary, Adr:16, F:16, Len:16, 0:16>>);
encode_msgs([], _Adr, W, M) when byte_size(M) rem 8 =:= 0 ->
    Data_len = byte_size(W),
    Msg_count = byte_size(M) div 8,
    <<Data_len:16, W/binary, Msg_count:16, M/binary>>.
