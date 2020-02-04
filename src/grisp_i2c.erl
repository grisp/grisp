%% @doc I2C driver API.
%%
%% This API is based on the
%% <a href="https://docs.rtems.org/doxygen/branches/master/group__I2CLinux.html">
%% Linux I2C User Space API
%% </a>.
%% For a description of the I2C protocol see
%% [https://www.kernel.org/doc/Documentation/i2c/i2c-protocol].
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

%% @doc Communicate with the I2C bus.
%%
%% The first entry in the list has to be the address as an integer.
%% The entry `{sleep, Time}' can be used to add delays between messages.
%% For possible flags see the
%% <a href="https://docs.rtems.org/doxygen/branches/master/group__I2CLinux.html">
%% Linux I2C User Space API
%% </a>.
%%
%% Sending a read message `{read, Length}' the `I2C_M_RD' flag will be set
%% automatcally. The `Length' is the number of bytes to be read.
%%
%% === Example ===
%% A read message like
%% ```
%% 1> grisp_i2c:msgs([16#40, {read, 2}]).
%% '''
%% will send an I2C message with
%% ```
%% addr  = 16#40
%% flags = I2C_M_RD
%% len   = 2
%% '''
%% using the notation from
%% [https://www.kernel.org/doc/Documentation/i2c/i2c-protocol]
%% this message will look like:
%% ```
%% S 16#40 Rd [A] [Data] A [Data] NA P
%% '''
%%
%% A write message like
%% ```
%% 2> grisp_i2c:msgs([16#40, {write, <<16#02>>}]).
%% '''
%% will send an I2C message with
%% ```
%% addr  = 16#40
%% flags = 16#0000
%% len   = 1
%% *buf  = 16#02
%% '''
%% using the notation from
%% [https://www.kernel.org/doc/Documentation/i2c/i2c-protocol]
%% this message will look like:
%% ```
%% S 16#40 Wr [A] 16#02 P
%% '''
-spec msgs([Adr::integer()
            | {sleep, Time::integer()}
            | {write, Data::binary()}
            | {write, Data::binary(), Flags::integer()}
            | {read, Length::integer()}
            | {read, Length::integer(), Flags::integer()}])
    -> LastResponse::any().
msgs([Adr | Msgs]) ->
    EncodedMsgs = do_msgs(Adr, Msgs),
    gen_server:call(?MODULE, {msgs, EncodedMsgs}).


%--- Callbacks -----------------------------------------------------------------

% @private
init(DriverMod) ->
    Ref = DriverMod:open(),
    {ok, #state{driver = {DriverMod, Ref}}}.

% @private
handle_call({msgs, EncodedMsgs}, _From, State) ->
    {DriverMod, Ref} = State#state.driver,
    RespList = [maybe_send_msgs(Msg, DriverMod, Ref) || Msg <- EncodedMsgs],
    LastResp = lists:last(RespList),
    {reply, LastResp, State}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------

do_msgs(Adr, Msgs) ->
    do_msgs(Adr, Msgs, [], []).

do_msgs(_Adr, [], [], ReversedEncodedMsgs) ->
    lists:reverse(ReversedEncodedMsgs);
do_msgs(Adr, [], ReversedToEncode, ReversedEncodedMsgs) ->
    ToEncode = lists:reverse(ReversedToEncode),
    EncodedMsgs = encode_msgs([Adr | ToEncode]),
    NewReversedEncodedMsgs = [EncodedMsgs | ReversedEncodedMsgs],
    do_msgs(Adr, [], [], NewReversedEncodedMsgs);
do_msgs(Adr, [{sleep, Time} | Rest], ReversedToEncode, ReversedEncodedMsgs) ->
    ToEncode = lists:reverse(ReversedToEncode),
    EncodedMsgs = encode_msgs([Adr | ToEncode]),
    NewReversedEncodedMsgs = [EncodedMsgs | ReversedEncodedMsgs],
    NewReversedEncodedMsgsWithSleep = [{sleep, Time} | NewReversedEncodedMsgs],
    do_msgs(Adr, Rest, [], NewReversedEncodedMsgsWithSleep);
do_msgs(Adr, [Msg | Rest], ReversedToEncode, ReversedEncodedMsgs) ->
    NewReversedToEncode = [Msg | ReversedToEncode],
    do_msgs(Adr, Rest, NewReversedToEncode, ReversedEncodedMsgs).

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

maybe_send_msgs({sleep, Time}, _DriverMod, _Ref) ->
    timer:sleep(Time);
maybe_send_msgs(Msg, DriverMod, Ref) ->
    DriverMod:command(Ref, Msg).
