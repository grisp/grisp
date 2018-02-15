-module(grisp_onewire).
% Chip: DS2482-100 Single-Channel 1-Wire Master
% https://datasheets.maximintegrated.com/en/ds/DS2482-100.pdf

-behaviour(gen_server).

% API
-export([
    start_link/0,
    transaction/1,
    reset/0,
    write_config/1,
    detect/0,
    bus_reset/0,
    write_byte/1,
    write_triplet/1,
    read_byte/0,
    search/0
]).

% Callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-include("grisp_i2c.hrl").

-define(DS2482_I2C_ADR, 16#18).
-define(CMD_DRST, 16#f0).
-define(CMD_WCFG, 16#d2).
-define(CMD_1WRS, 16#b4).
-define(CMD_1WWB, 16#a5).
-define(CMD_1WRB, 16#96).
-define(CMD_1WT,  16#78).
-define(CMD_SRP,  16#e1).

-define(TRANSACTION_KEY, '$onewire_transaction_token').
-define(TRANSACTION_TOKEN, 4435846174457203). % Random token

%--- API -----------------------------------------------------------------------

% @private
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

transaction(Fun) when is_function(Fun) ->
    case gen_server:call(?MODULE, {transaction, Fun}) of
        {result, Result} ->
            Result;
        {exception, Class, Reason, Stacktrace} ->
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% Spec: | S | AD,0 ‖ A ‖ DRST ‖ A ‖ Sr | AD,1 ‖ A | <byte> ‖ A\ | P |
reset() ->
    assert_transaction(),
    <<Status:8>> = grisp_i2c:msgs([?DS2482_I2C_ADR,
                                   {write, <<?CMD_DRST>>},
                                   {read, 1, ?I2C_M_NO_RD_ACK}]),
    case Status band 16#f7 of
        16#10 -> ok;
        Any -> error({invalid_status, Any})
    end.

%% Spec: | S | AD,0 ‖ A ‖ WCFG ‖ A ‖ <byte> ‖ A ‖ Sr | AD,1 ‖ A | <byte> ‖ A\ | P |
write_config(Conf) when is_list(Conf) ->
    write_config(lists:foldl(fun(X, A) -> A bor map_config(X) end, 0, Conf));
write_config(Conf) when is_integer(Conf) ->
    assert_transaction(),
    Val = (bnot(Conf) bsl 4) bor Conf,
    case grisp_i2c:msgs([?DS2482_I2C_ADR,
                         {write, <<?CMD_WCFG, Val>>},
                         {read, 1, ?I2C_M_NO_RD_ACK}]) of
        <<Conf:8>> ->
            ok;
        Any -> error({read_back_config, Any, Val})
    end.

detect() ->
    assert_transaction(),
    reset(),
    write_config([apu]).

bus_reset() ->
    assert_transaction(),
    grisp_i2c:msgs([?DS2482_I2C_ADR, {write, <<?CMD_1WRS>>}]),
    timer:sleep(1),
    check_status(grisp_i2c:msgs([?DS2482_I2C_ADR,
                                 {read, 1, ?I2C_M_NO_RD_ACK}])).

write_byte(Byte) ->
    assert_transaction(),
    grisp_i2c:msgs([?DS2482_I2C_ADR, {write, <<?CMD_1WWB, Byte>>}]),
    timer:sleep(1).

read_byte() ->
    assert_transaction(),
    grisp_i2c:msgs([?DS2482_I2C_ADR,
                    {write, <<?CMD_1WRB>>}]),
    timer:sleep(1),
    grisp_i2c:msgs([?DS2482_I2C_ADR,
                    {write, <<?CMD_SRP, 16#e1>>},
                    {read, 1, ?I2C_M_NO_RD_ACK}]).

write_triplet(Dir) ->
    assert_transaction(),
    grisp_i2c:msgs([?DS2482_I2C_ADR, {write, <<?CMD_1WT, Dir>>}]),
    timer:sleep(1),
    <<D:1, T:1, S:1, _:5>> = grisp_i2c:msgs([?DS2482_I2C_ADR,
                                             {read, 1, ?I2C_M_NO_RD_ACK}]),
    {D, T, S}.

search() ->
    assert_transaction(),
    bus_reset(),
    write_byte(16#f0),
    L = lists:reverse([ element(1, write_triplet(0)) || _ <- lists:seq(1,64) ]),
    lists:reverse([ Y || <<Y:8>> <= << <<X:1>> || X <- L >> ]).

%--- Callbacks -----------------------------------------------------------------

% @private
init([]) ->
    put(?TRANSACTION_KEY, ?TRANSACTION_TOKEN),
    {ok, []}.

% @private
handle_call({transaction, Fun}, _From, State) ->
    Reply = try
        {result, Fun()}
    catch
        Class:Reason ->
            {exception, Class, Reason, erlang:get_stacktrace()}
    end,
    {reply, Reply, State}.

% @private
handle_cast(Cast, _State) -> error({unknown_cast, Cast}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

%--- Internal ------------------------------------------------------------------

assert_transaction() ->
    case get('$onewire_transaction_token') of
        ?TRANSACTION_TOKEN -> ok;
        _                  -> error(no_transaction)
    end.

map_config(apu)       -> 1;
map_config(spu)       -> 4;
map_config(overdrive) -> 8.

check_status(<<_:5, Sd:1, Ppd:1, 0:1>>) ->
    case {Sd, Ppd} of
        {0, 0} -> nothing_present;
        {0, 1} -> presence_detected;
        {1, _} -> short_detected
    end;
check_status(<<_:7, 1:1>>) ->
    error(bus_reset_busy).
