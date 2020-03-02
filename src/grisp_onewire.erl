% @doc Driver API for the
% <a href="https://datasheets.maximintegrated.com/en/ds/DS2482-100.pdf">
% DS2482-100 Single-Channel 1-Wire Master
% </a>.
% @end
-module(grisp_onewire).

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
-include("grisp.hrl").

-define(DS2482_I2C_ADR, 16#18).
-define(CMD_DRST, 16#f0).
-define(CMD_WCFG, 16#d2).
-define(CMD_1WRS, 16#b4).
-define(CMD_1WWB, 16#a5).
-define(CMD_1WRB, 16#96).
-define(CMD_1WT,  16#78).
-define(CMD_SRP,  16#e1).

-define(TRANSACTION_TIMEOUT, 12000).
-define(TRANSACTION_KEY, '$onewire_transaction_token').
-define(TRANSACTION_TOKEN, 4435846174457203). % Random token

%--- API -----------------------------------------------------------------------

% @private
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @doc Run a 1-Wire transaction.
%
% Use this function to make sure that there is only one process running on the
% 1-Wire.
%
% === Example ===
% ```
%  2> grisp_onewire:transaction(fun() ->
%                                  presence_detected = grisp_onewire:bus_reset(),
%                                  grisp_onewire:write_byte(16#cc)
%                               end).
%  ok
% '''
-spec transaction(fun()) -> any().
transaction(Fun) when is_function(Fun) ->
    case gen_server:call(?MODULE, {transaction, Fun}, ?TRANSACTION_TIMEOUT) of
        {result, Result} ->
            Result;
        {exception, Class, Reason, Stacktrace} ->
            erlang:raise(Class, Reason, Stacktrace)
    end.

% @doc <q>Performs a global reset of device state machine logic. Terminates any
% ongoing 1-Wire communication.</q>
%
% Command code: f0h.
%
% This function can only be used inside of a {@link transaction/1} call.
% @end
% I2C messages to 1-Wire Master:
% | S | AD,0 ‖ A ‖ DRST ‖ A ‖ Sr | AD,1 ‖ A | <byte> ‖ A\ | P |
-spec reset() -> 'ok'.
reset() ->
    assert_transaction(),
    <<Status:8>> = grisp_i2c:msgs([?DS2482_I2C_ADR,
                                   {write, <<?CMD_DRST>>},
                                   {read, 1, ?I2C_M_NO_RD_ACK}]),
    case Status band 16#f7 of
        16#10 -> ok;
        Any -> error({invalid_status, Any})
    end.

% @doc Write configuration into 1-Wire master register.
%
% Command code: d2h.
%
% This function can only be used inside of a {@link transaction/1} call.
%
% The default configuration is 0, i.e., all three configurable bits set to 0.
% This corresponds to an empty list. Each atom in the list activates the
% corresponding configuration (sets the bit to 1) and each atom not present in
% the list leads to a deactivation (sets the bit to 0).
%
% === Atom to Integer to Configuration Bit Mapping ===
% <table border="1" summary="Atom to Integer to Configuration Bit Mapping">
%  <tr><th>Atom</th><th>Integer</th><th>Configuration Bit</th>
%  <th>Activates</th></tr>
%  <tr><td>`apu'</td><td>1</td><td>Bit 0 (APU)</td><td>Active pullup</td></tr>
%  <tr><td>`spu'</td><td>4</td><td>Bit 2 (SPU)</td><td>Strong pullup</td></tr>
%  <tr><td>`overdrive'</td><td>8</td><td>Bit 3 (1WS)</td><td>1-Wire overdrive
%  speed</td></tr>
% </table>
%
% === Example ===
% To activate active pullup and overdrive speed use:
% ```
%  3> grisp_onewire:transaction(fun() -> grisp_onewire:write_config([apu, overdrive]) end).
%  ok
% '''
% This is the same as:
% ```
%  4> grisp_onewire:transaction(fun() -> grisp_onewire:write_config(1 bor 8) end).
%  ok
% '''
% @end
% I2C messages to 1-Wire Master:
% | S | AD,0 ‖ A ‖ WCFG ‖ A ‖ <byte> ‖ A ‖ Sr | AD,1 ‖ A | <byte> ‖ A\ | P |
-spec write_config([apu | overdrive | spu] | integer()) -> ok.
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

% @doc Reset device and activate active pullup (APU).
%
% This function can only be used inside of a {@link transaction/1} call.
% @see reset/0.
% @see write_config/1.
% @end
-spec detect() -> 'ok'.
detect() ->
    assert_transaction(),
    reset(),
    write_config([apu]).

% @doc Reset the bus and check the register for devices.
%
% Command code: b4h.
%
% This function can only be used inside of a {@link transaction/1} call.
%
% === Return Value Description ===
% <table border="1" summary="Return Value Description">
%  <tr><th>Atom</th><th>Description</th></tr>
%  <tr><td>`nothing_present'</td><td>No device on the bus detected</td></tr>
%  <tr><td>`presence_detected'</td><td>Some devices on the bus detected</td></tr>
%  <tr><td>`short_detected'</td><td>A short circuit between data and ground on the
%  bus detected</td></tr>
% </table>
% @end
-spec bus_reset() -> 'nothing_present' | 'presence_detected' | 'short_detected'.
bus_reset() ->
    assert_transaction(),
    grisp_i2c:msgs([?DS2482_I2C_ADR, {write, <<?CMD_1WRS>>}]),
    timer:sleep(1),
    check_status(grisp_i2c:msgs([?DS2482_I2C_ADR,
                                 {read, 1, ?I2C_M_NO_RD_ACK}])).

% @doc Write one data byte to the 1-Wire line.
%
% Command: a5h
%
% This function can only be used inside of a {@link transaction/1} call.
-spec write_byte(integer()) -> ok.
write_byte(Byte) ->
    assert_transaction(),
    grisp_i2c:msgs([?DS2482_I2C_ADR, {write, <<?CMD_1WWB, Byte>>}]),
    timer:sleep(1).

% @doc Read one data byte from the 1-Wire line.
%
% Command codes: 96h, e1h
%
% This function can only be used inside of a {@link transaction/1} call.
read_byte() ->
    assert_transaction(),
    grisp_i2c:msgs([?DS2482_I2C_ADR,
                    {write, <<?CMD_1WRB>>}]),
    timer:sleep(1),
    grisp_i2c:msgs([?DS2482_I2C_ADR,
                    {write, <<?CMD_SRP, 16#e1>>},
                    {read, 1, ?I2C_M_NO_RD_ACK}]).

% @private
%
% Command:78h
%
% This function can only be used inside of a {@link transaction/1} call.
-spec write_triplet(0 | 1) -> {0 | 1,0 | 1,0 | 1}.
write_triplet(Dir) ->
    assert_transaction(),
    Db = case Dir of 1 -> 16#ff; 0 -> 0 end,
    grisp_i2c:msgs([?DS2482_I2C_ADR, {write, <<?CMD_1WT, Db>>}]),
    timer:sleep(1),
    <<D:1, T:1, S:1, _:5>> = grisp_i2c:msgs([?DS2482_I2C_ADR,
                                             {read, 1, ?I2C_M_NO_RD_ACK}]),
    {D, T, S}.

% @doc Search the bus for devices.
%
% This function can only be used inside of a {@link transaction/1} call.
%
% If there are connected devices, i.e., {@link bus_reset/0} returns
% `present_detected', this function provides a list of the unique 64-bit
% addresses of all detected devices.
% Otherwise, the return values match the values from {@link bus_reset/0} or
% `fail' for other failures during the search.
% The addresses are represented as lists of eight byte values.
%
% === Example ===
% With five DS18B20 temperature sensors connected one get something like:
% ```
%  1> grisp_onewire:transaction(fun grisp_onewire:search/0).
%  [[40,255,203,173,80,23,4,182],
%  [40,255,67,77,96,23,5,138],
%  [40,255,190,25,96,23,3,203],
%  [40,255,54,42,96,23,5,35],
%  [40,255,18,91,96,23,3,62]]
% '''
-spec search() -> 'fail' | 'nothing_present' | 'short_detected' | [[byte()]].
search() ->
    assert_transaction(),
    search(0, []).


%--- Callbacks -----------------------------------------------------------------

% @private
init([]) ->
    put(?TRANSACTION_KEY, ?TRANSACTION_TOKEN),
    {ok, []}.

% @private
handle_call({transaction, Fun}, _From, State) ->
    Reply = try
        {result, Fun()} % TODO: Implement timeout for transactions
    catch
	?EXCEPTION(Class, Reason, Stacktrace) ->
            {exception, Class, Reason, ?GET_STACK(Stacktrace)}
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

search(Last_discrepancy, All) ->
    case bus_reset() of
        presence_detected ->
            write_byte(16#f0),
            Last_id = case All of
                          [H|_] -> H;
                          []    -> undefined
                      end,
            case search(Last_discrepancy, 1, 0, Last_id, []) of
                {last_device, Id} -> [convert_id(Bits)  || Bits <- [Id | All]];
                {Discrepancy, Id}     -> search(Discrepancy, [Id|All]);
                fail              -> fail
            end;
        Any -> Any
    end.

convert_id(Bits) ->
    Bin = << <<X:1>> || X <- lists:reverse(Bits) >>,
    lists:reverse([Y || <<Y:8/big>> <= Bin]).

search(_, 65, 0, _, Bits) ->
    {last_device, lists:reverse(Bits)};
search(_, 65, Last_zero, _, Bits) ->
    {Last_zero, lists:reverse(Bits)};
search(Last_discrepancy, I, Last_zero, Last_id, Bits) ->
    case search_step(Last_discrepancy, I, Last_id) of
        {_, 1, 1} -> fail;
        {0, 0, 0} -> search(Last_discrepancy, I+1, I, Last_id, [0 | Bits]);
        {D, _, _} -> search(Last_discrepancy, I+1, Last_zero, Last_id, [D | Bits])
    end.

search_step(Last_discrepancy, I, Last_id) when I < Last_discrepancy ->
    write_triplet(lists:nth(I, Last_id));
search_step(Last_discrepancy, I, _) when I =:= Last_discrepancy ->
    write_triplet(1);
search_step(Last_discrepancy, I, _) when I > Last_discrepancy ->
    write_triplet(0).
