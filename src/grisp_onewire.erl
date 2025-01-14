-module(grisp_onewire).
-moduledoc """
Driver API for the
[DS2482-100 Single-Channel 1-Wire Master](https://datasheets.maximintegrated.com/en/ds/DS2482-100.pdf)

The functions in this module refer to the function commands documented in the
masters [data sheet](https://datasheets.maximintegrated.com/en/ds/DS2482-100.pdf).
Each function has a hexadecimal command code that is referenced in the
specification sheet.
""".

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

%--- API -----------------------------------------------------------------------

-doc(false).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Run a 1-Wire transaction.

Use this function to make sure that there is only one process running on the
1-Wire.

### Example
```
 2> grisp_onewire:transaction(fun() ->
                                 presence_detected = grisp_onewire:bus_reset(),
                                 grisp_onewire:write_byte(16#cc)
                              end).
 ok
```
""".
-spec transaction(fun()) -> any().
transaction(Fun) when is_function(Fun) ->
    case gen_server:call(?MODULE, {transaction, Fun}, ?TRANSACTION_TIMEOUT) of
        {result, Result} ->
            Result;
        {exception, Class, Reason, Stacktrace} ->
            erlang:raise(Class, Reason, Stacktrace)
    end.

-doc """
Reset the 1-Wire Master and terminate any ongoing 1-Wire communication.

This function can only be used inside of a `transaction/1` call.

_Command code: `f0h`._
""".
% I2C messages to 1-Wire Master:
% | S | AD,0 ‖ A ‖ DRST ‖ A ‖ Sr | AD,1 ‖ A | <byte> ‖ A\ | P |
-spec reset() -> 'ok'.
reset() ->
    Bus = assert_transaction(),
    Messages = [
        {write, ?DS2482_I2C_ADR, 0, <<?CMD_DRST>>},
        {read, ?DS2482_I2C_ADR, 0, 1}
    ],
    [ok, <<Status:8>>] = grisp_i2c:transfer(Bus, Messages),
    case Status band 16#f7 of
        16#10 -> ok;
        Any -> error({invalid_status, Any})
    end.

-doc """
Write configuration into 1-Wire master register.

This function can only be used inside of a `transaction/1` call.

The default configuration is `0`, i.e., all three configurable bits set to
`0`. This corresponds to an empty list. Each atom in the list activates the
corresponding configuration (sets the bit to `1`) and each atom not present in
the list leads to a deactivation (sets the bit to `0`).

### Atom to Integer to Configuration Bit Mapping
| Atom        | Integer | Configuration Bit | Activates              |
| ----------- | ------- | ----------------- | ---------------------- |
| `apu`       | `1`     | Bit 0 (APU)       | Active pullup          |
| `spu`       | `4`     | Bit 2 (SPU)       | Strong pullup          |
| `overdrive` | `8`     | Bit 3 (1WS)       | 1-Wire overdrive speed |

### Example
To activate active pullup and overdrive speed use:
```
 3> grisp_onewire:transaction(fun() -> grisp_onewire:write_config([apu, overdrive]) end).
 ok
```

This is the same as:
```
 4> grisp_onewire:transaction(fun() -> grisp_onewire:write_config(1 bor 8) end).
 ok
```

_Command code: `d2h`._
""".
% I2C messages to 1-Wire Master:
% | S | AD,0 ‖ A ‖ WCFG ‖ A ‖ <byte> ‖ A ‖ Sr | AD,1 ‖ A | <byte> ‖ A\ | P |
-spec write_config([apu | overdrive | spu] | integer()) -> ok.
write_config(Conf) when is_list(Conf) ->
    write_config(lists:foldl(fun(X, A) -> A bor map_config(X) end, 0, Conf));
write_config(Conf) when is_integer(Conf) ->
    Bus = assert_transaction(),
    Val = (bnot(Conf) bsl 4) bor Conf,
    Messages = [
        {write, ?DS2482_I2C_ADR, 0, <<?CMD_WCFG, Val>>},
        {read, ?DS2482_I2C_ADR, 0, 1}
    ],
    case grisp_i2c:transfer(Bus, Messages) of
        [ok, <<Conf:8>>] -> ok;
        Any -> error({read_back_config, Any, Val})
    end.

-doc """
Reset device and activate active pullup (APU).

This function can only be used inside of a `transaction/1` call.

See also: `reset/0`, `write_config/1`.
""".
-spec detect() -> 'ok'.
detect() ->
    assert_transaction(),
    reset(),
    write_config([apu]).

-doc """
Reset the bus and check the register for devices.

This function can only be used inside of a `transaction/1` call.

### Return Value Description

| Atom                | Description                                                 |
| ------------------- | ----------------------------------------------------------- |
| `nothing_present`   | No device on the bus detected                               |
| `presence_detected` | Some devices on the bus detected                            |
| `short_detected`    | A short circuit between data and ground on the bus detected |

_Command code: `b4h`._
""".
-spec bus_reset() -> 'nothing_present' | 'presence_detected' | 'short_detected'.
bus_reset() ->
    Bus = assert_transaction(),
    grisp_i2c:transfer(Bus, [{write, ?DS2482_I2C_ADR, 0, <<?CMD_1WRS>>}]),
    timer:sleep(1),
    [Result] = grisp_i2c:transfer(Bus, [
        {read, ?DS2482_I2C_ADR, 0, 1}
    ]),
    check_status(Result).

-doc """
Write one data byte to the 1-Wire line.

This function can only be used inside of a `transaction/1` call.

_Command code: `a5h`__
""".
-spec write_byte(integer()) -> ok.
write_byte(Byte) ->
    Bus = assert_transaction(),
    grisp_i2c:transfer(Bus, [{write, ?DS2482_I2C_ADR, 0, <<?CMD_1WWB, Byte>>}]),
    timer:sleep(1).

-doc """
Read one data byte from the 1-Wire line.

This function can only be used inside of a `transaction/1` call.

_Command codes: `96h`, `e1h`_
""".
read_byte() ->
    Bus = assert_transaction(),
    grisp_i2c:transfer(Bus, [{write, ?DS2482_I2C_ADR, 0, <<?CMD_1WRB>>}]),
    timer:sleep(1),
    [ok, Result] = grisp_i2c:transfer(Bus, [
        {write, ?DS2482_I2C_ADR, 0, <<?CMD_SRP, 16#e1>>},
        {read, ?DS2482_I2C_ADR, 0, 1}
    ]),
    Result.
-doc(false).
% This function can only be used inside of a {@link transaction/1} call.
%
% _Command code:`78h`_.
-spec write_triplet(0 | 1) -> {0 | 1,0 | 1,0 | 1}.
write_triplet(Dir) ->
    Bus = assert_transaction(),
    Db = case Dir of 1 -> 16#ff; 0 -> 0 end,
    grisp_i2c:transfer(Bus, [{write, ?DS2482_I2C_ADR, 0, <<?CMD_1WT, Db>>}]),
    timer:sleep(1),
    [<<D:1, T:1, S:1, _:5>>] = grisp_i2c:transfer(Bus, [
        {read, ?DS2482_I2C_ADR, 0, 1}
    ]),
    {D, T, S}.

-doc """
Search the bus for devices.

This function can only be used inside of a `transaction/1` call.

If there are connected devices, i.e., `bus_reset/0` returns
`presence_detected`, this function provides a list of the unique 64-bit
addresses of all detected devices.
Otherwise, the return values match the values from `bus_reset/0` or
`fail` for other failures during the search.
The addresses are represented as lists of eight byte values.

### Example
With five DS18B20 temperature sensors connected one can list the five device
IDs:
```
 1> grisp_onewire:transaction(fun grisp_onewire:search/0).
 [[40,255,203,173,80,23,4,182],
 [40,255,67,77,96,23,5,138],
 [40,255,190,25,96,23,3,203],
 [40,255,54,42,96,23,5,35],
 [40,255,18,91,96,23,3,62]]
```
""".
-spec search() -> 'fail' | 'nothing_present' | 'short_detected' | [[byte()]].
search() ->
    assert_transaction(),
    search(0, []).


%--- Callbacks -----------------------------------------------------------------

-doc(false).
init([]) ->
    Bus = grisp_i2c:open(i2c0),
    Detected = grisp_i2c:detect(Bus),
    case lists:member(?DS2482_I2C_ADR, Detected) of
        true -> ok;
        false -> error({bus_master_not_found, ?DS2482_I2C_ADR, Detected})
    end,
    put(?TRANSACTION_KEY, Bus),
    {ok, []}.

-doc(false).
handle_call({transaction, Fun}, _From, State) ->
    Reply = try
        {result, Fun()} % TODO: Implement timeout for transactions
    catch
        Class:Reason:Stacktrace ->
            {exception, Class, Reason, Stacktrace}
    end,
    {reply, Reply, State}.

-doc(false).
handle_cast(Cast, _State) -> error({unknown_cast, Cast}).

-doc(false).
handle_info(Info, _State) -> error({unknown_info, Info}).

%--- Internal ------------------------------------------------------------------

assert_transaction() ->
    case get(?TRANSACTION_KEY) of
        undefined -> error(no_transaction);
        Bus -> Bus
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
