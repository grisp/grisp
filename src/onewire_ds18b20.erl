-module(onewire_ds18b20).
-include("grisp_docs.hrl").
?moduledoc("""
Communicate with the
[DS18B20 - Programmable Resolution 1-Wire Digital Thermometer](https://datasheets.maximintegrated.com/en/ds/DS18B20.pdf)
""").

% API
-export([temp/1]).
-export([read_scratchpad/1]).
-export([convert/2]).

-define(READ_SCRATCHPAD, 16#BE).
-define(CONVERT_T,       16#44).

%--- API -----------------------------------------------------------------------

?doc("""
Read the temperature in °C from the scratchpad.

## Example
```
 onewire_ds18b20:temp([40,255,190,25,96,23,3,203]).
 22.375
```
""").
-spec temp([byte()]) -> float().
temp(ID) ->
    grisp_onewire:transaction(fun() ->
        select_device(ID),
        {<<LSB>>, <<MSB>>, Config} = read_scratchpad(),
        Bits = bits(Config),
        <<_:4, Temp:Bits/signed-big, _/binary>> = <<MSB, LSB>>,
        Temp / 16.0
    end).

?doc("""
Read the scratchpad.

Returns the two bytes of the temperature register (`LSB` and `MSB`) and
the one byte of the configuration register.
""").
-spec read_scratchpad([byte()]) -> {LSB::binary(), MSB::binary(),
                                   Config::binary()}.
read_scratchpad(ID) ->
    grisp_onewire:transaction(fun() ->
        select_device(ID),
        read_scratchpad()
    end).

?doc("""
Initiate a temperature measurement.

## Example
```
 1> onewire_ds18b20:convert([40,255,190,25,96,23,3,203], 500).
 ok
```
""").
-spec convert([byte()], any()) -> ok.
convert(ID, Timeout) ->
    grisp_onewire:transaction(fun() ->
        select_device(ID),
        grisp_onewire:write_byte(?CONVERT_T),
        confirm(Timeout)
    end).

%--- Internal ------------------------------------------------------------------

select_device(ID) ->
    case grisp_onewire:bus_reset() of
        presence_detected ->
            grisp_onewire:write_byte(16#55),
            [grisp_onewire:write_byte(B) || B <- ID];
        Other ->
            error({onewire, Other})
    end.

read_scratchpad() ->
    grisp_onewire:write_byte(?READ_SCRATCHPAD),
    [LSB, MSB, _TH, _TL, Config, _, _, _, _CRC]
        = [grisp_onewire:read_byte() || _ <- lists:seq(0, 8)],
    {LSB, MSB, Config}.

bits(<<_:1, 0:1, 0:1, _:5>>) -> 9;
bits(<<_:1, 0:1, 1:1, _:5>>) -> 10;
bits(<<_:1, 1:1, 0:1, _:5>>) -> 11;
bits(<<_:1, 1:1, 1:1, _:5>>) -> 12.

confirm(Timeout) ->
    confirm(grisp_onewire:read_byte(), ms(), Timeout).

confirm(<<16#00>>, Start, Timeout) ->
    case ms() - Start > Timeout of
        false ->
            timer:sleep(10),
            confirm(grisp_onewire:read_byte(), Start, Timeout);
        true ->
            error({onewire_ds18b20, confirmation_timeout})
    end;
confirm(<<1:1, _:7>>, _Start, _Timeout) ->
    ok.

ms() -> erlang:monotonic_time(millisecond).
