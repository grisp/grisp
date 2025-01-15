-module(grisp_i2c).
-include("grisp_docs.hrl").

?moduledoc("""
GRiSP I²C API.

[Inter-Integrated Circuit (I²C)](https://en.wikipedia.org/wiki/I²C) is a
synchronous addressable serial communication bus. Two types of
nodes can exist, controllers and targets. Multiple controllers and targets
can exist on the bus simultaneously. Controllers generate the clock signal
and initiate communication with targets. Targets follow the clock and respond
when addressed. A GRiSP board acts as a controller to any connected target
peripherals on the I²C buses.

In GRiSP there are two main I²C buses, one "internal" bus (`i2c0`) and
one "external" bus (`i2c1`). The internal bus talks to targets on the SoM and
the board itself, such as EEPROM memory or the 1-Wire controller. The
external bus talks to any connected target peripherals, either on the PMOD
connector or the raised I²C pins.

Communication consists of either read or write messages. A read or write must
be addressed to a specific target (chip). A target is addressed by a 7-bit
address. The registry to use is specified in a target specific way. E.g. to
read a register, some targets expect a write message with the register address
as a value, followed by a read message.


> #### Note {: .tip}
> Please refer to the specification for a specific target chip for
instructions of what messages to send and receive, and how the registers are
structured.
""").

-include("grisp_nif.hrl").

% API
-export([buses/0]).
-export([open/1]).
-export([detect/1]).
-export([read/4]).
-export([write/4]).
-export([transfer/2]).

% Callbacks
-ifndef(DOC).
-on_load(on_load/0).
-endif.

%--- Types ---------------------------------------------------------------------

-type bus_name() :: atom().
-type bus_path() :: iodata().
-opaque bus() :: reference().
-type reg_addr() :: byte().
-type target_addr() :: 1..127.
-type length() :: non_neg_integer().
-type message() :: {
    Type :: read | write,
    Target :: target_addr(),
    Flags :: non_neg_integer(),
    Payload :: iodata() | length()
}.
-type error() :: {error, any()}.

-export_type([bus_name/0]).
-export_type([bus/0]).
-export_type([reg_addr/0]).
-export_type([target_addr/0]).
-export_type([message/0]).

%--- API -----------------------------------------------------------------------
?doc("""
Lists I²C buses.

### Example
```
1> grisp_i2c:buses()
#{i2c0 => #{name => i2c0,path => <<"/dev/i2c-0">>},
  i2c1 => #{name => i2c1,path => <<"/dev/i2c-1">>}}
```
""").
-spec buses() -> #{bus_name() := #{name := bus_name(), path := bus_path()}}.
buses() ->
    #{
        i2c0 => #{name => i2c0, path => <<"/dev/i2c-0">>},
        i2c1 => #{name => i2c1, path => <<"/dev/i2c-1">>}
    }.

?doc("""
Opens an I²C bus device by name.

### Example
```
2> I2C0 = grisp_i2c:open(i2c1).
#Ref<0.4157010815.3886678017.238942>
```
""").
% @see buses/0
-spec open(bus_name()) -> bus().
open(Name) ->
    #{path := Path} = maps:get(Name, buses()),
    i2c_open_nif(null(Path)).

?doc("""
Detects I²C devices on a bus.

Returns the address of each found device.

### Example
```
3> [io_lib:format("0x~.16B", [Target]) || Target <- grisp_i2c:detect(I2C0)].
["0x18","0x36","0x37","0x52","0x57","0x5A","0x5F"]
```
""").
-spec detect(bus()) -> [target_addr()].
detect(Bus) -> [Target || Target <- lists:seq(1, 127), present(Bus, Target)].

?doc("""
Performs a simplified read from a register on an I²C chip.

This function sends a write message with the single byte register address as
value, then a read message of the specified length.

> #### Note {: .tip}
> Only some chips support this simple operation. Please consult
the target chip specification.

### Example
```
4> grisp_i2c:read(I2C1, TargetAddr, RegAddr, 1).
<<255>>
```
""").
-spec read(bus(), target_addr(), reg_addr(), length()) -> binary().
read(Bus, Target, Register, Length) ->
    [ok, Resp] = transfer(Bus, [
        {write, Target, 0, <<Register>>},
        {read, Target, 1, Length}
    ]),
    Resp.

?doc("""
Performs a simplified write to a register on an I²C chip.

This function sends a write message with the single byte register address as
the first byte and then the data as the following bytes.

> #### Note {: .tip}
> Only some chips support this simple operation. Please consult
the target chip specification.

### Example
```
5> grisp_i2c:write(I2C1, TargetAddr, RegAddr, <<Value:8>>).
ok
```
""").
-spec write(bus(), target_addr(), reg_addr(), binary()) -> ok.
write(Bus, Target, Register, Data) ->
    [ok] = transfer(Bus, [{write, Target, 0, <<Register, Data/binary>>}]),
    ok.

?doc("""
Transfers I²C messages on a bus.

### Examples
```
6> grisp_i2c:transfer(I2C1, [{read, TargetAddr, RegAddr, 1}])
[<<255>>]
7> grisp_i2c:transfer(I2C1, [{write, TargetAddr, RegAddr, <<Value:8>>}])
ok
```
""").
-spec transfer(bus(), [message()]) -> [ok | binary()] | error().
transfer(Bus, Messages) -> i2c_transfer_nif(Bus, Messages).

%--- Callbacks -----------------------------------------------------------------

% @private
on_load() -> ?NIF_LOAD.

%--- Internal ------------------------------------------------------------------

i2c_open_nif(Bus) -> ?NIF_STUB([Bus]).

i2c_transfer_nif(Bus, Messages) -> ?NIF_STUB([Bus, Messages]).

null(Bin) -> [Bin, 0].

present(Bus, Target) ->
    case grisp_i2c:transfer(Bus, [{write, Target, 0, <<>>}]) of
        {error, _} -> false;
        [ok] -> true
    end.
