-module(grisp_gpio).
-include("grisp_docs.hrl").

?moduledoc("""
GRiSP General Purpose Input/Output (GPIO) API.

General Purpose Input / Output (GPIO) is used to control digital signals on a
pin. The digital values `0` and `1` correspond to a low or high voltage
respectively. On GRiSP the voltage for GPIO pins is 3.3V.

A pin can be controlled either as an output pin or an input pin. For an output
pin, it is possible to set or get the current value. For an input pin, it is
possible to get the current value.

### Pin Mappings

For PMOD connectors, the number column (*#*) maps to the respective PMOD
pin (see [Figure 1](#figure_1)).

#### GRiSP 2 Pin Mappings

| ID          | Slot     | Type    | #  | Schematic | Direction | Description              |
| ----------- | -------- | ------- | -- | --------- | --------- | ------------------------ |
| `gpio1_1`   | GPIO1    | PMOD 1A | 1  | X1404.1   | In/Out    |                          |
| `gpio1_2`   | GPIO1    | PMOD 1A | 2  | X1404.2   | In/Out    |                          |
| `gpio1_3`   | GPIO1    | PMOD 1A | 3  | X1404.3   | In/Out    |                          |
| `gpio1_4`   | GPIO1    | PMOD 1A | 4  | X1404.4   | In/Out    |                          |
| `gpio1_7`   | GPIO1    | PMOD 1A | 7  | X1404.7   | In/Out    |                          |
| `gpio1_8`   | GPIO1    | PMOD 1A | 8  | X1404.8   | In/Out    |                          |
| `gpio1_9`   | GPIO1    | PMOD 1A | 9  | X1404.9   | In/Out    |                          |
| `gpio1_10`  | GPIO1    | PMOD 1A | 10 | X1404.10  | In/Out    |                          |
| `gpio_1_3`  | GPIO_1   | Generic |    | X1300.3   | In/Out    |                          |
| `gpio_1_4`  | GPIO_1   | Generic |    | X1300.4   | In/Out    |                          |
| `gpio_1_5`  | GPIO_1   | Generic |    | X1300.5   | In/Out    |                          |
| `gpio_1_6`  | GPIO_1   | Generic |    | X1300.6   | In/Out    |                          |
| `gpio_2_3`  | GPIO_2/4 | Generic |    | X1301.3   | In/Out    |                          |
| `gpio_2_4`  | GPIO_2/4 | Generic |    | X1301.4   | In/Out    |                          |
| `gpio_2_5`  | GPIO_2/4 | Generic |    | X1301.5   | In/Out    |                          |
| `gpio_2_6`  | GPIO_2/4 | Generic |    | X1301.6   | In/Out    |                          |
| `gpio_2_7`  | GPIO_2/4 | Generic |    | X1301.7   | In/Out    |                          |
| `gpio_2_8`  | GPIO_2/4 | Generic |    | X1301.8   | In/Out    |                          |
| `gpio_2_9`  | GPIO_2/4 | Generic |    | X1301.9   | In/Out    |                          |
| `gpio_2_10` | GPIO_2/4 | Generic |    | X1301.10  | In/Out    |                          |
| `gpio_2_11` | GPIO_2/4 | Generic |    | X1301.11  | In/Out    |                          |
| `gpio_2_12` | GPIO_2/4 | Generic |    | X1301.12  | In/Out    |                          |
| `gpio_2_13` | GPIO_2/4 | Generic |    | X1301.13  | In/Out    |                          |
| `gpio_2_14` | GPIO_2/4 | Generic |    | X1301.14  | In/Out    |                          |
| `led1_r`    | LED 1    | LED     |    | RGB1red   | Out       | _Reserved by LED driver_ |
| `led1_g`    | LED 1    | LED     |    | RGB1green | Out       | _Reserved by LED driver_ |
| `led1_b`    | LED 1    | LED     |    | RGB1blue  | Out       | _Reserved by LED driver_ |
| `led2_r`    | LED 2    | LED     |    | RGB2red   | Out       | _Reserved by LED driver_ |
| `led2_g`    | LED 2    | LED     |    | RGB2green | Out       | _Reserved by LED driver_ |
| `led2_b`    | LED 2    | LED     |    | RGB3blue  | Out       | _Reserved by LED driver_ |
| `jumper_1`  | Mode     | Jumper  |    | JUMPER1   | In        | Mode Switch Jumper State |
| `jumper_2`  | Mode     | Jumper  |    | JUMPER2   | In        | Mode Switch Jumper State |
| `jumper_3`  | Mode     | Jumper  |    | JUMPER3   | In        | Mode Switch Jumper State |
| `jumper_4`  | Mode     | Jumper  |    | JUMPER4   | In        | Mode Switch Jumper State |
| `jumper_5`  | Mode     | Jumper  |    | JUMPER5   | In        | Mode Switch Jumper State |

> #### Warning {: .warning}
> Reserved pins should only be carefully used if their drivers are not in use

#### GRiSP 1 Pin Mappings

| ID          | Slot     | Type    | #  | Schematic | Direction | Description              |
| ----------- | -------- | ------- | -- | --------- | --------- | ------------------------ |
| `gpio1_1`   | GPIO1    | PMOD 1  | 1  | X502.1    | In/Out    |                          |
| `gpio1_2`   | GPIO1    | PMOD 1  | 2  | X502.2    | In/Out    |                          |
| `gpio1_3`   | GPIO1    | PMOD 1  | 3  | X502.3    | In/Out    |                          |
| `gpio1_4`   | GPIO1    | PMOD 1  | 4  | X502.4    | In/Out    |                          |
| `gpio2_1`   | GPIO2    | PMOD 1  | 1  | X503.1    | In/Out    |                          |
| `gpio2_2`   | GPIO2    | PMOD 1  | 2  | X503.2    | In/Out    |                          |
| `gpio2_3`   | GPIO2    | PMOD 1  | 3  | X503.3    | In/Out    |                          |
| `gpio2_4`   | GPIO2    | PMOD 1  | 4  | X503.4    | In/Out    |                          |
| `led1_r`    | LED 1    | LED     |    | RGB1red   | Out       | _Reserved by LED driver_ |
| `led1_g`    | LED 1    | LED     |    | RGB1green | Out       | _Reserved by LED driver_ |
| `led1_b`    | LED 1    | LED     |    | RGB1blue  | Out       | _Reserved by LED driver_ |
| `led2_r`    | LED 2    | LED     |    | RGB2red   | Out       | _Reserved by LED driver_ |
| `led2_g`    | LED 2    | LED     |    | RGB2green | Out       | _Reserved by LED driver_ |
| `led2_b`    | LED 2    | LED     |    | RGB3blue  | Out       | _Reserved by LED driver_ |
| `jumper_1`  | Mode     | Jumper  |    | JUMPER1   | In        | Mode Switch Jumper State |
| `jumper_2`  | Mode     | Jumper  |    | JUMPER2   | In        | Mode Switch Jumper State |
| `jumper_3`  | Mode     | Jumper  |    | JUMPER3   | In        | Mode Switch Jumper State |
| `jumper_4`  | Mode     | Jumper  |    | JUMPER4   | In        | Mode Switch Jumper State |
| `jumper_5`  | Mode     | Jumper  |    | JUMPER5   | In        | Mode Switch Jumper State |
| `spi1_pin7` | SPI1     | PMOD 2A | 7  | X501.7    | In/Out    |                          |
| `spi1_pin8` | SPI1     | PMOD 2A | 8  | X501.8    | In/Out    |                          |
| `spi1_pin9` | SPI1     | PMOD 2A | 9  | X501.9    | In/Out    | _Reserved by SPI driver_ |
| `spi1_pin10`| SPI1     | PMOD 2A | 10 | X501.10   | In/Out    | _Reserved by SPI driver_ |
| `spi1_pin1` | SPI1     | PMOD 2A | 1  | X501.1    | In/Out    | _Reserved by SPI driver_ |
| `spi2_pin1` | SPI2     | PMOD 2  | 1  | X509.1    | In/Out    | _Reserved by SPI driver_ |
| `uart_1_cts`| UART     | PMOD 3  | 1  | X508.1    | In/Out    |                          |
| `uart_2_txd`| UART     | PMOD 3  | 2  | X508.2    | In/Out    |                          |
| `uart_3_rxd`| UART     | PMOD 3  | 3  | X508.3    | In/Out    |                          |
| `uart_4_rts`| UART     | PMOD 3  | 4  | X508.4    | In/Out    |                          |

> #### Warning {: .warning}
> Reserved pins should only be carefully used if their drivers are not in use

### PMOD Pin Numbers

<a name="figure_1"/>
<img src="../assets/pin_mapping.svg" style="width: 700px;">
Figure 1. PMOD connectors as seen from the side of a GRiSP board with number mappings

PMOD Type A consists of:
 * 4 × data, pins #1-4</li>
 * 1 × ground, pin #5</li>
 * 1 × 3.3V power, pin #6</li>

PMOD Type B consists of:
 * 8 × data, pins #1-4 and #7-10</li>
 * 1 × ground, pins #5 and #11</li>
 * 1 × 3.3V power, pins #6 and #12</li>
""").

-include("grisp_nif.hrl").

% API
-export([open/1]).
-export([open/2]).
-export([set/2]).
-export([get/1]).

% Callbacks
-ifndef(DOC).
-on_load(on_load/0).
-endif.

% Macros
-define(DEFAULT_OPTS, #{mode => {output, 0}}).

% Attributes
-compile({no_auto_import, [get/1]}).

%--- Types ---------------------------------------------------------------------

-type pin() :: atom().
-type opts() :: #{'mode' => mode(),
                  _ => _}.
-type mode() :: 'input' | {'output', value()}.
-opaque ref() :: reference().
-type value() :: 0 | 1.

-export_type([pin/0]).
-export_type([opts/0]).
-export_type([mode/0]).
-export_type([ref/0]).
-export_type([value/0]).

%--- API -----------------------------------------------------------------------

?doc(#{equiv => open(Pin, #{})}).
-spec open(pin()) -> ref().
open(Pin) -> open(Pin, #{}).

?doc("""
Creates a reference to a GPIO pin.
If no mode is given in the options, it defaults to `{output, 0}`.

<!-- tabs-open -->
### LED Output Example
Open the GPIO pin of the red component of LED 1 as an output pin with initial
value of `0`:
```
1> grisp_gpio:open(led1_r, #{mode => {output, 0}}).
#Ref<0.2691682867.116916226.176944>
```

### Jumper Input Example
Open the GPIO pin of Mode Jumper 1 as an input pin:
```
1> grisp_gpio:open(jumper_1, #{mode => input}).
#Ref<0.2691682867.116916226.176944>
```
<!-- tabs-close -->
""").
-spec open(pin(), opts()) -> ref().
open(Pin, UserOpts) ->
    #{mode := Mode} = maps:merge(?DEFAULT_OPTS, UserOpts),
    gpio_open_nif(pin(Pin), Mode).

?doc("""
Sets the current value of an output pin.

### Example
Turn off the red component of LED 1:
```
1> LED1R = grisp_gpio:open(led1_r, #{mode => {output, 0}}).
#Ref<0.2691682867.116916226.176944>
2> grisp_gpio:set(LED1R, 0).
ok
```
Turn on the red component of LED 1:
```
3> grisp_gpio:set(LED1R, 1).
ok
```
""").
-spec set(ref(), value()) -> ok.
set(Pin, Value) when is_integer(Value) -> gpio_set_nif(Pin, Value).

?doc("""
Returns the current value of a pin.

Returns the actual value for input pins or the currently set value for output
pins.

### Examples
To see whether the red component of LED 1 is enabled:
```
1> LED1R = grisp_gpio:open(led1_r, #{mode => {output, 0}}).
#Ref<0.2691682867.116916226.176944>
2> grisp_gpio:get(LED1R).
0
3> grisp_gpio:set(LED1R, 1).
ok
2> grisp_gpio:get(LED1R).
1
```

To see whether Mode Jumper 1 is on or off:
```
1> Jumper1 = grisp_gpio:open(jumper_1, #{mode => input}).
#Ref<0.2691682867.116916226.176944>
2> grisp_gpio:get(Jumper1).
0
```
Flip the jumper
```
3> grisp_gpio:get(Jumper1).
1
```
""").
-spec get(ref()) -> value().
get(Pin) -> gpio_get_nif(Pin).

%--- Callbacks -----------------------------------------------------------------

-ifndef(DOC).
on_load() -> ?NIF_LOAD.
-endif.

%--- Internal ------------------------------------------------------------------

gpio_open_nif(Attributes, Mode) -> ?NIF_STUB([Attributes, Mode]).

gpio_set_nif(Pin, Value) -> ?NIF_STUB([Pin, Value]).

gpio_get_nif(Pin) -> ?NIF_STUB([Pin]).

pin(Pin) -> pin(grisp_hw:platform(), Pin).

% erlfmt-ignore
pin(grisp_base, gpio1_1)    -> #{index => 0};
pin(grisp_base, gpio1_2)    -> #{index => 1};
pin(grisp_base, gpio1_3)    -> #{index => 2};
pin(grisp_base, gpio1_4)    -> #{index => 3};
pin(grisp_base, gpio2_1)    -> #{index => 4};
pin(grisp_base, gpio2_2)    -> #{index => 5};
pin(grisp_base, gpio2_3)    -> #{index => 6};
pin(grisp_base, gpio2_4)    -> #{index => 7};
pin(grisp_base, led1_r)     -> #{index => 8};
pin(grisp_base, led1_g)     -> #{index => 9};
pin(grisp_base, led1_b)     -> #{index => 10};
pin(grisp_base, led2_r)     -> #{index => 11};
pin(grisp_base, led2_g)     -> #{index => 12};
pin(grisp_base, led2_b)     -> #{index => 13};
pin(grisp_base, jumper_1)   -> #{index => 14};
pin(grisp_base, jumper_2)   -> #{index => 15};
pin(grisp_base, jumper_3)   -> #{index => 16};
pin(grisp_base, jumper_4)   -> #{index => 17};
pin(grisp_base, jumper_5)   -> #{index => 18};
pin(grisp_base, spi1_pin7)  -> #{index => 19};
pin(grisp_base, spi1_pin8)  -> #{index => 20};
pin(grisp_base, spi1_pin9)  -> #{index => 21};
pin(grisp_base, spi1_pin10) -> #{index => 22};
pin(grisp_base, spi1_pin1)  -> #{index => 23};
pin(grisp_base, spi2_pin1)  -> #{index => 24};
pin(grisp_base, uart_1_cts) -> #{index => 25};
pin(grisp_base, uart_2_txd) -> #{index => 26};
pin(grisp_base, uart_3_rxd) -> #{index => 27};
pin(grisp_base, uart_4_rts) -> #{index => 28};
pin(grisp2, gpio1_1)        -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 0};
pin(grisp2, gpio1_2)        -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 1};
pin(grisp2, gpio1_3)        -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 2};
pin(grisp2, gpio1_4)        -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 3};
pin(grisp2, gpio1_7)        -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 4};
pin(grisp2, gpio1_8)        -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 5};
pin(grisp2, gpio1_9)        -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 6};
pin(grisp2, gpio1_10)       -> #{path => <<"/pmod-gpio\0">>,             property => <<"grisp,gpios\0">>, index => 7};
pin(grisp2, uart_7)        -> #{path => <<"/pmod-uart\0">>,             property => <<"grisp,gpios\0">>, index => 4};
pin(grisp2, uart_8)        -> #{path => <<"/pmod-uart\0">>,             property => <<"grisp,gpios\0">>, index => 5};
pin(grisp2, uart_9)        -> #{path => <<"/pmod-uart\0">>,             property => <<"grisp,gpios\0">>, index => 6};
pin(grisp2, uart_10)       -> #{path => <<"/pmod-uart\0">>,             property => <<"grisp,gpios\0">>, index => 7};
pin(grisp2, i2c_1)        -> #{path => <<"/pmod-i2c\0">>,             property => <<"grisp,gpios\0">>, index => 0};
pin(grisp2, i2c_2)        -> #{path => <<"/pmod-i2c\0">>,             property => <<"grisp,gpios\0">>, index => 1};
pin(grisp2, i2c_3)        -> #{path => <<"/pmod-i2c\0">>,             property => <<"grisp,gpios\0">>, index => 2};
pin(grisp2, i2c_4)       -> #{path => <<"/pmod-i2c\0">>,             property => <<"grisp,gpios\0">>, index => 3};
pin(grisp2, gpio_1_3)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 0};
pin(grisp2, gpio_1_4)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 1};
pin(grisp2, gpio_1_5)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 2};
pin(grisp2, gpio_1_6)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 3};
pin(grisp2, gpio_2_3)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 4};
pin(grisp2, gpio_2_4)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 5};
pin(grisp2, gpio_2_5)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 6};
pin(grisp2, gpio_2_6)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 7};
pin(grisp2, gpio_2_7)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 8};
pin(grisp2, gpio_2_8)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 9};
pin(grisp2, gpio_2_9)       -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 10};
pin(grisp2, gpio_2_10)      -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 11};
pin(grisp2, gpio_2_11)      -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 12};
pin(grisp2, gpio_2_12)      -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 13};
pin(grisp2, gpio_2_13)      -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 14};
pin(grisp2, gpio_2_14)      -> #{path => <<"/pin-gpio\0">>,              property => <<"grisp,gpios\0">>, index => 15};
pin(grisp2, led1_r)         -> #{path => <<"/leds/grisp-rgb1-red\0">>,   property => <<"gpios\0">>,       index => 0};
pin(grisp2, led1_g)         -> #{path => <<"/leds/grisp-rgb1-green\0">>, property => <<"gpios\0">>,       index => 0};
pin(grisp2, led1_b)         -> #{path => <<"/leds/grisp-rgb1-blue\0">>,  property => <<"gpios\0">>,       index => 0};
pin(grisp2, led2_r)         -> #{path => <<"/leds/grisp-rgb2-red\0">>,   property => <<"gpios\0">>,       index => 0};
pin(grisp2, led2_g)         -> #{path => <<"/leds/grisp-rgb2-green\0">>, property => <<"gpios\0">>,       index => 0};
pin(grisp2, led2_b)         -> #{path => <<"/leds/grisp-rgb2-blue\0">>,  property => <<"gpios\0">>,       index => 0};
pin(grisp2, jumper_1)       -> #{path => <<"/jumper-keys\0">>,           property => <<"grisp,gpios\0">>, index => 0};
pin(grisp2, jumper_2)       -> #{path => <<"/jumper-keys\0">>,           property => <<"grisp,gpios\0">>, index => 1};
pin(grisp2, jumper_3)       -> #{path => <<"/jumper-keys\0">>,           property => <<"grisp,gpios\0">>, index => 2};
pin(grisp2, jumper_4)       -> #{path => <<"/jumper-keys\0">>,           property => <<"grisp,gpios\0">>, index => 3};
pin(grisp2, jumper_5)       -> #{path => <<"/jumper-keys\0">>,           property => <<"grisp,gpios\0">>, index => 4};
pin(Platform, Pin) ->
    error({unknown_pin, Platform, Pin}).
