% @doc GRiSP General Purpose Input/Output (GPIO) API.
%
% General Purpose Input / Output (GPIO) is used to control digital signals on a
% pin. The digital values `0' and `1' correspond to a low or high voltage
% respectively. On GRiSP the voltage for GPIO pins is 3.3V.
%
% A pin can be controlled either as an output pin or an input pin. For an output
% pin, it is possible to set or get the current value. For an input pin, it is
% possible to get the current value.
%
% === Pin Mappings ===
%
% For PMOD connectors, the number column (<b>#</b>) maps to the respective PMOD
% pin (see <a href="#figure_1">Figure 1</a>).
%
% <table border="1" cellpadding="8">
%   <caption>GRiSP 2 Pin Mappings</caption>
%   <tr>
%     <th rowspan="2">ID</th>
%     <th colspan="4">Mapping</th>
%     <th rowspan="2">Direction</th>
%     <th rowspan="2">Description</th>
%   </tr>
%   <tr>
%     <th>Slot</th>
%     <th>Type</th>
%     <th>#</th>
%     <th>Schematic</th>
%   </tr>
%   <tr><td>`gpio1_1'</td>    <td>GPIO1</td>    <td>PMOD 1A</td>  <td>1</td>  <td>X1404.1</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio1_2'</td>    <td>GPIO1</td>    <td>PMOD 1A</td>  <td>2</td>  <td>X1404.2</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio1_3'</td>    <td>GPIO1</td>    <td>PMOD 1A</td>  <td>3</td>  <td>X1404.3</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio1_4'</td>    <td>GPIO1</td>    <td>PMOD 1A</td>  <td>4</td>  <td>X1404.4</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio1_7'</td>    <td>GPIO1</td>    <td>PMOD 1A</td>  <td>7</td>  <td>X1404.7</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio1_8'</td>    <td>GPIO1</td>    <td>PMOD 1A</td>  <td>8</td>  <td>X1404.8</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio1_9'</td>    <td>GPIO1</td>    <td>PMOD 1A</td>  <td>9</td>  <td>X1404.9</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_1_3'</td>   <td>GPIO_1</td>   <td>Generic</td>  <td></td>   <td>X1300.3</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_1_4'</td>   <td>GPIO_1</td>   <td>Generic</td>  <td></td>   <td>X1300.4</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_1_5'</td>   <td>GPIO_1</td>   <td>Generic</td>  <td></td>   <td>X1300.5</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_1_6'</td>   <td>GPIO_1</td>   <td>Generic</td>  <td></td>   <td>X1300.6</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_2_3'</td>   <td>GPIO_2/4</td> <td>Generic</td>  <td></td>   <td>X1301.3</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_2_4'</td>   <td>GPIO_2/4</td> <td>Generic</td>  <td></td>   <td>X1301.4</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_2_5'</td>   <td>GPIO_2/4</td> <td>Generic</td>  <td></td>   <td>X1301.5</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_2_6'</td>   <td>GPIO_2/4</td> <td>Generic</td>  <td></td>   <td>X1301.6</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_2_7'</td>   <td>GPIO_2/4</td> <td>Generic</td>  <td></td>   <td>X1301.7</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_2_8'</td>   <td>GPIO_2/4</td> <td>Generic</td>  <td></td>   <td>X1301.8</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_2_9'</td>   <td>GPIO_2/4</td> <td>Generic</td>  <td></td>   <td>X1301.9</td>    <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_2_10'</td>  <td>GPIO_2/4</td> <td>Generic</td>  <td></td>   <td>X1301.10</td>   <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_2_11'</td>  <td>GPIO_2/4</td> <td>Generic</td>  <td></td>   <td>X1301.11</td>   <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_2_12'</td>  <td>GPIO_2/4</td> <td>Generic</td>  <td></td>   <td>X1301.12</td>   <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_2_13'</td>  <td>GPIO_2/4</td> <td>Generic</td>  <td></td>   <td>X1301.13</td>   <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio_2_14'</td>  <td>GPIO_2/4</td> <td>Generic</td>  <td></td>   <td>X1301.14</td>   <td>In/Out</td> <td></td></tr>
%   <tr><td>`led1_r'</td>     <td>LED 1</td>    <td>LED</td>      <td></td>   <td>RGB1red</td>    <td>Out</td>    <td><em>Reserved by LED driver</em></td></tr>
%   <tr><td>`led1_g'</td>     <td>LED 1</td>    <td>LED</td>      <td></td>   <td>RGB1green</td>  <td>Out</td>    <td><em>Reserved by LED driver</em></td></tr>
%   <tr><td>`led1_b'</td>     <td>LED 1</td>    <td>LED</td>      <td></td>   <td>RGB1blue</td>   <td>Out</td>    <td><em>Reserved by LED driver</em></td></tr>
%   <tr><td>`led2_r'</td>     <td>LED 2</td>    <td>LED</td>      <td></td>   <td>RGB2red</td>    <td>Out</td>    <td><em>Reserved by LED driver</em></td></tr>
%   <tr><td>`led2_g'</td>     <td>LED 2</td>    <td>LED</td>      <td></td>   <td>RGB2green</td>  <td>Out</td>    <td><em>Reserved by LED driver</em></td></tr>
%   <tr><td>`led2_b'</td>     <td>LED 2</td>    <td>LED</td>      <td></td>   <td>RGB3blue</td>   <td>Out</td>    <td><em>Reserved by LED driver</em></td></tr>
%   <tr><td>`jumper_1'</td>   <td>Mode</td>     <td>Jumper</td>   <td></td>   <td>JUMPER1</td>    <td>In</td>     <td>Mode Switch Jumper State</td></tr>
%   <tr><td>`jumper_2'</td>   <td>Mode</td>     <td>Jumper</td>   <td></td>   <td>JUMPER2</td>    <td>In</td>     <td>Mode Switch Jumper State</td></tr>
%   <tr><td>`jumper_3'</td>   <td>Mode</td>     <td>Jumper</td>   <td></td>   <td>JUMPER3</td>    <td>In</td>     <td>Mode Switch Jumper State</td></tr>
%   <tr><td>`jumper_4'</td>   <td>Mode</td>     <td>Jumper</td>   <td></td>   <td>JUMPER4</td>    <td>In</td>     <td>Mode Switch Jumper State</td></tr>
%   <tr><td>`jumper_5'</td>   <td>Mode</td>     <td>Jumper</td>   <td></td>   <td>JUMPER5</td>    <td>In</td>     <td>Mode Switch Jumper State</td></tr>
%   <tfoot><tr><td colspan="7"><b>Warning!</b> Reserved pins should only be carefully used if their drivers are not in use</td></tr></tfoot>
% </table><br/>
% <table border="1" cellpadding="8">
%   <caption>GRiSP 1 Pin Mappings</caption>
%   <tr>
%     <th rowspan="2">ID</th>
%     <th colspan="4">Mapping</th>
%     <th rowspan="2">Direction</th>
%     <th rowspan="2">Description</th>
%   </tr>
%   <tr>
%     <th>Slot</th>
%     <th>Type</th>
%     <th>#</th>
%     <th>Schematic</th>
%   </tr>
%   <tr><td>`gpio1_1'</td>    <td>GPIO1</td>    <td>PMOD 1</td>   <td>1</td>  <td>X502.1</td>     <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio1_2'</td>    <td>GPIO1</td>    <td>PMOD 1</td>   <td>2</td>  <td>X502.2</td>     <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio1_3'</td>    <td>GPIO1</td>    <td>PMOD 1</td>   <td>3</td>  <td>X502.3</td>     <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio1_4'</td>    <td>GPIO1</td>    <td>PMOD 1</td>   <td>4</td>  <td>X502.4</td>     <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio2_1'</td>    <td>GPIO2</td>    <td>PMOD 1</td>   <td>1</td>  <td>X503.1</td>     <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio2_2'</td>    <td>GPIO2</td>    <td>PMOD 1</td>   <td>2</td>  <td>X503.2</td>     <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio2_3'</td>    <td>GPIO2</td>    <td>PMOD 1</td>   <td>3</td>  <td>X503.3</td>     <td>In/Out</td> <td></td></tr>
%   <tr><td>`gpio2_4'</td>    <td>GPIO2</td>    <td>PMOD 1</td>   <td>4</td>  <td>X503.4</td>     <td>In/Out</td> <td></td></tr>
%   <tr><td>`led1_r'</td>     <td>LED 1</td>    <td>LED</td>      <td></td>   <td>RGB1red</td>    <td>Out</td>    <td><em>Reserved by LED driver</em></td></tr>
%   <tr><td>`led1_g'</td>     <td>LED 1</td>    <td>LED</td>      <td></td>   <td>RGB1green</td>  <td>Out</td>    <td><em>Reserved by LED driver</em></td></tr>
%   <tr><td>`led1_b'</td>     <td>LED 1</td>    <td>LED</td>      <td></td>   <td>RGB1blue</td>   <td>Out</td>    <td><em>Reserved by LED driver</em></td></tr>
%   <tr><td>`led2_r'</td>     <td>LED 2</td>    <td>LED</td>      <td></td>   <td>RGB2red</td>    <td>Out</td>    <td><em>Reserved by LED driver</em></td></tr>
%   <tr><td>`led2_g'</td>     <td>LED 2</td>    <td>LED</td>      <td></td>   <td>RGB2green</td>  <td>Out</td>    <td><em>Reserved by LED driver</em></td></tr>
%   <tr><td>`led2_b'</td>     <td>LED 2</td>    <td>LED</td>      <td></td>   <td>RGB3blue</td>   <td>Out</td>    <td><em>Reserved by LED driver</em></td></tr>
%   <tr><td>`jumper_1'</td>   <td>Mode</td>     <td>Jumper</td>   <td></td>   <td>JUMPER1</td>    <td>In</td>     <td>Mode Switch Jumper State</td></tr>
%   <tr><td>`jumper_2'</td>   <td>Mode</td>     <td>Jumper</td>   <td></td>   <td>JUMPER2</td>    <td>In</td>     <td>Mode Switch Jumper State</td></tr>
%   <tr><td>`jumper_3'</td>   <td>Mode</td>     <td>Jumper</td>   <td></td>   <td>JUMPER3</td>    <td>In</td>     <td>Mode Switch Jumper State</td></tr>
%   <tr><td>`jumper_4'</td>   <td>Mode</td>     <td>Jumper</td>   <td></td>   <td>JUMPER4</td>    <td>In</td>     <td>Mode Switch Jumper State</td></tr>
%   <tr><td>`jumper_5'</td>   <td>Mode</td>     <td>Jumper</td>   <td></td>   <td>JUMPER5</td>    <td>In</td>     <td>Mode Switch Jumper State</td></tr>
%   <tr><td>`spi1_pin7'</td>  <td>SPI1</td>     <td>PMOD 2A</td>  <td>7</td>  <td>X501.7</td>     <td>In/Out</td> <td></td></tr>
%   <tr><td>`spi1_pin8'</td>  <td>SPI1</td>     <td>PMOD 2A</td>  <td>8</td>  <td>X501.8</td>     <td>In/Out</td> <td></td></tr>
%   <tr><td>`spi1_pin9'</td>  <td>SPI1</td>     <td>PMOD 2A</td>  <td>9</td>  <td>X501.9</td>     <td>In/Out</td> <td><em>Reserved by SPI driver</em></td></tr>
%   <tr><td>`spi1_pin10'</td> <td>SPI1</td>     <td>PMOD 2A</td>  <td>10</td> <td>X501.10</td>    <td>In/Out</td> <td><em>Reserved by SPI driver</em></td></tr>
%   <tr><td>`spi1_pin1'</td>  <td>SPI1</td>     <td>PMOD 2A</td>  <td>1</td>  <td>X501.1</td>     <td>In/Out</td> <td><em>Reserved by SPI driver</em></td></tr>
%   <tr><td>`spi2_pin1'</td>  <td>SPI2</td>     <td>PMOD 2</td>   <td>1</td>  <td>X509.1</td>     <td>In/Out</td> <td><em>Reserved by SPI driver</em></td></tr>
%   <tr><td>`uart_1_cts'</td> <td>UART</td>     <td>PMOD 3</td>   <td>1</td>  <td>X508.1</td>     <td>In/Out</td> <td></td></tr>
%   <tr><td>`uart_2_txd'</td> <td>UART</td>     <td>PMOD 3</td>   <td>2</td>  <td>X508.2</td>     <td>In/Out</td> <td></td></tr>
%   <tr><td>`uart_3_rxd'</td> <td>UART</td>     <td>PMOD 3</td>   <td>3</td>  <td>X508.3</td>     <td>In/Out</td> <td></td></tr>
%   <tr><td>`uart_4_rts'</td> <td>UART</td>     <td>PMOD 3</td>   <td>4</td>  <td>X508.4</td>     <td>In/Out</td> <td></td></tr>
%   <tfoot><tr><td colspan="7"><b>Warning!</b> Reserved pins should only be carefully used if their drivers are not in use</td></tr></tfoot>
% </table><br/>
%
% === PMOD Pin Numbers ===
%
% <figure id="figure_1">
%   <img src="images/pin-mapping.svg" width="700px" alt="PMOD connectors as seen
%     from the side of a GRiSP board with number mappings"/>
%   <figcaption><em>Figure 1. PMOD connectors as seen from the side of a GRiSP
%     board with number mappings</em></figcaption>
% </figure>
%
% PMOD Type A consists of:
% <ul>
%   <li>4 &#215; data, pins #1-4</li>
%   <li>1 &#215; ground, pin #5</li>
%   <li>1 &#215; 3.3V power, pin #6</li>
% </ul>
% PMOD Type B consists of:
% <ul>
%   <li>8 &#215; data, pins #1-4 and #7-10</li>
%   <li>1 &#215; ground, pins #5 and #11</li>
%   <li>1 &#215; 3.3V power, pins #6 and #12</li>
% </ul>
-module(grisp_gpio).

-include("grisp_nif.hrl").

% API
-export([open/1]).
-export([open/2]).
-export([set/2]).
-export([get/1]).

% Callbacks
-export([on_load/0]).
-on_load(on_load/0).

% Macros
-define(DEFAULT_OPTS, #{mode => {output, 0}}).

% Attributes
-compile({no_auto_import, [get/1]}).

%--- Types ---------------------------------------------------------------------

-type pin() :: atom().
-type opts() :: map().
-opaque ref() :: reference().
-type value() :: 0 | 1.

-export_type([pin/0]).
-export_type([opts/0]).
-export_type([ref/0]).
-export_type([value/0]).

%--- API -----------------------------------------------------------------------

% @equiv open(Pin, #{})
-spec open(pin()) -> ref().
open(Pin) -> open(Pin, #{}).

% @doc Creates a reference to a GPIO pin.
%
% === Example ===
% Open the GPIO pin of the red component of LED 1 as an output pin with initial
% value of `0':
% ```
% 1> grisp_gpio:open(led1_r, {output, 0}).
% #Ref<0.2691682867.116916226.176944>
% '''
-spec open(pin(), opts()) -> ref().
open(Pin, UserOpts) ->
    #{mode := Mode} = maps:merge(?DEFAULT_OPTS, UserOpts),
    gpio_open_nif(pin(Pin), Mode).

% @doc Sets the current value of an output pin.
%
% === Example ===
% Turn off the red component of LED 1:
% ```
% 1> LED1R = grisp_gpio:open(led1_r, {output, 0}).
% #Ref<0.2691682867.116916226.176944>
% 2> grisp_gpio:set(LED1R, 0).
% ok
% '''
% Turn on the red component of LED 1:
% ```
% 3> grisp_gpio:set(LED1R, 1).
% ok
% '''
-spec set(ref(), value()) -> ok.
set(Pin, Value) when is_integer(Value) -> gpio_set_nif(Pin, Value).

% @doc Returns the current value of a pin.
%
% Returns the actual value for input pins or the currently set value for output
% pins.
%
% === Examples ===
% To see whether the red component of LED 1 is enabled:
% ```
% 1> LED1R = grisp_gpio:open(led1_r, {output, 0}).
% #Ref<0.2691682867.116916226.176944>
% 2> grisp_gpio:get(LED1R).
% 0
% 3> grisp_gpio:set(LED1R, 1).
% ok
% 2> grisp_gpio:get(LED1R).
% 1
% '''
-spec get(ref()) -> value().
get(Pin) -> gpio_get_nif(Pin).

%--- Callbacks -----------------------------------------------------------------

% @private
on_load() -> ok = erlang:load_nif(atom_to_list(?MODULE), 0).

%--- Internal ------------------------------------------------------------------

gpio_open_nif(_Attributes, _Mode) -> ?NIF_STUB.

gpio_set_nif(_Pin, _Value) -> ?NIF_STUB.

gpio_get_nif(_Pin) -> ?NIF_STUB.

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
