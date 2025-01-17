-module(pmod_hb5).

% API
-export([open/1]).
-export([open/2]).
-export([stop/1]).
-export([forward/1]).
-export([backward/1]).

%--- API -----------------------------------------------------------------------

open({gpio1, top}) ->
    open(gpio1_1, gpio1_2);
open({gpio1, bottom}) ->
    open(gpio1_7, gpio1_8);
open({uart, bottom}) ->
    open(uart_7, uart_8);
open(i2c) ->
    open(i2c_1, i2c_2).

open(Pin1, Pin2) ->
    Mode = #{mode => {output, 0}},
    {
        grisp_gpio:open(Pin1, Mode),
        grisp_gpio:open(Pin2, Mode)
    }.

stop({_Pin1, Pin2}) -> grisp_gpio:set(Pin2, 0).

forward(State) ->
    stop(State),    % Never change direction on a active H-bridge
    direction(State, forward),
    start(State).

backward(State) ->
    stop(State),    % Never change direction on a active H-bridge
    direction(State, backward),
    start(State).

%--- Internal ------------------------------------------------------------------

start({_Pin1, Pin2}) -> grisp_gpio:set(Pin2, 1).

direction({Pin1, _Pin2}, forward)  -> grisp_gpio:set(Pin1, 0);
direction({Pin1, _Pin2}, backward)  -> grisp_gpio:set(Pin1, 1).
