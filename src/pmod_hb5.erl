-module(pmod_hb5).

-export([config/1, stop/1, forward/1, backward/1]).

config(Slot) ->
    grisp_gpio:configure_slot(Slot, {output_0, output_0, input, input}).

start(gpio1) -> grisp_gpio:set(gpio1_2);
start(gpio2) -> grisp_gpio:set(gpio2_2).

stop(gpio1) -> grisp_gpio:clear(gpio1_2);
stop(gpio2) -> grisp_gpio:clear(gpio2_2).


forward(Slot) ->
    stop(Slot),    % Never change direction on a active H-bridge
    direction(Slot, forward),
    start(Slot).


backward(Slot) ->
    stop(Slot),	  % Never change direction on a active H-bridge
    direction(Slot, backward),
    start(Slot).

direction(gpio1, forward)  -> grisp_gpio:clear(gpio1_1);
direction(gpio1, backward) -> grisp_gpio:set(gpio1_1);
direction(gpio2, forward)  -> grisp_gpio:clear(gpio2_1);
direction(gpio2, backward) -> grisp_gpio:set(gpio2_1).
