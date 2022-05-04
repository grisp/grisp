-module(pmod_hb5).

% API
-export([open/1]).
-export([open/2]).
-export([stop/1]).
-export([forward/1]).
-export([backward/1]).

%--- API -----------------------------------------------------------------------
open(Slot) ->
    open(Slot, first).
open(Slot, first) ->
    Mode = #{mode => {output, 0}},
    {
        grisp_gpio:open(pin(Slot, 1), Mode),
        grisp_gpio:open(pin(Slot, 2), Mode)
    };
open(Slot, second) ->
    Mode = #{mode => {output, 0}},
    {
        grisp_gpio:open(pin(Slot, 7), Mode),
        grisp_gpio:open(pin(Slot, 8), Mode)
    }.

stop({_Pin1, Pin2}) -> grisp_gpio:set(Pin2, 0).

forward(State) ->
    stop(State),    % Never change direction on a active H-bridge
    direction(State, forward),
    start(State).

backward(State) ->
    stop(State),	  % Never change direction on a active H-bridge
    direction(State, backward),
    start(State).

%--- Internal ------------------------------------------------------------------

start({_Pin1, Pin2}) -> grisp_gpio:set(Pin2, 1).

direction({Pin1, _Pin2}, forward)  -> grisp_gpio:set(Pin1, 0);
direction({Pin1, _Pin2}, backward)  -> grisp_gpio:set(Pin1, 1).

pin(Slot, Pin) ->
    list_to_atom(atom_to_list(Slot) ++ "_" ++ integer_to_list(Pin)).
