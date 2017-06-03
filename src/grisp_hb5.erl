-module(grisp_hb5).

-export([config/2, stop/2, forward/2, backward/2]).

config(P, G) ->
    grisp_gpio_drv:configure_gpio(P, G, 
				  {output_0, output_0, input, input}).

start(P, G) ->
    grisp_gpio_drv:set(P, G, 2).

stop(P, G) ->
    grisp_gpio_drv:clear(P, G, 2).


forward(P, G) ->
    stop(P, G),	  % never change direcon on a active H-bridge
    grisp_gpio_drv:clear(P, G, 1),
    start(P, G).


backward(P, G) ->
    stop(P, G),	  % never change direcon on a active H-bridge
    grisp_gpio_drv:set(P, G, 1),
    start(P, G).
