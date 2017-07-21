-module(grisp_gpio_drv_emu).

% API
-export([open/0]).
-export([command/2]).
-export([led/3]).  %% for access in mocking

%--- API -----------------------------------------------------------------------

open() -> undefined.

command(_State, <<8, 3>>)  -> led(1, red, off);
command(_State, <<8, 4>>)  -> led(1, red, on);
command(_State, <<9, 3>>)  -> led(1, green, off);
command(_State, <<9, 4>>)  -> led(1, green, on);
command(_State, <<10, 3>>) -> led(1, blue, off);
command(_State, <<10, 4>>) -> led(1, blue, on);
command(_State, <<11, 3>>) -> led(2, red, off);
command(_State, <<11, 4>>) -> led(2, red, on);
command(_State, <<12, 3>>) -> led(2, green, off);
command(_State, <<12, 4>>) -> led(2, green, on);
command(_State, <<13, 3>>) -> led(2, blue, off);
command(_State, <<13, 4>>) -> led(2, blue, on);
command(_State, Command) ->
    grisp_device_emu:broadcast({gpio, Command}).


%% Add led function to enable mocking instead of visually observing
%% We intercept calls to led instead of printing the output.
led(Nr, Color, Status) ->
  io:format("LED~p ~s ~s~n", [Nr, uppercase(Color), uppercase(Status)]).

%% in R20 string:uppercase instead of string:to_upper
uppercase(Atom) when is_atom(Atom) ->
  uppercase(atom_to_list(Atom));
uppercase(String) ->
  string:to_upper(String).

