-module(grisp_ngpio).

% API
-export([open/1]).
-export([set/2]).
-export([get/1]).
-export([toggle/1]).
-export([clear/1]).

% Callbacks
-export([init/0]).

% Attributes
-on_load(init/0).

-include("grisp_nif.hrl").

%--- API -----------------------------------------------------------------------

open(Path) -> gpio_open_nif(null(Path)).

set(Pin, Value) when is_integer(Value) -> gpio_set_nif(Pin, Value).

get(Pin) -> gpio_get_nif(Pin).

toggle(Pin) -> gpio_toggle_nif(Pin).

clear(Pin) -> set(Pin, 0).

%--- Callbacks -----------------------------------------------------------------

init() ->
    case grisp_hw:platform() of
        grisp2 -> ok = erlang:load_nif(atom_to_list(?MODULE), 0);
        _Other -> ok
    end.

%--- Internal ------------------------------------------------------------------

gpio_open_nif(_Path) -> ?NIF_STUB.

gpio_set_nif(_Pin, _Value) -> ?NIF_STUB.

gpio_get_nif(_Pin) -> ?NIF_STUB.

gpio_toggle_nif(_Pin) -> ?NIF_STUB.

null(Bin) -> [Bin, 0].
