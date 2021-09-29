% @private
-module(grisp_nspi).

% API
-export([init/0]).
-export([open_nif/0]).
-export([command/4]).

-define(CPOL_LOW, 0).
-define(CPOL_HIGH, 1).
-define(CPHA_LEADING, 0).
-define(CPHA_TRAILING, 2).
-define(nif_stub, nif_stub_error(?LINE)).

-on_load(init/0).

%--- API -----------------------------------------------------------------------

init() -> erlang:load_nif(atom_to_list(?MODULE), 0).

open_nif() -> ?nif_stub.

command(Obj, Slot, Mode, Command) ->
    command_nif(Obj, slave_select(Slot), mode(Mode), Command).

%--- Internal ------------------------------------------------------------------

command_nif(_Obj, _Slot, _Mode, _Command) -> ?nif_stub.

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, Line}).

slave_select(spi1) -> 
    case grisp_hw:platform() of
        grisp_base -> 2;
        grisp2 -> 0
    end;
slave_select(spi2) ->
    case grisp_hw:platform() of
        grisp_base -> 3;
        grisp2 -> 1
    end.

mode(#{cpol := low,  cpha := leading})  -> ?CPOL_LOW  bor ?CPHA_LEADING;
mode(#{cpol := low,  cpha := trailing}) -> ?CPOL_LOW  bor ?CPHA_TRAILING;
mode(#{cpol := high, cpha := leading})  -> ?CPOL_HIGH bor ?CPHA_LEADING;
mode(#{cpol := high, cpha := trailing}) -> ?CPOL_HIGH bor ?CPHA_TRAILING.
