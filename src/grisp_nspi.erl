% @private
-module(grisp_nspi).

% API
-export([init/0]).
-export([open_nif/0]).
-export([ioctl/4]).

-define(CPOL_LOW, 0).
-define(CPOL_HIGH, 1).
-define(CPHA_LEADING, 0).
-define(CPHA_TRAILING, 2).
-define(nif_stub, nif_stub_error(?LINE)).

-on_load(init/0).

%--- API -----------------------------------------------------------------------

init() -> erlang:load_nif(atom_to_list(?MODULE), 0).

open_nif() -> ?nif_stub.

ioctl(Obj, CS, Mode, Msg) when CS =:= spi1_pin9; CS =:= spi1_pin10 ->
    try
        grisp_gpio:configure_slot(spi1, disable_cs),
        grisp_gpio:clear(CS),
        ioctl(Obj, spi1, Mode, Msg)
    after
        grisp_gpio:set(CS),
        grisp_gpio:configure_slot(spi1, enable_cs)
    end;
ioctl(Obj, CS, Mode, Msg) ->
    ioctl_nif(Obj, chip_select(grisp_hw:platform(), CS), mode(Mode), Msg).

%--- Internal ------------------------------------------------------------------

ioctl_nif(_Obj, _CS, _Mode, _Msg) -> ?nif_stub.

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, Line}).

chip_select(grisp_base, spi1) -> 2;
chip_select(grisp_base, spi2) -> 3;
chip_select(grisp_base, spi1_pin1) -> 2;
chip_select(grisp2, spi1) -> 0;
chip_select(grisp2, spi2) -> 1;
chip_select(grisp2, spi2_pin1) -> 1;
chip_select(grisp2, spi2_pin9) -> 2;
chip_select(grisp2, spi2_pin10) -> 3.

mode(#{cpol := low,  cpha := leading})  -> ?CPOL_LOW  bor ?CPHA_LEADING;
mode(#{cpol := low,  cpha := trailing}) -> ?CPOL_LOW  bor ?CPHA_TRAILING;
mode(#{cpol := high, cpha := leading})  -> ?CPOL_HIGH bor ?CPHA_LEADING;
mode(#{cpol := high, cpha := trailing}) -> ?CPOL_HIGH bor ?CPHA_TRAILING.
