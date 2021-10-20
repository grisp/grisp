% @private
-module(grisp_nspi).

-include("grisp_nif.hrl").

% API
-export([open_nif/0]).
-export([ioctl/4]).

% Callbacks
-export([on_load/0]).
-on_load(on_load/0).

-define(CPOL_LOW, 0).
-define(CPOL_HIGH, 1).
-define(CPHA_LEADING, 0).
-define(CPHA_TRAILING, 2).

%--- API -----------------------------------------------------------------------

open_nif() -> ?NIF_STUB.

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

%--- Callbacks -----------------------------------------------------------------

on_load() -> erlang:load_nif(atom_to_list(?MODULE), 0).

%--- Internal ------------------------------------------------------------------

ioctl_nif(_Obj, _CS, _Mode, _Msg) -> ?NIF_STUB.

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
