% @private
-module(grisp_nspi).

-include("grisp_nif.hrl").

% API
-export([open/1]).
-export([open/2]).
-export([transfer/2]).

% Callbacks
-export([on_load/0]).
-on_load(on_load/0).

-define(CPOL_HIGH, 1).
-define(CPHA_TRAILING, 2).
-define(CS_DISABLE, 4).

%--- API -----------------------------------------------------------------------

open(spi1) -> open(spi1, spi1_pin1);
open(spi2) -> open(spi2, spi2_pin1).

open(_Bus, CS) -> {open_nif(), pin(grisp_hw:platform(), CS)}.

transfer(Ref, Messages) -> [message(Ref, M) || M <- Messages].

%--- Callbacks -----------------------------------------------------------------

on_load() -> erlang:load_nif(atom_to_list(?MODULE), 0).

%--- Internal ------------------------------------------------------------------

pin(grisp_base, spi1_pin1) -> {cs, 2};
pin(grisp_base, spi2_pin1) -> {cs, 3};
pin(grisp2, spi1_pin1) -> {cs, 0};
pin(grisp2, spi2_pin1) -> {cs, 1};
pin(grisp2, spi2_pin9) -> {cs, 2};
pin(grisp2, spi2_pin10) -> {cs, 3};
pin(_Platform, Pin) -> {gpio, grisp_gpio:open(Pin, #{mode => {output, 1}})}.

message({Bus, Pin}, {Mode, Message}) ->
    chip_select(Pin, mode(Mode), fun(CS, M) ->
        ioctl_nif(Bus, CS, M, Message)
    end);
message({Bus, Pin}, {Mode, Message, Skip, Pad}) ->
    chip_select(Pin, mode(Mode), fun(CS, M) ->
        Padding = binary:copy(<<16#ff>>, Pad),
        Request = <<Message/binary, Padding/binary>>,
        Result = ioctl_nif(Bus, CS, M, Request),
        <<_:Skip/binary, Response/binary>> = Result,
        Response
    end).

chip_select({cs, Pin}, Mode, Fun) ->
    Fun(Pin, Mode);
chip_select({gpio, Pin}, Mode, Fun) ->
    grisp_gpio:set(Pin, 0),
    try
        case grisp_hw:platform() of
            grisp_base -> Fun(0, Mode);
            grisp2 -> Fun(0, Mode bor ?CS_DISABLE)
        end
    after
        grisp_gpio:set(Pin, 1)
    end.

mode(#{cpol := low, cpha := leading}) -> 0;
mode(#{cpol := low, cpha := trailing}) -> ?CPHA_TRAILING;
mode(#{cpol := high, cpha := leading}) -> ?CPOL_HIGH;
mode(#{cpol := high, cpha := trailing}) -> ?CPOL_HIGH bor ?CPHA_TRAILING.

open_nif() -> ?NIF_STUB.

ioctl_nif(_Obj, _CS, _Mode, _Msg) -> ?NIF_STUB.
