-module(grisp_hw).
-include("grisp_docs.hrl").
?moduledoc("""
GRiSP hardware access.
""").

-include("grisp_nif.hrl").
-include("grisp_hw.hrl").

% API
-export([platform/0]).
-export([eeprom_read/0]).

% only used for GRISP1 and pre-production GRISP2 boards
-export([eeprom_reset_crc/0]).

% testing
-export([crc16/1]).

% Callbacks
-ifndef(DOC).
-on_load(on_load/0).
-endif.

%--- API -----------------------------------------------------------------------

?doc("""
Returns the platform name as an atom.
""").
-spec platform() -> atom().
platform() -> hw_platform_nif().

?doc("""
Reads GRiSP meta data from EEPROM

### Example
```
1> grisp_hw:eeprom_read().
{ok, #{grisp_batch => 1,
       grisp_pcb_variant => 1,
       grisp_pcb_version => "1.2",
       grisp_prod_date => {{2021,8,27},{0,0,0}},
       grisp_serial => 1002,
       grisp_version => "2"}}
```
""").
eeprom_read() ->
    <<_SigVersion:8,
      _Dummy1:3/binary,     %% unused
      Serial:4/little-unit:8,
      BatchNr:2/little-unit:8,
      ProdYear:2/little-unit:8,
      ProdMonth:8,
      ProdDay:8,
      VersMajor:1/binary,   %% contains GRiSP version and PCB Major
      VersMinor:8,          %% actually the PCB minor
      Variant:8,
      _Mac:6/binary,
      _Dummy2:1/binary,     %% unused
      Crc:2/little-unit:8,
      _Dummy3:2/binary>> = Data =
        grisp_eeprom:read(board, 0, ?GRISP_EEPROM_DATA_SIZE),
    <<GrispVersion:4, PcbMajor:4>> = VersMajor,
    MetaData = #{grisp_version      => lists:flatten(io_lib:format("~p", [GrispVersion])),
                 grisp_serial       => Serial,
                 grisp_pcb_version  => lists:flatten(io_lib:format("~p.~p", [PcbMajor, VersMinor])),
                 grisp_pcb_variant  => Variant,
                 grisp_batch        => BatchNr,
                 grisp_prod_date    => {{ProdYear,ProdMonth,ProdDay},{0,0,0}}},
    <<DataToBeVerified:24/binary, _/binary>> = Data,
    case (crc16(DataToBeVerified) =:= Crc) of
        true    -> {ok, MetaData};
        false   -> {invalid_crc, MetaData}
    end.

?doc("""
Fixes CRC bytes for pre-production boards
""").
-spec eeprom_reset_crc() -> ok.
eeprom_reset_crc() ->
    <<DataToBeVerified:24/binary, _/binary>> =
        grisp_eeprom:read(board, 0, ?GRISP_EEPROM_DATA_SIZE),
    CrcData = <<(crc16(DataToBeVerified)):2/little-unit:8>>,
    grisp_eeprom:write(board, 24, CrcData),
    case eeprom_read() of
        {ok, _} -> ok;
        _ -> error
    end.


%--- Callbacks -----------------------------------------------------------------

on_load() -> ?NIF_LOAD.

%--- Internal ------------------------------------------------------------------

hw_platform_nif() -> ?NIF_STUB([]).

?doc(false).
crc16(Cp) ->
    crc16(0, Cp).

?doc(false).
crc16(Crc, <<>>) ->
    Crc;
crc16(Crc, <<Cp:8, CpTail/binary>>) ->
    crc16((Crc bsr 8) bxor element(((Crc bxor Cp) band 16#ff) + 1, ?misc_crc16_tab), CpTail).
