-module(ieee802154_standalone_stack_SUITE).

%--- Includes ------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("mac_frame.hrl").
-include("ieee802154.hrl").
%--- Export --------------------------------------------------------------------

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([stack_test/1]).
-export([get_set_mac_extended_addr/1]).
-export([get_set_mac_short_addr/1]).
-export([get_set_pan_id/1]).
-export([get_set_max_be/1]).
-export([get_set_min_BE/1]).
-export([get_set_max_csma_backoff/1]).
-export([get_set_unsupported_attribute/1]).
-export([double_on_off/1]).
-export([mlme_reset_pib_reset_true_test/1]).
-export([mlme_reset_pib_reset_false_test/1]).
-export([tx_invalid_address/1]).

%--- Callbacks -----------------------------------------------------------------
all() -> [stack_test,
          double_on_off,
          get_set_mac_extended_addr,
          get_set_mac_short_addr,
          get_set_pan_id,
          get_set_min_BE,
          get_set_max_csma_backoff,
          get_set_unsupported_attribute,
          mlme_reset_pib_reset_true_test,
          mlme_reset_pib_reset_false_test,
          tx_invalid_address].

init_per_testcase(_, Config) ->
    ct:pal("~w", [Config]),
    MockPhyPid = mock_phy:start_link(spi2, {perfect, #{}}),
    IEEE = ieee802154:start_link(#ieee_parameters{phy_layer = mock_phy,
                                                  duty_cycle = ieee802154_duty_cycle_non_beacon}),
    [{ieee_pid, IEEE}, {phy_pid, MockPhyPid} | Config].

end_per_testcase(_, _Config) ->
    ieee802154:stop_link(),
    mock_phy:stop_link().

%--- Test cases ----------------------------------------------------------------

stack_test(_Config) -> 
    ieee802154:rx_on(),
    ieee802154:transmission({#frame_control{},
                             #mac_header{},
                             <<"Simple Frame">>}),
    ieee802154:transmission({#frame_control{ack_req = ?ENABLED},
                             #mac_header{},
                             <<"AR Frame">>}),
    ieee802154:rx_off(),
    ieee802154:transmission({#frame_control{},
                             #mac_header{},
                             <<"Simple Frame">>}),
    ieee802154:transmission({#frame_control{ack_req = ?ENABLED},
                             #mac_header{},
                             <<"AR Frame">>}).

double_on_off(_Config) ->
    ieee802154:rx_on(),
    {error, rx_already_on} = ieee802154:rx_on(),
    ieee802154:rx_off(),
    ieee802154:rx_off().

get_set_mac_extended_addr(_Config) ->
    ExtAddr = <<16#DECACAFEDECACAFE:64>>,
    DefaultValue = ieee802154:get_pib_attribute(mac_extended_address),
    #{eui := EUI} = mock_phy:read(eui),
    ?assertEqual(EUI, DefaultValue),
    ieee802154:set_pib_attribute(mac_extended_address, ExtAddr),
    NewValue = ieee802154:get_pib_attribute(mac_extended_address),
    ?assertEqual(ExtAddr, NewValue),
    ?assertNotEqual(ExtAddr, DefaultValue).

%--- Test cases: PiB getters/setters
%  cw0 => 2, % cf. p.22 standard
%  mac_max_BE => 5,
%  mac_max_csma_backoffs => 4,
%  mac_min_BE => 3
%  mac_pan_id => <<16#CAFE:16>>,
%  mac_short_addr => <<16#ABCD:16>>
get_set_mac_short_addr(_Config) ->
    ShortAddr = <<16#CAFE:16>>,
    DefaultValue = ieee802154:get_pib_attribute(mac_short_address),
    ieee802154:set_pib_attribute(mac_short_address, ShortAddr),
    NewValue = ieee802154:get_pib_attribute(mac_short_address),
    ?assertEqual(ShortAddr, NewValue),
    ?assertNotEqual(ShortAddr, DefaultValue).

get_set_pan_id(_Config) ->
    PanId = <<16#ABCD:16>>,
    DefaultValue = ieee802154:get_pib_attribute(mac_pan_id),
    ieee802154:set_pib_attribute(mac_pan_id, PanId),
    NewValue = ieee802154:get_pib_attribute(mac_pan_id),
    ?assertEqual(PanId, NewValue),
    ?assertNotEqual(PanId, DefaultValue).

get_set_max_be(_Config) ->
    MaxBe = 8,
    DefaultValue = ieee802154:get_pib_attribute(mac_max_BE),
    ieee802154:set_pib_attribute(mac_max_BE, MaxBe),
    NewValue = ieee802154:get_pib_attribute(mac_max_BE),
    ?assertEqual(MaxBe, NewValue),
    ?assertNotEqual(MaxBe, DefaultValue).

get_set_max_csma_backoff(_Config) ->
    MaxCSMABackoff = 5,
    DefaultValue = ieee802154:get_pib_attribute(mac_max_csma_backoffs),
    ieee802154:set_pib_attribute(mac_max_csma_backoffs, MaxCSMABackoff),
    NewValue = ieee802154:get_pib_attribute(mac_max_csma_backoffs),
    ?assertEqual(MaxCSMABackoff, NewValue),
    ?assertNotEqual(MaxCSMABackoff, DefaultValue).

get_set_min_BE(_Config) ->
    MinBE = 8,
    DefaultValue = ieee802154:get_pib_attribute(mac_min_BE),
    ieee802154:set_pib_attribute(mac_min_BE, MinBE),
    NewValue = ieee802154:get_pib_attribute(mac_min_BE),
    ?assertEqual(MinBE, NewValue),
    ?assertNotEqual(MinBE, DefaultValue).

get_set_unsupported_attribute(_Config) ->
    {error, unsupported_attribute} = ieee802154:get_pib_attribute(unknown_attribute),
    {error, unsupported_attribute} = ieee802154:set_pib_attribute(unknown_attribute, 40).

mlme_reset_pib_reset_true_test(_Config) ->
    ieee802154:set_pib_attribute(mac_extended_address, <<16#DECACAFEFEEDBEEF:64>>),
    ieee802154:set_pib_attribute(mac_short_address, <<16#BEEF:16>>),
    ieee802154:set_pib_attribute(mac_pan_id, <<16#FEED:16>>),
    ieee802154:set_pib_attribute(mac_max_BE, 4),
    ieee802154:set_pib_attribute(mac_max_csma_backoffs, 2),
    ieee802154:set_pib_attribute(mac_min_BE, 1),

    ieee802154:reset(true),

    NewExtAddr = ieee802154:get_pib_attribute(mac_extended_address),
    ?assertEqual(<<16#FFFFFFFF00000000:64>>, NewExtAddr),
    NewShortAddr = ieee802154:get_pib_attribute(mac_short_address),
    ?assertEqual(<<16#FFFF:16>>, NewShortAddr),
    NewPanId = ieee802154:get_pib_attribute(mac_pan_id),
    ?assertEqual(<<16#FFFF:16>>, NewPanId),
    #{pan_id := PmodPanId, short_addr := PmodShortAddr} = mock_phy:read(panadr),
    ?assertEqual(<<16#FFFF:16>>, PmodPanId),
    ?assertEqual(<<16#FFFF:16>>, PmodShortAddr),
    NewMaxBe = ieee802154:get_pib_attribute(mac_max_BE),
    ?assertEqual(5, NewMaxBe),
    NewMaxCSMABackoffs = ieee802154:get_pib_attribute(mac_max_csma_backoffs),
    ?assertEqual(4, NewMaxCSMABackoffs),
    NewMinBE = ieee802154:get_pib_attribute(mac_min_BE),
    ?assertEqual(3, NewMinBE).

mlme_reset_pib_reset_false_test(_Config) ->
    ieee802154:set_pib_attribute(mac_extended_address, <<16#DECACAFEFEEDBEEF:64>>),
    ieee802154:set_pib_attribute(mac_short_address, <<16#BEEF:16>>),
    ieee802154:set_pib_attribute(mac_pan_id, <<16#FEED:16>>),
    ieee802154:set_pib_attribute(mac_max_BE, 4),
    ieee802154:set_pib_attribute(mac_max_csma_backoffs, 2),
    ieee802154:set_pib_attribute(mac_min_BE, 1),

    ieee802154:reset(false),

    #{eui := PmodEUI} = mock_phy:read(eui),
    NewExtAddr = ieee802154:get_pib_attribute(mac_extended_address),
    ?assertEqual(<<16#DECACAFEFEEDBEEF:64>>, NewExtAddr),
    ?assertEqual(<<16#DECACAFEFEEDBEEF:64>>, PmodEUI),
    #{pan_id := PmodPanId, short_addr := PmodShortAddr} = mock_phy:read(panadr),
    NewShortAddr = ieee802154:get_pib_attribute(mac_short_address),
    ?assertEqual(<<16#BEEF:16>>, NewShortAddr),
    ?assertEqual(<<16#BEEF:16>>, PmodShortAddr),
    NewPanId = ieee802154:get_pib_attribute(mac_pan_id),
    ?assertEqual(<<16#FEED:16>>, NewPanId),
    ?assertEqual(<<16#FEED:16>>, PmodPanId),
    NewMaxBe = ieee802154:get_pib_attribute(mac_max_BE),
    ?assertEqual(4, NewMaxBe),
    NewMaxCSMABackoffs = ieee802154:get_pib_attribute(mac_max_csma_backoffs),
    ?assertEqual(2, NewMaxCSMABackoffs),
    NewMinBE = ieee802154:get_pib_attribute(mac_min_BE),
    ?assertEqual(1, NewMinBE).

tx_invalid_address(_Config) ->
    FC = #frame_control{src_addr_mode = ?NONE, dest_addr_mode = ?NONE},
    MH = #mac_header{},
    Payload = <<"Invalid address">>,
    Frame = {FC, MH, Payload},
    {error, invalid_address} = ieee802154:transmission(Frame, ?DISABLED).
