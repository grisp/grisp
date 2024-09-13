-module(ieee802154_ranging_network_SUITE).

%--- Includes ------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("mac_frame.hrl").
-include("ieee802154.hrl").

%--- Export --------------------------------------------------------------------

-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([all/0, groups/0]).

-export([standard_ranging_initiator/1]).
-export([standard_ranging_responder/1]).

-export([non_ranging_initiator/1]).
-export([ranging_responder_tx_no_ranging/1]).

%--- Callbacks -----------------------------------------------------------------
all() -> [{group, standard_ranging_exchange},
          {group, tx_no_request}].

%% RNG = Ranging
%% rcvr = Receiver
%% standard_ranging_exchange: Tests a simple ranging exchange
%% tx_no_request: RNG is activated for the rcvr but not requested by initiator
groups() -> [{standard_ranging_exchange, [parallel], [standard_ranging_initiator,
                                                      standard_ranging_responder]},
             {tx_no_request, [parallel], [non_ranging_initiator,
                                          ranging_responder_tx_no_ranging]}].

%--- Group setup and teardown --------------------------------------------------
init_per_group(_, Config) ->
    {NetPid, Network} = ieee802154_node:boot_network_node(),
    ExpectedFrame = {#frame_control{src_addr_mode = ?EXTENDED, dest_addr_mode = ?EXTENDED, ack_req = ?ENABLED}, #mac_header{src_addr = <<16#CAFEDECA00000001:64>>, dest_addr = <<16#CAFEDECA00000002:64>>}, <<"Test">>},
    [{tx_frame, ExpectedFrame}, {net_pid, NetPid}, {network, Network} | Config].

end_per_group(_, Config) ->
    Network = ?config(network, Config),
    NetPid = ?config(net_pid, Config),
    ieee802154_node:stop_network_node(Network, NetPid).

%--- Test cases setup and teardown ---------------------------------------------
init_per_testcase(standard_ranging_initiator, Config) -> 
    Network = ?config(network, Config),
    NodeRef = ieee802154_node:boot_ieee802154_node(initiator, Network, mac_extended_address, <<16#CAFEDECA00000001:64>>),
    [{node, NodeRef} | Config];

init_per_testcase(standard_ranging_responder, Config) -> 
    Network = ?config(network, Config),
    {_, Node} = NodeRef = ieee802154_node:boot_ieee802154_node(responder, Network, mac_extended_address, <<16#CAFEDECA00000002:64>>, fun ranging_input_callback/4),
    erpc:call(Node, ets, insert, [callback_table, {nb_rx_frames, 0}]), % ets table is created by the mock phy
    erpc:call(Node, ets, insert, [callback_table, {rx_frames, []}]),
    [{node, NodeRef} | Config];

init_per_testcase(non_ranging_initiator, Config) ->
    init_per_testcase(standard_ranging_initiator, Config);

init_per_testcase(ranging_responder_tx_no_ranging, Config) ->
    Network = ?config(network, Config),
    {_, Node} = NodeRef = ieee802154_node:boot_ieee802154_node(responder, Network, mac_extended_address, <<16#CAFEDECA00000002:64>>, fun ranging_input_callback_tx_no_request/4),
    erpc:call(Node, ets, insert, [callback_table, {nb_rx_frames, 0}]), % ets table is created by the mock phy
    erpc:call(Node, ets, insert, [callback_table, {rx_frames, []}]),
    [{node, NodeRef} | Config];

init_per_testcase(responder_no_ranging, Config) ->
    Network = ?config(network, Config),
    {_, Node} = NodeRef = ieee802154_node:boot_ieee802154_node(responder, Network, mac_extended_address, <<16#CAFEDECA00000002:64>>, fun ranging_input_callback_rx_no_request/4),
    erpc:call(Node, ets, insert, [callback_table, {nb_rx_frames, 0}]), % ets table is created by the mock phy
    erpc:call(Node, ets, insert, [callback_table, {rx_frames, []}]),
    [{node, NodeRef} | Config];

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_Name, Config) ->
    {NodePid, Node} = ?config(node, Config),
    ieee802154_node:stop_ieee802154_node(Node, NodePid).

ranging_input_callback(Frame, LQI, Security, Ranging) ->
    ?RANGING_ACTIVE = Ranging#ranging_informations.ranging_received,
    ?assert(is_integer(Ranging#ranging_informations.ranging_counter_start)),
    ?assert(is_integer(Ranging#ranging_informations.ranging_counter_stop)),
    update_callback_table(Frame, LQI, Security, Ranging).

ranging_input_callback_tx_no_request(Frame, LQI, Security, Ranging) ->
    ?NO_RANGING_REQUESTED = Ranging#ranging_informations.ranging_received,
    update_callback_table(Frame, LQI, Security, Ranging).

ranging_input_callback_rx_no_request(Frame, LQI, Security, Ranging) ->
    ?RANGING_REQUESTED_BUT_NOT_SUPPORTED= Ranging#ranging_informations.ranging_received,
    update_callback_table(Frame, LQI, Security, Ranging).

%--- Test cases ----------------------------------------------------------------
standard_ranging_initiator(Config) ->
    {_, Node} = ?config(node, Config),
    Frame = ?config(tx_frame, Config),
    timer:sleep(100),
    {ok, RangingInfos} = erpc:call(Node,  ieee802154, transmission, [Frame, ?ALL_RANGING]),
    ?assert(RangingInfos#ranging_informations.ranging_received),
    ?assert(is_integer(RangingInfos#ranging_informations.ranging_counter_start)),
    ?assert(is_integer(RangingInfos#ranging_informations.ranging_counter_stop)).


standard_ranging_responder(Config) ->
    {_, Node} = ?config(node, Config),
    ExpectedFrame = ?config(tx_frame, Config),
    ok = erpc:call(Node, ieee802154, rx_on, []),
    timer:sleep(300),
    [{nb_rx_frames, 1}] = erpc:call(Node, ets, lookup, [callback_table, nb_rx_frames]),
    [{_, [ExpectedFrame]}] = erpc:call(Node, ets, lookup, [callback_table, rx_frames]).

non_ranging_initiator(Config) ->
    {_, Node} = ?config(node, Config),
    Frame = ?config(tx_frame, Config),
    timer:sleep(100),
    {ok, RangingInfos} = erpc:call(Node,  ieee802154, transmission, [Frame, ?NON_RANGING]),
    ?assertNot(RangingInfos#ranging_informations.ranging_received).

ranging_responder_tx_no_ranging(Config) ->
    standard_ranging_responder(Config).

%--- Utils ---------------------------------------------------------------------
update_callback_table(Frame, _LQI, _Security, _Ranging) ->
    [{_, NbRxFrames}] = ets:lookup(callback_table, nb_rx_frames),
    ets:insert(callback_table, {nb_rx_frames, NbRxFrames+1}),
    [{_, Frames}] = ets:lookup(callback_table, rx_frames),
    ets:insert(callback_table, {rx_frames, [Frame | Frames]}).