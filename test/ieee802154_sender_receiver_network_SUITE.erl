-module(ieee802154_sender_receiver_network_SUITE).


%% Callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2, init_per_testcase/2, end_per_testcase/2]).

%% Testcases
-export([sender/1]).
-export([sender_no_ack/1]).
-export([sender_rx_loop_reply/1]).
-export([receiver/1]).
-export([receiver_callback/1]).
-export([receiver_rx_loop_reply/1]).
-export([outsider/1]).
-export([sender_busy_medium/1]).
-export([jammer/1]).
-export([fwd_sender/1, fwd_mid/1, fwd_receiver/1]).

%--- Includes ------------------------------------------------------------------

-include("mac_frame.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%--- CT functions --------------------------------------------------------------

all() -> [
         %  {group, simple_tx_rx},
         %  {group, tx_rx_multiple_nodes},
         %  {group, tx_rx_ack},
         %  {group, tx_rx_no_ack},
         %  {group, rx_loop_on},
          {group, tx_rx_loop_on_reply},
          {group, busy_medium},
          {group, forwarding}
         ].

groups() -> [
             {simple_tx_rx, [parallel], [sender, receiver]},
             {tx_rx_multiple_nodes, [parallel], [sender, receiver, outsider]},
             {tx_rx_ack, [parallel], [sender, receiver, outsider]},
             {tx_rx_no_ack, [parallel], [sender_no_ack, outsider]},
             {rx_loop_on, [parallel], [sender, receiver_callback]},
             {tx_rx_loop_on_reply, [parallel], [sender_rx_loop_reply, receiver_rx_loop_reply]},
             {busy_medium, [parallel], [sender_busy_medium, jammer]},
             {forwarding, [parallel], [fwd_sender, fwd_mid, fwd_receiver]}
            ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(busy_medium, Config) ->
    {NetPid, Network} = ieee802154_node:boot_network_node(),
    [{net_pid, NetPid}, {network, Network} | Config];

init_per_group(tx_rx_no_ack, Config) -> init_per_group(tx_rx_ack, Config); % Config is the same

init_per_group(tx_rx_ack, Config) ->
    ExpectedFrame = {#frame_control{src_addr_mode = ?EXTENDED, dest_addr_mode = ?EXTENDED, ack_req = ?ENABLED}, #mac_header{src_addr = <<16#CAFEDECA00000001:64>>, dest_addr = <<16#CAFEDECA00000002:64>>}, <<"Test">>},
    {NetPid, Network} = ieee802154_node:boot_network_node(),
    [{net_pid, NetPid}, {network, Network}, {expected_frame, ExpectedFrame} | Config];

init_per_group(tx_rx_loop_on_reply, Config) ->
    ExpectedFrame = {#frame_control{src_addr_mode = ?EXTENDED, dest_addr_mode = ?EXTENDED, ack_req = ?ENABLED}, #mac_header{src_addr = <<16#CAFEDECA00000001:64>>, dest_addr = <<16#CAFEDECA00000002:64>>}, <<"Test">>},
    Reply = {#frame_control{src_addr_mode = ?EXTENDED, dest_addr_mode = ?EXTENDED, ack_req = ?ENABLED}, #mac_header{src_addr = <<16#CAFEDECA00000002:64>>, dest_addr = <<16#CAFEDECA00000001:64>>}, <<"Reply">>},
    {NetPid, Network} = ieee802154_node:boot_network_node(),
    [{net_pid, NetPid}, {network, Network}, {expected_frame, ExpectedFrame}, {reply, Reply} | Config];

init_per_group(forwarding, Config) ->
    ExpectedPayload = <<"Forwarded frame">>,
    {NetPid, Network} = ieee802154_node:boot_network_node(),
    [{net_pid, NetPid}, {network, Network}, {expected_payload, ExpectedPayload} | Config];

init_per_group(_, Config) ->
    ExpectedFrame = {#frame_control{src_addr_mode = ?EXTENDED, dest_addr_mode = ?EXTENDED}, #mac_header{src_addr = <<16#CAFEDECA00000001:64>>, dest_addr = <<16#CAFEDECA00000002:64>>}, <<"Test">>},
    {NetPid, Network} = ieee802154_node:boot_network_node(),
    [{net_pid, NetPid}, {network, Network}, {expected_frame, ExpectedFrame} | Config].

end_per_group(_, Config) ->
    Network = ?config(network, Config),
    NetPid = ?config(net_pid, Config),
    ieee802154_node:stop_network_node(Network, NetPid).

init_per_testcase(sender_no_ack, Config) -> 
    Network = ?config(network, Config),
    NodeRef = ieee802154_node:boot_ieee802154_node(sender_no_ack, Network, mac_extended_address, <<16#CAFEDECA00000001:64>>),
    [{sender_no_ack, NodeRef} | Config];

init_per_testcase(sender, Config) ->
    Network = ?config(network, Config),
    NodeRef = ieee802154_node:boot_ieee802154_node(sender, Network, mac_extended_address, <<16#CAFEDECA00000001:64>>),
    [{sender, NodeRef} | Config];

init_per_testcase(sender_rx_loop_reply, Config) ->
    Network = ?config(network, Config),
    {_, Node} = NodeRef = ieee802154_node:boot_ieee802154_node(sender_rx_loop_reply, Network, mac_extended_address, <<16#CAFEDECA00000001:64>>,fun input_callback/4),
    init_ets_callback_table(Node),
    [{sender_rx_loop_reply, NodeRef} | Config];

init_per_testcase(receiver, Config) ->
    Network = ?config(network, Config),
    NodeRef = ieee802154_node:boot_ieee802154_node(receiver, Network, mac_extended_address, <<16#CAFEDECA00000002:64>>),
    [{receiver, NodeRef} | Config];

init_per_testcase(outsider, Config) ->
    Network = ?config(network, Config),
    NodeRef = ieee802154_node:boot_ieee802154_node(outsider, Network, mac_extended_address, <<16#CAFEDECA00000003:64>>),
    [{outsider, NodeRef} | Config];

init_per_testcase(receiver_callback, Config) ->
    Network = ?config(network, Config),
    {_, Node} = NodeRef = ieee802154_node:boot_ieee802154_node(receiver_callback, Network, mac_extended_address, <<16#CAFEDECA00000002:64>>, fun input_callback/4),
    init_ets_callback_table(Node),
    [{receiver_callback, NodeRef} | Config];

init_per_testcase(receiver_rx_loop_reply, Config) ->
    Network = ?config(network, Config),
    {_, Node} = NodeRef = ieee802154_node:boot_ieee802154_node(receiver_rx_loop_reply, Network, mac_extended_address, <<16#CAFEDECA00000002:64>>, fun input_callback/4),
    init_ets_callback_table(Node),
    [{receiver_rx_loop_reply, NodeRef} | Config];

init_per_testcase(sender_busy_medium, Config) ->
    Network = ?config(network, Config),
    {_, Node} = NodeRef = ieee802154_node:boot_ieee802154_node(sender_busy_medium, Network, mac_extended_address, <<16#CAFEDECA00000001:64>>, fun input_callback/4),
    init_ets_callback_table(Node),
    [{sender_busy_medium, NodeRef} | Config];

init_per_testcase(jammer, Config) ->
    Network = ?config(network, Config),
    {_, Node} = NodeRef = ieee802154_node:boot_ieee802154_node(jammer, Network, mac_extended_address, <<16#CAFEDECA00000001:64>>, fun input_callback/4),
    init_ets_callback_table(Node),
    [{jammer, NodeRef} | Config];

init_per_testcase(fwd_sender, Config) ->
    Network = ?config(network, Config),
    NodeRef = ieee802154_node:boot_ieee802154_node(fwd_sender, Network, mac_extended_address, <<16#CAFEDECA00000001:64>>),
    [{fwd_sender, NodeRef} | Config];

init_per_testcase(fwd_mid, Config) ->
    Network = ?config(network, Config),
    NodeRef = ieee802154_node:boot_ieee802154_node(fwd_mid, Network, mac_extended_address, <<16#CAFEDECA00000002:64>>, fun mock_top_layer:rx_frame/4),
    [{fwd_mid, NodeRef} | Config];

init_per_testcase(fwd_receiver, Config) ->
    Network = ?config(network, Config),
    NodeRef = ieee802154_node:boot_ieee802154_node(fwd_receiver, Network, mac_extended_address, <<16#CAFEDECA00000003:64>>, fun mock_top_layer:rx_frame/4),
    [{fwd_receiver, NodeRef} | Config];

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(Name, Config) ->
    {NodePid, Node} = ?config(Name, Config),
    ieee802154_node:stop_ieee802154_node(Node, NodePid).

%--- Test cases -----------------------------------------------------------------------------

sender(Config) ->
    ct:sleep(100),
    {_, Node} = ?config(sender, Config),
    {FrameControl, MacHeader, Payload} = get_expected_frame(Config),
    {ok, _} = erpc:call(Node, ieee802154, transmission, [{FrameControl, MacHeader, Payload}]).

receiver(Config) ->
    {_, Node} = ?config(receiver, Config),
    {FrameControl, MacHeader, Payload} = get_expected_frame(Config),
    {ok, {FrameControl, MacHeader, Payload}} = erpc:call(Node, ieee802154, reception, []).

receiver_callback(Config) -> 
    {_, Node} = ?config(receiver_callback, Config),
    ExpectedFrame = get_expected_frame(Config),
    ok = erpc:call(Node, ieee802154, rx_on, []),
    {error, rx_already_on} = erpc:call(Node, ieee802154, reception, []),
    ct:sleep(200),
    [{nb_rx_frames, 1}] = erpc:call(Node, ets, lookup, [callback_table, nb_rx_frames]),
    [{_, [ExpectedFrame]}] = erpc:call(Node, ets, lookup, [callback_table, rx_frames]),
    {error, rx_already_on} = erpc:call(Node, ieee802154, rx_on, []),
    ok = erpc:call(Node, ieee802154, rx_off, []),
    ok = erpc:call(Node, ieee802154, rx_off, []).

outsider(Config) ->
    {_, Node} = ?config(outsider, Config),
    {error, affrej} = erpc:call(Node, ieee802154, reception, []). % The outsider shall receive the ACK but not the dataframe

% in this test case, the sender is expecting an ACK but never receives one
sender_no_ack(Config) ->
    ct:sleep(100),
    {_, Node} = ?config(sender_no_ack, Config),
    {FrameControl, MacHeader, Payload} = get_expected_frame(Config),
    {error, no_ack} = erpc:call(Node, ieee802154, transmission, [{FrameControl, MacHeader, Payload}]).

%--- TX RX loop reply group
sender_rx_loop_reply(Config) ->
     ct:sleep(100),
    {_, Node} = ?config(sender_rx_loop_reply, Config),
    {FrameControl, MacHeader, Payload} = get_expected_frame(Config),
    Reply = ?config(reply, Config),
    ok = erpc:call(Node, ieee802154, rx_on, []),
    {ok, _} = erpc:call(Node, ieee802154, transmission, [{FrameControl, MacHeader, Payload}]),
    ct:sleep(200),
    ok = erpc:call(Node, ieee802154, rx_off, []),
    [{nb_rx_frames, NbRxFrames}] = erpc:call(Node, ets, lookup, [callback_table, nb_rx_frames]), % When ack is received, rx_loop and callbacks are off => doesn't count in the simulation as a received frame
    ?assertEqual(1, NbRxFrames),
    [{_, [RxFrame]}] = erpc:call(Node, ets, lookup, [callback_table, rx_frames]),
    ?assertEqual(Reply, RxFrame).

receiver_rx_loop_reply(Config) ->
    {_, Node} = ?config(receiver_rx_loop_reply, Config),
    ExpectedFrame = get_expected_frame(Config),
    {FrameControl, MacHeader, Payload} = ?config(reply, Config),
    ok = erpc:call(Node, ieee802154, rx_on, []),
    ct:sleep(200),
    [{nb_rx_frames, 1}] = erpc:call(Node, ets, lookup, [callback_table, nb_rx_frames]),
    [{_, [ExpectedFrame]}] = erpc:call(Node, ets, lookup, [callback_table, rx_frames]),
    {ok, _} = erpc:call(Node, ieee802154, transmission, [{FrameControl, MacHeader, Payload}]),
    ok = erpc:call(Node, ieee802154, rx_off, []).

%--- unslotted CSMA-CA test case group
sender_busy_medium(Config) -> 
    {_, Node} = ?config(sender_busy_medium, Config),
    timer:sleep(100),
    Frame = [{#frame_control{src_addr_mode = ?EXTENDED, dest_addr_mode = ?EXTENDED}, #mac_header{src_addr = <<16#CAFEDECA00000001:64>>, dest_addr = <<16#CAFEDECA00000002:64>>}, <<"Test - this frame shouldn't be transmitted">>}],
    Ret = erpc:call(Node, ieee802154, transmission, Frame),
    ?assertEqual({error, channel_access_failure}, Ret).

jammer(Config) -> 
    {_, Node} = ?config(jammer, Config),
    jammer_loop(Node, 50).

%--- Forwarding test
fwd_sender(Config) ->
    {_, Node} = ?config(fwd_sender, Config),
    Payload = ?config(expected_payload, Config),
    SrcAddr = erpc:call(Node, ieee802154, get_pib_attribute, [mac_extended_address]),
    lists:map(fun(Seqnum) ->
                      FC = #frame_control{src_addr_mode = ?EXTENDED, dest_addr_mode = ?EXTENDED, ack_req = ?ENABLED},
                      MH = #mac_header{seqnum = Seqnum, src_addr = SrcAddr, dest_addr = <<16#CAFEDECA00000002:64>>},
                      Frame = {FC, MH, Payload},
                      Ret  = erpc:call(Node, ieee802154, transmission, [Frame]),
                      Comment = lists:flatten(io_lib:format("Frames seqnum #~w", [Seqnum])),
                      ?assertMatch({ok, _}, Ret, Comment)
              end, lists:seq(1, 10)).

fwd_receiver(Config) ->
    {_, Node} = ?config(fwd_receiver, Config),
    ok = erpc:call(Node, ieee802154, rx_on, []),
    NodeAddr = erpc:call(Node, ieee802154, get_pib_attribute, [mac_extended_address]),
    ct:sleep(10000),
    verify_frame_seq(Node, NodeAddr, 11, 10).

fwd_mid(Config) ->
    {_, Node} = ?config(fwd_mid, Config),
    ok = erpc:call(Node, ieee802154, rx_on, []),
    NodeAddr = erpc:call(Node, ieee802154, get_pib_attribute, [mac_extended_address]),
    ct:sleep(10000),
    verify_frame_seq(Node, NodeAddr, 1, 10).

%--- Internal -------------------------------------------------------------------------------

get_expected_frame(Config) ->
    ?config(expected_frame, Config).

input_callback({FC, _MH, _Payload} = Frame, _LQI, _Security, _Ranging) ->
    case FC#frame_control.frame_type of
        ?FTYPE_DATA ->
            [{_, NbRxFrames}] = ets:lookup(callback_table, nb_rx_frames),
            ets:insert(callback_table, {nb_rx_frames, NbRxFrames+1}),
            [{_, Frames}] = ets:lookup(callback_table, rx_frames),
            ets:insert(callback_table, {rx_frames, [Frame | Frames]});
        _ ->
            ok
    end.

init_ets_callback_table(Node) ->
    erpc:call(Node, ets, insert, [callback_table, {nb_rx_frames, 0}]), % ets table is created by the mock phy
    erpc:call(Node, ets, insert, [callback_table, {rx_frames, []}]).

jammer_loop(_, 0) -> 
    ok;
jammer_loop(Node, N) ->
    ok = erpc:call(Node, mock_phy_network, transmit, [<<"Jamming frame">>, #tx_opts{}]),
    timer:sleep(5),
    jammer_loop(Node, N-1).

verify_frame_seq(Node, DestAddr, FirstSeqnum, ExpNbrFrames) ->
    {ok ,Frames} = erpc:call(Node, mock_top_layer, dump, []),
    Seqnums = lists:map(fun({_, MH, _}) -> MH#mac_header.seqnum end, Frames),
    Comment = lists:flatten(io_lib:format("seqnums received: ~w", [Seqnums])),
    ?assertEqual(ExpNbrFrames, length(Frames), Comment),
    lists:foldl(fun({_, MH, _}, ExpectedSeqnum) ->
                        Seqnum = MH#mac_header.seqnum,
                        FrameDestAddr = MH#mac_header.dest_addr,
                        ?assertEqual(ExpectedSeqnum, Seqnum),
                        ?assertEqual(DestAddr, FrameDestAddr),
                        ExpectedSeqnum + 1
                end, FirstSeqnum, Frames).