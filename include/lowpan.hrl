-include("ieee802154.hrl").
-include("mac_frame.hrl").

%-------------------------------------------------------------------------------
% Useful records
%-------------------------------------------------------------------------------
-record(ipv6PckInfo,
        {version = 6,
         trafficClass,
         flowLabel,
         payloadLength,
         nextHeader,
         hopLimit,
         sourceAddress,
         destAddress,
         payload}).
-record(datagramInfo, {fragtype, datagramSize, datagramTag, datagramOffset, payload}).


%-------------------------------------------------------------------------------
% Dispatch Type and Header
%-------------------------------------------------------------------------------

%@doc dispatch value bit pattern from rfc4944, DH stands for dispatch header

-define(NALP_DHTYPE, 2#00). % Not a LoWPAN frame, such packet shall be discarded
-define(IPV6_DHTYPE, 2#01000001). % Uncompressed IPv6 Addresses
-define(IPHC_DHTYPE, 2#011).      %  LOWPAN_IPHC compressed IPv6 (RFC6282)
-define(BC0_DHTYPE, 2#01010000). % LOWPAN_BC0 broadcast
-define(ESC_DHTYPE, 2#01111111). % Additional Dispatch byte follows
-define(MESH_DHTYPE, 2#10). % Mesh Header
-define(FRAG1_DHTYPE, 2#11000). % Frist fragmentation Header
-define(FRAGN_DHTYPE, 2#11100). % Subsequent fragmentation Header
-define(UDP_DHTYPE, 2#11110). % UDP header compression

-define(Oxf0b, 2#111100001011).
-define(Oxf0, 2#11110000).

-type dispatch_type() ::
    ?NALP_DHTYPE |
    ?IPV6_DHTYPE |
    ?IPHC_DHTYPE |
    ?BC0_DHTYPE  |
    ?ESC_DHTYPE  |
    ?MESH_DHTYPE |
    ?FRAG1_DHTYPE|
    ?FRAGN_DHTYPE.

%-------------------------------------------------------------------------------
% Fragmentation Type and Header
%-------------------------------------------------------------------------------

-type frag_type() :: ?FRAG1_DHTYPE | ?FRAGN_DHTYPE.

-record(frag_header,
        {frag_type = ?FRAG1_DHTYPE :: frag_type(),
         datagram_size, % 11 bits 
         datagram_tag, % 16 bits 
         datagram_offset}). % 8-bits 

-record(frag_info,
        {datagram_size,
         datagram_tag,
         datagram_offset
        }). 

-record(datagram,
        {tag,
         size,
         cmpt,
         timer,
         fragments
         }).

-define(MAX_FRAME_SIZE,80). % Because IEEE 802.15.4 leaves approximately 80-100 bytes of payload
-define(MAX_FRAG_SIZE_NoMESH,75). % Because max frame size is 80 bytes, and lowpan header 30 bytes (5 bytes for fragHeader) 8 bytes are from IPHC which is included in payload for frag
-define(MAX_FRAG_SIZE_MESH,58). % Considering max frame size = 80 bytes, lowpan header = 30 bytes (17 bytes for meshHeader, 5 bytes for fragHeader, 8 bytes for IPHC)
-define(MAX_DTG_SIZE, 2047). % 11 bits datagram_size
-define(REASSEMBLY_TIMEOUT, 60000). % 60 sec
-define(FRAG_HEADER_SIZE,5). % 5 bytes including frag_type, datagram_size, datagram_tag, and datagram_offset
-define(DATAGRAMS_MAP,#{}). % map of received datagrams, the keys are the tag of datagrams
-define(MAX_TAG_VALUE, 65535).
-define(DEFAULT_TAG_VALUE, 2#0000000000000000).
-define(BC_SEQNUM, 2#00000000).

-record(additional_info, 
        {datagram_size, 
         datagram_tag,
         hops_left,
         timer
        }). 
-define(INFO_ON, 1).
-define(INFO_OFF, 0).

%-------------------------------------------------------------------------------
% Header Compression
%-------------------------------------------------------------------------------
-record(ipv6_header,
        {version = 2#0110, % 4-bit
         traffic_class, % 8-bit 
         flow_label, % 20-bit 
         payload_length, % 16-bit 
         next_header, % 8-bit 
         hop_limit, % 8-bit 
         source_address, % 128-bit 
         destination_address}). % 128-bit
-record(udp_header,
        {source_port, % 16-bit 
         destination_port,  % 16-bit 
         length,  % 16-bit 
         checksum}). % 16-bit
-record(iphc_header,
        {dispatch = ?IPHC_DHTYPE, % 3-bit dispatch value
         tf, % 2-bit field for Traffic Class and Flow Control compression options
         nh, % 1-bit field for Next Header encoding using NHC
         hlim, % 2-bit field for Hop Limit compression
         cid, % 1-bit field for Context Identifier Extension
         sac, % 1-bit field for Source Address Compression (stateless or stateful)
         sam, % 2-bit field for Source Address Mode
         m, % 1-bit field for Multicast Compression
         dac, % 1-bit field for Destination Address Compression (stateless or stateful)
         dam}). % 2-bit field for Destination Address Mode

-define(LINK_LOCAL_PREFIX, 16#FE80).
-define(MULTICAST_PREFIX, 16#FF02).
-define(GLOBAL_PREFIX_1, 16#2001).
-define(GLOBAL_PREFIX_3, 16#2003).
-define(MESH_LOCAL_PREFIX, 16#FD00).
-type prefix_type() :: ?LINK_LOCAL_PREFIX | ?GLOBAL_PREFIX_1 | ?MULTICAST_PREFIX.


-define(UDP_PN, 17). % PN stands for Protocol Number
-define(TCP_PN, 6).
-define(ICMP_PN, 58).

% inspired from Thread Usage of 6LoWPAN
-define(Context_id_table, 
        #{1 => <<?MESH_LOCAL_PREFIX:16,16#0DB8:16, 0:32>>, % mesh local prefix
          2 => <<0:64>>, % cooja mesh local prefix
          %2 => <<?GLOBAL_PREFIX_1:16, 0:48>>, % global prefix 1
          3 => <<?GLOBAL_PREFIX_3:16, 0:48>>}). % global prefix 3
          
-define(Prefixt_id_table, 
        #{<<?MESH_LOCAL_PREFIX:16, 0:48>> => 1 , % mesh local prefix
        <<0:64>> => 2, % cooja mesh local prefix
       % <<?GLOBAL_PREFIX_1:16, 0:48>> => 2, % global prefix 1
        <<?GLOBAL_PREFIX_3:16, 0:48>> => 3}). % global prefix 3
                                                                                                                                                                                                                                                                                                                                                          % add more context prefix
-define(SHORT_ADDR_LEN, 2).
-define(EXTENDED_ADDR_LEN, 8).

%-------------------------------------------------------------------------------
% Routing
%-------------------------------------------------------------------------------

-define(BroadcastAdd, <<"ÿÿ">>).
-define(ACK_FRAME, <<>>).

-record(mesh_header,
        {mesh_type = ?MESH_DHTYPE,
         v_bit, % 1-bit 
         f_bit, % 1-bit 
         hops_left, % 4-bit 
         originator_address, % link-layer address of the Originator
         final_destination_address  % link-layer address of the Final Destination
         %deep_hops_left = undefined 
        }).

-record(meshInfo,
        {version = 6,
         v_bit, 
         f_bit, 
         hops_left, 
         originator_address, 
         final_destination_address,
         deep_hops_left,
         payload}).


-define(Max_Hops, 2#1110). 
-define(DeepHopsLeft, 16#F). % 0xF
-define(Max_DeepHopsLeft, 2#11111111). % 8-bit Deep Hops Left

-define(Node1MacAddress, <<16#CAFEDECA00000001:64>>).
-define(Node2MacAddress, <<16#CAFEDECA00000002:64>>).
-define(Node3MacAddress, <<16#CAFEDECA00000003:64>>).
-define(Node4MacAddress, <<16#CAFEDECA00000004:64>>).
-define(Node5MacAddress, <<16#CAFEDECA00000005:64>>).

% Used to test 16-bit node addresses 
% -define(Node1MacAddress, <<16#0001:16>>).
% -define(Node2MacAddress, <<16#0002:16>>).
% -define(Node3MacAddress, <<16#0003:16>>).


-define(node1_addr,
        lowpan_core:generateEUI64MacAddr(?Node1MacAddress)).
-define(node2_addr, 
        lowpan_core:generateEUI64MacAddr(?Node2MacAddress)).
-define(node3_addr,
        lowpan_core:generateEUI64MacAddr(?Node3MacAddress)).
-define(node4_addr,
        lowpan_core:generateEUI64MacAddr(?Node4MacAddress)).
-define(node5_addr,
        lowpan_core:generateEUI64MacAddr(?Node5MacAddress)).

-define(Default_routing_table,
        #{?node1_addr => ?node1_addr,
          ?node2_addr => ?node2_addr,
          ?node3_addr => ?node3_addr, 
          ?node4_addr => ?node4_addr, 
          ?node5_addr => ?node5_addr}).

% -define(Node1_routing_table,
%         #{?node1_addr => ?node1_addr,
%           ?node2_addr => ?node3_addr,
%           ?node3_addr => ?node2_addr}).

-define(Node1_routing_table, % 5 node routing test: 1 -> 2 -> 5
        #{?node5_addr => ?node2_addr, 
          ?node2_addr => ?node2_addr, 
          ?node3_addr => ?node3_addr, 
          ?node4_addr => ?node4_addr}).

-define(Node2_routing_table, % 5 node routing test: 2 -> 3 -> 5
        #{?node5_addr => ?node3_addr}).

-define(Node3_routing_table, % 5 node routing test: 3 -> 4 -> 5
        #{?node5_addr => ?node4_addr}).

-define(Node4_routing_table, % 5 node routing test: 4 -> 5
        #{?node5_addr => ?node5_addr}).

-define(Node5_routing_table, % 5 node routing test
        #{?node5_addr => ?node5_addr}).



%-------------------------------------------------------------------------------
% Metrics
%-------------------------------------------------------------------------------
-record(metrics, 
        {ack_counter = 0, 
        fragments_nbr = 1, 
        start_time = 0, 
        end_time = 0,
        pckt_len = 0, 
        compressed_pckt_len = 0}).
