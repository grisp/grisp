-include("lowpan.hrl").

%-------------------------------------------------------------------------------
% Common value for testing purpose
%-------------------------------------------------------------------------------

-define(Payload, <<"Hello world this is an ipv6 packet for testing purpose">>).
-define(BigPayload, lowpan_core:generateChunks()).
-define(PayloadLength, byte_size(?Payload)).

-define(Node1Address, lowpan_core:generateLLAddr(?Node1MacAddress)). % generates a link local address based on the mac address
-define(Node2Address, lowpan_core:generateLLAddr(?Node2MacAddress)).
-define(Node3Address, lowpan_core:generateLLAddr(?Node3MacAddress)).
-define(Node4Address, lowpan_core:generateLLAddr(?Node4MacAddress)).
-define(Node5Address, lowpan_core:generateLLAddr(?Node5MacAddress)).

-define(IPv6Header, #ipv6_header{
    version = 6,
    traffic_class = 0,
    flow_label = 0,
    payload_length = ?PayloadLength,
    next_header = 12,
    hop_limit = 64,
    source_address = ?Node1Address,
    destination_address = ?Node2Address
}).

-define(IPv6Header3, #ipv6_header{
    version = 6,
    traffic_class = 0,
    flow_label = 0,
    payload_length = ?PayloadLength,
    next_header = 12,
    hop_limit = 64,
    source_address = ?Node1Address,
    destination_address = ?Node3Address
}).

-define(IPv6Header4, #ipv6_header{
    version = 6,
    traffic_class = 0,
    flow_label = 0,
    payload_length = ?PayloadLength,
    next_header = 12,
    hop_limit = 64,
    source_address = ?Node1Address,
    destination_address = ?Node4Address
}).

-define(IPv6Header5, #ipv6_header{
    version = 6,
    traffic_class = 0,
    flow_label = 0,
    payload_length = ?PayloadLength,
    next_header = 12,
    hop_limit = 64,
    source_address = ?Node1Address,
    destination_address = ?Node5Address
}).

-define(FrameControl, #frame_control{
    frame_type = ?FTYPE_DATA,
    src_addr_mode = ?EXTENDED,
    dest_addr_mode = ?EXTENDED
}).
-define(Ipv6Pckt, lowpan_ipv6:buildIpv6Packet(?IPv6Header, ?Payload)).
-define(MacHeader, #mac_header{src_addr = ?Node1MacAddress, dest_addr = ?Node2MacAddress}).


%-------------------------------------------------------------------------------
% multiple hop Routing tables
%-------------------------------------------------------------------------------

-define(Node1_multiple_hop_routing_table,
        #{?node4_addr => ?node2_addr}).

-define(Node2_multiple_hop_routing_table,
        #{?node4_addr => ?node3_addr}).

-define(Node3_multiple_hop_routing_table,
         #{?node4_addr => ?node4_addr}).

-define(Node4_multiple_hop_routing_table,
         #{?node4_addr => ?node4_addr,
          ?node3_addr => ?node3_addr}).

