-module(lowpan_test_SUITE).

-include("lowpan_utils.hrl").

-export([all/0, init_per_testcase/1, end_per_testcase/1]).
-export([
    pkt_encapsulation_test/1, fragmentation_test/1, datagram_info_test/1,
    reassemble_fragments_list_test/1, reassemble_single_fragments_test/1,
    reassemble_full_ipv6_pckt_test/1, compress_header_example1_test/1,
    compress_header_example2_test/1, link_local_addr_pckt_comp/1,
    multicast_addr_pckt_comp/1, global_context_pckt_comp1/1, udp_nh_pckt_comp/1,
    tcp_nh_pckt_comp/1, icmp_nh_pckt_comp/1, unc_ipv6/1, iphc_pckt_16bit_addr/1,
    iphc_pckt_64bit_addr/1, msh_pckt/1, extended_EUI64_from_64mac/1, extended_EUI64_from_48mac/1,
    extended_EUI64_from_16mac/1, check_tag_unicity/1, link_local_from_16mac/1, multicast_addr_validity/1, 
    broadcast_pckt/1
]).
-export([cooja_example3/1]).
-export([cooja_example2/1]).
-export([cooja_example1/1]).


all() ->
    [
        pkt_encapsulation_test,
        datagram_info_test,
        reassemble_fragments_list_test,
        reassemble_single_fragments_test,
        reassemble_full_ipv6_pckt_test,
        compress_header_example1_test,
        compress_header_example2_test,
        link_local_addr_pckt_comp,
        multicast_addr_pckt_comp,
        global_context_pckt_comp1,
        udp_nh_pckt_comp,
        tcp_nh_pckt_comp,
        icmp_nh_pckt_comp,
        unc_ipv6,
        iphc_pckt_64bit_addr,
        iphc_pckt_16bit_addr,
        msh_pckt, extended_EUI64_from_64mac,extended_EUI64_from_48mac,
        extended_EUI64_from_16mac, check_tag_unicity, link_local_from_16mac, 
        multicast_addr_validity, broadcast_pckt
        %cooja_example1, cooja_example2, cooja_example3
    ].

init_per_testcase(Config) ->
    Config.

end_per_testcase(_Config) ->
    ok.

%-------------------------------------------------------------------------------
%                       6LoWPAN Packet Encapsulation
%-------------------------------------------------------------------------------

pkt_encapsulation_test(_Config) ->
    Payload = <<"This is an Ipv6 pckt">>,
    IPv6Header =
        #ipv6_header{
            version = 6,
            traffic_class = 0,
            flow_label = 0,
            payload_length = byte_size(Payload),
            next_header = 17,
            hop_limit = 64,
            source_address = <<1>>,
            destination_address = <<2>>
        },
    IPv6Packet = lowpan_ipv6:buildIpv6Packet(IPv6Header, Payload),
    DhTypebinary = <<?IPV6_DHTYPE:8, 0:16>>,
    ToCheck = <<DhTypebinary/binary, IPv6Packet/binary>>,
    ToCheck = lowpan_core:pktEncapsulation(IPv6Header, Payload),
    ok.


unc_ipv6(_Config) ->
    Ipv6Pckt = lowpan_ipv6:buildIpv6Packet(?IPv6Header, ?Payload),

    Expected = <<?IPV6_DHTYPE:8, Ipv6Pckt/bitstring>>,
    Expected = lowpan_core:getUncIpv6(Ipv6Pckt).

iphc_pckt_16bit_addr(_Config) ->
    Node1Addr = lowpan_core:generateLLAddr(<<16#0001:16>>),
    Node2Addr = lowpan_core:generateLLAddr(<<16#0002:16>>),
    IPv6Header =
        #ipv6_header{
            version = 6,
            traffic_class = 0,
            flow_label = 0,
            payload_length = byte_size(?Payload),
            next_header = 12,
            hop_limit = 64,
            source_address = Node1Addr,
            destination_address = Node2Addr
        },
    Ipv6Pckt = lowpan_ipv6:buildIpv6Packet(IPv6Header,?Payload),

    InlineData = <<12:8>>,
    ExpectedHeader =
        <<?IPHC_DHTYPE:3, 3:2, 0:1, 2:2, 0:1, 0:1, 3:2, 0:1, 0:1, 3:2, InlineData/binary>>,

    % Create the IPHC packet
    {IPHC, _} = lowpan_core:compressIpv6Header(Ipv6Pckt, false),
    io:format("IPHC: ~p~n", [IPHC]),
    io:format("ExpectedHeader: ~p~n", [ExpectedHeader]),
    IPHC = ExpectedHeader.

iphc_pckt_64bit_addr(_Config) ->
    InlineData = <<12:8, (?node1_addr)/binary, (?node2_addr)/binary>>,
    ExpectedHeader =
        <<?IPHC_DHTYPE:3, 3:2, 0:1, 2:2, 0:1, 0:1, 1:2, 0:1, 0:1, 1:2, InlineData/binary>>,

    % Create the IPHC packet
    {IPHC, _} = lowpan_core:compressIpv6Header(?Ipv6Pckt, false),
    io:format("IPHC: ~p~n", [IPHC]),
    io:format("ExpectedHeader: ~p~n", [ExpectedHeader]),
    IPHC = ExpectedHeader.

msh_pckt(_Config) ->
    MeshHeader =
        #mesh_header{
            v_bit = 0,
            f_bit = 0,
            hops_left = 14,
            originator_address = ?Node1MacAddress,
            final_destination_address = ?Node2MacAddress
        },

    BinMeshHeader = lowpan_core:buildMeshHeader(MeshHeader),
    ExpectedHeader =
        <<?MESH_DHTYPE:2, 0:1, 0:1, 14:4, ?Node1MacAddress/binary, ?Node2MacAddress/binary>>,

    ExpectedHeader = BinMeshHeader.


broadcast_pckt(_Config) ->
    DestMacAddr = lowpan_core:generateEUI64MacAddr(<<16#1234:16>>),
    MeshHeader =
        #mesh_header{
            v_bit = 0,
            f_bit = 0,
            hops_left = 14,
            originator_address = ?Node1MacAddress,
            final_destination_address = DestMacAddr
        },

    BinMeshHeader = lowpan_core:buildMeshHeader(MeshHeader),

    DestAddr = <<16#FF02:16, 0:64, 1:16, 16#FF00:16, 16#1234:16>>,
    DestAddress = binary:decode_unsigned(DestAddr),
    {_, BroadcastHeader, _} = lowpan_core:getNextHop(?Node1MacAddress, ?Node1MacAddress, DestMacAddr, DestAddress, 3, false),

    ExpectedHeader = <<BinMeshHeader/bitstring, ?BC0_DHTYPE, 3:8>>,

    io:format("Expected: ~p~n~nReceived: ~p~n", [ExpectedHeader, BroadcastHeader]),
    ExpectedHeader = BroadcastHeader.

%-------------------------------------------------------------------------------
%                         Ipv6 Packet Compression
%-------------------------------------------------------------------------------

%--- Basic IPHC test case

% Link-local address
link_local_addr_pckt_comp(_Config) ->
    Payload = <<"Testing basic IPHC compression with link-local address">>,
    IPv6Header =
        #ipv6_header{
            version = 6,
            traffic_class = 0,
            flow_label = 0,
            payload_length = byte_size(Payload),
            next_header = 0,
            hop_limit = 64,
            source_address = <<16#FE80:16, 0:48, ?Node1MacAddress/binary>>,
            destination_address = <<16#FE80:16, 0:48, ?Node2MacAddress/binary>>
        },
    Ipv6Pckt = lowpan_ipv6:buildIpv6Packet(IPv6Header, Payload),

    Tf = 2#11,
    Nh = 0,
    Hlim = 2#10,
    Cid = 0,
    Sac = 0,
    Sam = 2#01,
    M = 0,
    Dac = 0,
    Dam = 2#01,
    ExpectedCarriedInline =
        #{
            "SAM" => <<?Node1MacAddress/binary>>,
            "DAM" => <<?Node2MacAddress/binary>>,
            "NextHeader" => 0
        },

    InlineData =
        <<0:8, ?Node1MacAddress/binary,
            ?Node2MacAddress/binary>>,
    ExpectedHeader =
        <<?IPHC_DHTYPE:3, Tf:2, Nh:1, Hlim:2, Cid:1, Sac:1, Sam:2, M:1, Dac:1, Dam:2, InlineData/binary>>,

    {CompressedHeader, CarriedInlineData} = lowpan_core:compressIpv6Header(Ipv6Pckt, false),
    io:format("Expected ~p~nReceived ~p~n", [ExpectedHeader, CompressedHeader]),
    ExpectedHeader = CompressedHeader,

    ExpectedCarriedInline = CarriedInlineData,
    ok.

% Multicast address
multicast_addr_pckt_comp(_Config) ->
    Payload = <<"Testing basic IPHC compression with multicast address">>,
    IPv6Header =
        #ipv6_header{
            version = 6,
            traffic_class = 0,
            flow_label = 2,
            payload_length = byte_size(Payload),
            %UDP
            next_header = 0,
            hop_limit = 1,
            source_address = <<16#FE80:16, 0:48, ?Node1MacAddress/binary>>,
            destination_address = <<16#FF02:16, 0:48, ?Node2MacAddress/binary>>
        },

    Ipv6Pckt = lowpan_ipv6:buildIpv6Packet(IPv6Header, Payload),

    Tf = 2#01,
    Nh = 0,
    Hlim = 2#01,
    Cid = 0,
    Sac = 0,
    Sam = 2#01,
    M = 1,
    Dac = 0,
    Dam = 2#00,

    Dest = IPv6Header#ipv6_header.destination_address,
    ExpectedCarriedInline =
        #{
            "SAM" => <<?Node1MacAddress/binary>>,
            "DAM" => <<Dest/binary>>,
            "NextHeader" => 0,
            "ECN" => 0,
            "FlowLabel" => 2
        },

    
    InlineData =
        <<0:2, 0:2, 2:20, 0:8, ?Node1MacAddress/binary,
            Dest/binary>>,

    ExpectedHeader =
        <<?IPHC_DHTYPE:3, Tf:2, Nh:1, Hlim:2, Cid:1, Sac:1, Sam:2, M:1, Dac:1, Dam:2, InlineData/binary>>,

    {CompressedHeader, CarriedInlineData} = lowpan_core:compressIpv6Header(Ipv6Pckt, false),
    io:format("Expected ~p~nReceived ~p~n", [ExpectedHeader, CompressedHeader]),
    ExpectedHeader = CompressedHeader,

    ExpectedCarriedInline = CarriedInlineData,
    ok.

%---Global contexts test case, affected fields are cid, sac and dac
global_context_pckt_comp1(_Config) ->
    Payload = <<"Testing basic IPHC compression with multicast address">>,
    Source_address = <<16#2001:16, 0:48, ?Node1MacAddress/binary>>,
    Destination_address = <<16#2001:16, 0:48, ?Node2MacAddress/binary>>,
    IPv6Header =
        #ipv6_header{
            version = 6,
            traffic_class = 0,
            flow_label = 3,
            payload_length = byte_size(Payload),
            %UDP
            next_header = 0,
            hop_limit = 255,
            source_address = Source_address,
            destination_address = Destination_address
        },

    Ipv6Pckt = lowpan_ipv6:buildIpv6Packet(IPv6Header, Payload),

    Tf = 2#01,
    Nh = 0,
    Hlim = 2#11,
    Cid = 0,
    Sac = 1,
    Sam = 2#00,
    M = 0,
    Dac = 1,
    Dam = 2#00,

    ExpectedCarriedInline =
        #{ 
            "SAM" => Source_address,
            "NextHeader" => 0,
            "ECN" => 0,
            "FlowLabel" => 3,
            "DAM" => Destination_address
        },
    io:format("ExpectedCarriedInline: ~p~n", [ExpectedCarriedInline]),
    
    InlineData =
        <<0:2, 0:2, 3:20, 0:8, Source_address/binary, Destination_address/binary >>,
    ExpectedHeader =
        <<?IPHC_DHTYPE:3, Tf:2, Nh:1, Hlim:2, Cid:1, Sac:1, Sam:2, M:1, Dac:1, Dam:2, InlineData/binary>>,

    {CompressedHeader, CarriedInlineData} = lowpan_core:compressIpv6Header(Ipv6Pckt, false),
    io:format("Expected ~p~nReceived ~p~n", [ExpectedHeader, CompressedHeader]),
    ExpectedHeader = CompressedHeader,

    ExpectedCarriedInline = CarriedInlineData,
    ok.

%---Different types of Next Headers test case
udp_nh_pckt_comp(_Config) ->
    Payload = <<"Testing basic IPHC compression with link-local address">>,

    PayloadLength = byte_size(Payload),
    Source_address = <<16#FE80:16, 0:48, ?Node1MacAddress/binary>>,
    Destination_address = <<16#FE80:16, 0:48, ?Node2MacAddress/binary>>,

    UdpPckt = <<1025:16, 61617:16, 25:16, 16#f88c:16>>,

    Ipv6Pckt =
        <<6:4, 0:8, 0:20, PayloadLength:16, 17:8, 64:8, Source_address/binary, Destination_address/binary, UdpPckt/binary, Payload/binary>>,

    Tf = 2#11,
    Nh = 1,
    Hlim = 2#10,
    Cid = 0,
    Sac = 0,
    Sam = 2#01,
    M = 0,
    Dac = 0,
    Dam = 2#01,
    C = 0,
    P = 2#01,
    ExpectedCarriedInline = #{"SAM" => <<?Node1MacAddress/binary>>, "DAM" => <<?Node2MacAddress/binary>>},

    InlineData = <<?Node1MacAddress/binary, ?Node2MacAddress/binary>>,
    UdpInline = <<1025:16, 177:8, 63628:16>>,

    io:format("UdpInline ~p~n", [UdpInline]),
    ExpectedHeader =
        <<?IPHC_DHTYPE:3, Tf:2, Nh:1, Hlim:2, Cid:1, Sac:1, Sam:2, M:1, Dac:1, Dam:2, InlineData/binary, ?UDP_DHTYPE:5, C:1, P:2, UdpInline/binary>>,

    Pckt = <<Ipv6Pckt/binary, UdpPckt/binary>>,
    {CompressedHeader, CarriedInlineData} = lowpan_core:compressIpv6Header(Pckt, false),

    io:format("Expected ~p~nReceived ~p~n", [ExpectedHeader, CompressedHeader]),
    ExpectedHeader = CompressedHeader,

    ExpectedCarriedInline = CarriedInlineData,
    ok.

tcp_nh_pckt_comp(_Config) ->
    Payload = <<"Testing basic IPHC compression with link-local address">>,
    IPv6Header =
        #ipv6_header{
            version = 6,
            traffic_class = 0,
            flow_label = 0,
            payload_length = byte_size(Payload),
            % TCP
            next_header = 6,
            hop_limit = 64,
            source_address = <<16#FE80:16, 0:48, ?Node1MacAddress/binary>>,
            destination_address = <<16#FE80:16, 0:48,?Node2MacAddress/binary>>
        },

    Ipv6Pckt = lowpan_ipv6:buildIpv6Packet(IPv6Header, Payload),

    Tf = 2#11,
    Nh = 0,
    Hlim = 2#10,
    Cid = 0,
    Sac = 0,
    Sam = 2#01,
    M = 0,
    Dac = 0,
    Dam = 2#01,
    ExpectedCarriedInline =
        #{
            "SAM" => <<?Node1MacAddress/binary>>,
            "DAM" => <<?Node2MacAddress/binary>>,
            "NextHeader" => 6
        },

    InlineData = <<6:8, ?Node1MacAddress/binary, ?Node2MacAddress/binary>>,
    ExpectedHeader =
        <<?IPHC_DHTYPE:3, Tf:2, Nh:1, Hlim:2, Cid:1, Sac:1, Sam:2, M:1, Dac:1, Dam:2, InlineData/binary>>,

    {CompressedHeader, CarriedInlineData} = lowpan_core:compressIpv6Header(Ipv6Pckt, false),

    io:format("Expected ~p~nReceived ~p~n", [ExpectedHeader, CompressedHeader]),
    ExpectedHeader = CompressedHeader,

    ExpectedCarriedInline = CarriedInlineData,
    ok.

icmp_nh_pckt_comp(_Config) ->
    Payload = <<"Testing basic IPHC compression with link-local address">>,
    IPv6Header =
        #ipv6_header{
            version = 6,
            traffic_class = 0,
            flow_label = 0,
            payload_length = byte_size(Payload),
            %ICMPv6
            next_header = 58,
            hop_limit = 255,
            source_address = <<16#FE80:16, 0:48, ?Node1MacAddress/binary>>,
            destination_address = <<16#FE80:16, 0:48, ?Node2MacAddress/binary>>
        },

    Ipv6Pckt = lowpan_ipv6:buildIpv6Packet(IPv6Header, Payload),

    Tf = 2#11,
    Nh = 0,
    Hlim = 2#11,
    Cid = 0,
    Sac = 0,
    Sam = 2#01,
    M = 0,
    Dac = 0,
    Dam = 2#01,
    ExpectedCarriedInline =
        #{
            "SAM" => <<?Node1MacAddress/binary>>,
            "DAM" => <<?Node2MacAddress/binary>>,
            "NextHeader" => 58
        },

    InlineData = <<58:8, ?Node1MacAddress/binary, ?Node2MacAddress/binary>>,
    ExpectedHeader =
        <<?IPHC_DHTYPE:3, Tf:2, Nh:1, Hlim:2, Cid:1, Sac:1, Sam:2, M:1, Dac:1, Dam:2, InlineData/binary>>,

    {CompressedHeader, CarriedInlineData} = lowpan_core:compressIpv6Header(Ipv6Pckt, false),

    io:format("Expected ~p~nReceived ~p~n", [ExpectedHeader, CompressedHeader]),
    ExpectedHeader = CompressedHeader,

    ExpectedCarriedInline = CarriedInlineData,
    ok.

%---Online resource (https://www.youtube.com/watch?v=0JMVO3HN0xo&t=778s)
compress_header_example1_test(_Config) ->
    Payload = <<"Hello world this is an ipv6 packet">>,
    PayloadLength = byte_size(Payload),

    SrcAddress = <<16#FE80:16, 0:48, 16#020164FFFE2FFC0A:64>>,
    DstAddress = <<16#FF02:16, 0:48, 16#0000000000000001:64>>,
    Ipv6Pckt =
        <<6:4, 224:8, 0:20, PayloadLength:16, 58:8, 255:8, SrcAddress/binary, DstAddress/binary, Payload/bitstring>>,

    Tf = 2#10,
    Nh = 0,
    Hlim = 2#11,
    Cid = 0,
    Sac = 0,
    Sam = 2#11,
    M = 1,
    Dac = 0,
    Dam = 2#11,
    ExpectedCarriedInline =
        #{
            "DAM" => <<1>>,
            "NextHeader" => 58,
            "TrafficClass" => 224
        },
    InlineData = <<0:2, 56:6, 58:8, 1:8>>,
    ExpectedHeader =
        <<?IPHC_DHTYPE:3, Tf:2, Nh:1, Hlim:2, Cid:1, Sac:1, Sam:2, M:1, Dac:1, Dam:2, InlineData/binary>>,

    {CompressedHeader, CarriedInlineData} = lowpan_core:compressIpv6Header(Ipv6Pckt, false),
    io:format("Expected ~p~nReceived ~p~n", [ExpectedHeader, CompressedHeader]),

    ExpectedHeader = CompressedHeader,

    ExpectedCarriedInline = CarriedInlineData,
    ok.

compress_header_example2_test(_Config) ->
    Payload = <<"Hello world this is an ipv6 packet">>,
    PayloadLength = byte_size(Payload),

    SrcAddress = <<16#2001066073013728:64, 16#0223DFFFFEA9F7AC:64>>,
    DstAddress = <<16#2001A45040070803:64, 16#0000000000001004:64>>,
    Ipv6Pckt =
        <<6:4, 0:8, 0:20, PayloadLength:16, 6:8, 64:8, SrcAddress/binary, DstAddress/binary, Payload/binary>>,

    Tf = 2#11,
    Nh = 0,
    Hlim = 2#10,
    Cid = 0,
    Sac = 1,
    Sam = 2#00,
    M = 0,
    Dac = 1,
    Dam = 2#00,
    InlineData = <<6:8, SrcAddress/binary, DstAddress/binary>>,
    ExpectedHeader =
        <<?IPHC_DHTYPE:3, Tf:2, Nh:1, Hlim:2, Cid:1, Sac:1, Sam:2, M:1, Dac:1, Dam:2, InlineData/binary>>,

    {CompressedHeader, _} = lowpan_core:compressIpv6Header(Ipv6Pckt, false),
    io:format("Expected ~p~nActual ~p~n", [ExpectedHeader, CompressedHeader]),

    ExpectedHeader = CompressedHeader,

    ok.

%-------------------------------------------------------------------------------
%                      6LoWPAN IPv6 Packet Fragmentation
%-------------------------------------------------------------------------------

fragmentation_test(_Config) ->
    % fragmentation test based on the computation of the size of all fragment payloads
    Payload = <<"This is an Ipv6 pckt">>,
    IPv6Header =
        #ipv6_header{
            version = 6,
            traffic_class = 0,
            flow_label = 0,
            payload_length = byte_size(Payload),
            next_header = 17,
            hop_limit = 64,
            source_address = <<1>>,
            destination_address = <<2>>
        },
    IPv6Pckt = lowpan_ipv6:buildIpv6Packet(IPv6Header, Payload),
    Fragments = lowpan_core:fragmentIpv6Packet(IPv6Pckt, byte_size(Payload), false),
    ReassembledSize =
        lists:foldl(fun({_, Fragment}, Acc) -> byte_size(Fragment) + Acc end, 0, Fragments),
    Psize = byte_size(IPv6Pckt),
    Psize = ReassembledSize,
    ok.

datagram_info_test(_Config) ->
    Data = <<"payload">>,
    Fragment = <<?FRAG1_DHTYPE:5, 1000:11, 12345:16, Data/bitstring>>,

    DtgInfo = lowpan_core:datagramInfo(Fragment),
    FragType = DtgInfo#datagramInfo.fragtype,
    DatagramSize = DtgInfo#datagramInfo.datagramSize,
    DatagramTag = DtgInfo#datagramInfo.datagramTag,
    DatagramOffset = DtgInfo#datagramInfo.datagramOffset,
    Payload = DtgInfo#datagramInfo.payload,

    io:format("~p~n", [Payload]),

    ?FRAG1_DHTYPE = FragType,
    1000 = DatagramSize,
    12345 = DatagramTag,
    0 = DatagramOffset,
    Data = Payload,
    ok.

%-------------------------------------------------------------------------------
%                          Ipv6 Packet Reassembly
%-------------------------------------------------------------------------------

reassemble_fragments_list_test(_Config) ->
    Data = <<"Hello World!">>,
    PayloadLen = byte_size(Data),
    Datagram = #datagram{
        tag = 25,
        size = PayloadLen,
        cmpt = PayloadLen,
        fragments = #{0 => <<"Hello ">>, 1 => <<"World!">>},
        timer = erlang:system_time(second)
    },

    Reassembled = lowpan_core:reassemble(Datagram),
    <<"Hello World!">> = Reassembled,
    ok.

reassemble_single_fragments_test(_Config) ->
    Data = <<"Hello World!">>,
    PayloadLen = byte_size(Data),

    DatagramMap = ets:new(datagram_map_test, [named_table, public]),
    {Result1, _Map1} = lowpan_core:storeFragment(DatagramMap, {<<1>>, 25}, 0, <<"Hello ">>, erlang:system_time(second), PayloadLen, 25, self()),
    incomplete_first = Result1,

    {Result2, _Map2} = lowpan_core:storeFragment(DatagramMap, {<<1>>, 25}, 1, <<"World!">>, erlang:system_time(second), PayloadLen, 25, self()),
    complete = Result2,

    Reassembled = lowpan_core:reassemble(#datagram{
                                        tag = 25,
                                        size = PayloadLen,
                                        cmpt = PayloadLen,
                                        timer = erlang:system_time(second),
                                        fragments = #{0 => <<"Hello ">>, 1 => <<"World!">>}
                                    }),
    Data = Reassembled,
    ets:delete(DatagramMap),
    ok.

reassemble_full_ipv6_pckt_test(_Config) ->
    Payload = lowpan_core:generateChunks(),
    IPv6Header =
        #ipv6_header{
            version = 6,
            traffic_class = 0,
            flow_label = 0,
            payload_length = byte_size(Payload),
            next_header = 17,
            hop_limit = 64,
            source_address = <<1:128>>,
            destination_address = <<2:128>>
        },

    Ipv6Pckt = lowpan_ipv6:buildIpv6Packet(IPv6Header, Payload),
    io:format("Original pckt size ~p bytes~n", [byte_size(Ipv6Pckt)]),
    FragmentList = lowpan_core:fragmentIpv6Packet(Ipv6Pckt, byte_size(Ipv6Pckt), false),

    DatagramMap = ets:new(datagram_map_test, [named_table, public]),

    lists:foreach(
        fun({FragHeader, FragPayload}) ->
            Offset = case byte_size(FragHeader) of 
                4 -> %first frag
                    0;
                5 -> 
                    <<_:32, Ofset:8>> = FragHeader, 
                    Ofset
            end,

            io:format("Storing fragment with offset ~p~n", [Offset]),
            {Result, _Map} = lowpan_core:storeFragment(DatagramMap, {<<1>>, 25}, Offset, FragPayload, erlang:system_time(second), byte_size(Ipv6Pckt), 25, self()),
            io:format("Fragment stored result: ~p~n", [Result])
        end,
        FragmentList
    ),

    Datagram = ets:lookup_element(DatagramMap, {<<1>>, 25}, 2),
    io:format("Datagram after storing fragments: ~p~n", [Datagram]),

    Reassembled = lowpan_core:reassemble(Datagram),
    io:format("Reassembled: ~p~nIpv6Pckt: ~p~n", [Reassembled, Ipv6Pckt]),

    case Ipv6Pckt of
        Reassembled -> io:format("Reassembly successful.~n");
        _ -> io:format("Reassembly failed.~n")
    end,

    % Nettoyage
    ets:delete(DatagramMap),
    ok.


%-------------------------------------------------------------------------------
%                               Additionnal tests
%-------------------------------------------------------------------------------
extended_EUI64_from_48mac(_Config)->
    MacAddr = <<16#9865FD361453:48>>, 
    Expected = <<16#9A65FDFFFE361453:64>>,
    Result = lowpan_core:getEUI64From48bitMac(MacAddr), 
    io:format("Expected ~p~nResult ~p~n",[Expected, Result]),
    Result = Expected.  

extended_EUI64_from_64mac(_Config)->
    MacAddr = <<16#00124B0006386C1A:64>>, 
    Expected = <<16#02124B0006386C1A:64>>,
    Result = lowpan_core:getEUI64FromExtendedMac(MacAddr), 
    io:format("Expected ~p~nResult ~p~n",[Expected, Result]),
    Result = Expected. 

extended_EUI64_from_16mac(_Config)->
    MacAddr = <<16#0001:16>>, 
    Expected = <<16#FDFF:16, 0:8, 16#FFFE:16, 0:8, 16#0001:16>>,
    Result = lowpan_core:getEUI64FromShortMac(MacAddr), 
    io:format("Expected ~p~nResult ~p~n",[Expected, Result]),
    Result = Expected. 

link_local_from_16mac(_Config)->
    MacAddr = <<16#0001:16>>, 
    Expected = <<16#FE80:16, 0:48,16#FDFF:16, 0:8, 16#FFFE:16, 0:8, 16#0001:16>>,
    Result = lowpan_core:generateLLAddr(MacAddr), 
    io:format("Expected ~p~nResult ~p~n",[Expected, Result]),
    Result = Expected. 

check_tag_unicity(_Config) ->
    TagMap = #{},
    Tag1 = 10,
    {_, NewTagMap} = lowpan_core:checkTagUnicity(TagMap, Tag1),
    Tag2 = 10,
    {ValidTag2, FinalMap} = lowpan_core:checkTagUnicity(NewTagMap, Tag2),
    io:format("TagMap: ~p~n", [FinalMap]),
    ValidTag2 =/= Tag2.
    

multicast_addr_validity(_Config) ->
    Ipv6Addr = <<16#FF02:16, 0:64, 1:16, 16#FF00:16, 16#1234:16>>, 
    GenAddr = lowpan_core:generateMulticastAddr(Ipv6Addr), 
    ExpectedAddr = <<16#9234:16>>, 
    io:format("ExpectedAddr ~p~nGenAddr ~p~n", [ExpectedAddr, GenAddr]),
    GenAddr = ExpectedAddr.
    

%-------------------------------------------------------------------------------
% Contiki-ng cooja packet example tests
%-------------------------------------------------------------------------------
cooja_example1(_Config)->
    Payload = <<"Cooja example 1">>,
    PayloadLength = byte_size(Payload),

    Source_address = <<16#FE80:16, 0:48, 207:16, 7:16, 7:16, 7:16>>,
    Destination_address = <<16#FF02:16, 0:48, 0:48,16#1a:16>>,

    Ipv6Pckt =
        <<6:4, 0:8, 0:20, PayloadLength:16, 58:8, 64:8, Source_address/binary, Destination_address/binary, Payload/binary>>,

    Tf = 2#11,
    Nh = 0,
    Hlim = 2#10,
    Cid = 0,
    Sac = 0,
    Sam = 2#11,
    M = 1,
    Dac = 0,
    Dam = 2#11,
    <<_:120,Last8:8>> = Destination_address,
    ExpectedCarriedInline = #{"NextHeader" => 58, "DAM"=><<Last8:8>>},

    InlineData = <<58:8, Last8:8>>,
    ExpectedHeader =
        <<?IPHC_DHTYPE:3, Tf:2, Nh:1, Hlim:2, Cid:1, Sac:1, Sam:2, M:1, Dac:1, Dam:2, InlineData/binary>>,

    CH = {Tf, Nh, Hlim, Cid, Sac, Sam, M, Dac, Dam, InlineData},
    io:format("CH: ~p~n",[CH]),
    io:format("Expected IPHC carried values: ~p~n", [ExpectedCarriedInline]),
    Pckt = <<Ipv6Pckt/binary>>,
    {CompressedHeader, CarriedInlineData} = lowpan_core:compressIpv6Header(Pckt, false),

   

    %io:format("Expected ~p~nReceived ~p~n", [ExpectedHeader, CompressedHeader]),
    ExpectedHeader = CompressedHeader,

    ExpectedCarriedInline = CarriedInlineData,
    ok.

cooja_example2(_Config)->
    Payload = <<"Cooja example 2">>,
    PayloadLength = byte_size(Payload),

    Source_address = <<16#FE80:16, 0:48, 202:16, 2:16, 2:16, 2:16>>,
    Destination_address = <<16#FE80:16, 0:48, 212:16, 7402:16, 2:16, 2:16>>,

    UdpPckt = <<5683:16, 5683:16, 37:16, 16#8441:16>>,

    Ipv6Pckt =
        <<6:4, 0:8, 0:20, PayloadLength:16, 17:8, 64:8, Source_address/binary, Destination_address/binary, UdpPckt/binary, Payload/binary>>,

    Tf = 2#11,
    Nh = 1,
    Hlim = 2#10,
    Cid = 0,
    Sac = 0,
    Sam = 2#11,
    M = 0,
    Dac = 0,
    Dam = 2#11,
    C = 0,
    P = 2#00,
    ExpectedCarriedInline = #{},

    %InlineData = <<?Node1MacAddress/binary, ?Node2MacAddress/binary>>,
    UdpInline = <<5683:16, 5683:8, 8441:16>>,

    io:format("Expected UdpInline ~p~n", [UdpInline]),
    ExpectedHeader =
        <<?IPHC_DHTYPE:3, Tf:2, Nh:1, Hlim:2, Cid:1, Sac:1, Sam:2, M:1, Dac:1, Dam:2, ?UDP_DHTYPE:5, C:1, P:2, UdpInline/binary>>,

    %CH = {Tf, Nh, Hlim, Cid, Sac, Sam, M, Dac, Dam, UdpInline},
    io:format("Expected IPHC carried values: ~p~n", [ExpectedCarriedInline]),
    Pckt = <<Ipv6Pckt/binary, UdpPckt/binary>>,
    {CompressedHeader, CarriedInlineData} = lowpan_core:compressIpv6Header(Pckt, false),

    %io:format("Expected ~p~nReceived ~p~n", [ExpectedHeader, CompressedHeader]),
    ExpectedHeader = CompressedHeader,

    ExpectedCarriedInline = CarriedInlineData,
    ok.

cooja_example3(_Config)->
    Payload = <<"Cooja example 3">>,
    PayloadLength = byte_size(Payload),

    Source_address = <<0:64, 207:16, 7:16, 7:16, 7:16>>,
    Destination_address = <<0:64, 202:16, 2:16, 2:16, 2:16>>,


    Ipv6Pckt =
        <<6:4, 0:8, 0:20, PayloadLength:16, 43:8, 63:8, Source_address/binary, Destination_address/binary, Payload/binary>>,

    Tf = 2#11,
    Nh = 1,
    Hlim = 2#00,
    Cid = 1,
    Sac = 1,
    Sam = 2#01,
    M = 0,
    Dac = 1,
    Dam = 2#11,

    <<_:64, Last64S:64>> = Source_address, 

    ExpectedCarriedInline = #{"HopLimit"=>63, "NH"=>43, "SAM" => <<Last64S:64>>},

    InlineData = <<?Node1MacAddress/binary, ?Node2MacAddress/binary>>,

    ExpectedHeader =
        <<?IPHC_DHTYPE:3, Tf:2, Nh:1, Hlim:2, Cid:1, Sac:1, Sam:2, M:1, Dac:1, Dam:2, InlineData/binary>>,

    %CH = {Tf, Nh, Hlim, Cid, Sac, Sam, M, Dac, Dam, InlineData},
    io:format("Expected IPHC carried values: ~p~n", [ExpectedCarriedInline]),
    Pckt = <<Ipv6Pckt/binary>>,
    
    {CompressedHeader, CarriedInlineData} = lowpan_core:compressIpv6Header(Pckt, false),
    
    %io:format("Expected ~p~nReceived ~p~n", [ExpectedHeader, CompressedHeader]),
    ExpectedHeader = CompressedHeader,

    ExpectedCarriedInline = CarriedInlineData,
    ok.
