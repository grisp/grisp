-module(lowpan_ipv6).
-export([buildIpv6UdpPacket/3, buildIpv6Header/1, buildUdpHeader/1, getHeader/1]).
-export([buildIpv6Packet/2]).

-record(ipv6_header, {
    version = 2#0110, % 4-bit (version 6)
    traffic_class,  % 8-bit 
    flow_label,  % 20-bit 
    payload_length, % 16-bit 
    next_header,  % 8-bit 
    hop_limit,  % 8-bit 
    source_address, % 128-bit
    destination_address % 128-bit 
}).

-record(udp_header, {
    source_port, % 16-bit identifies the sender's port
    destination_port,  % 16-bit identifies the receiver's port and is required
    length,  % 16-bit indicates the length in bytes of the UDP datagram
    checksum % 16-bit may be used for error-checking of the header and data
}).

%-------------------------------------------------------------------------------
%% @doc Returns an IPv6 header in binary format
%% @spec build_ipv6_header(#ipv6_header{}) -> binary().
%-------------------------------------------------------------------------------
-spec buildIpv6Header(#ipv6_header{}) -> binary().
buildIpv6Header(IPv6Header) ->
    #ipv6_header{
        version =  _Version,
        traffic_class = Traffic_class, 
        flow_label = Flow_label, 
        payload_length = Payload_length,
        next_header = Next_header, 
        hop_limit = Hop_limit, 
        source_address = SourceAddr,
        destination_address = DestAddr
    } = IPv6Header,

    <<6:4,Traffic_class:8,Flow_label:20,Payload_length:16,Next_header:8,Hop_limit:8,SourceAddr/binary,DestAddr/binary>>.

%-------------------------------------------------------------------------------
%% @doc Extracts the IPv6 header from a packet
%% @spec get_header(binary()) -> binary().
%-------------------------------------------------------------------------------
-spec getHeader(binary()) -> binary().
getHeader(Ipv6Pckt) ->
    <<Header:320, _/bitstring>> = Ipv6Pckt, 
    <<Header:320>>.

%-------------------------------------------------------------------------------
%% @doc Returns a UDP header in binary format
%% @spec build_udp_header(#udp_header{}) -> binary().
%-------------------------------------------------------------------------------
-spec buildUdpHeader(#udp_header{}) -> binary().
buildUdpHeader(UdpHeader) ->
    #udp_header{
        source_port =  SourcePort,
        destination_port = DestinationPort, 
        length = Length, 
        checksum = Checksum
    } = UdpHeader,

    <<SourcePort:16,DestinationPort:16,Length:16,Checksum:16>>.

%-------------------------------------------------------------------------------
%% @doc Builds an IPv6 packet with the given header and payload
%% @spec buildIpv6Packet(#ipv6_header{}, binary()) -> binary().
%-------------------------------------------------------------------------------
-spec buildIpv6Packet(#ipv6_header{}, binary()) -> binary().
buildIpv6Packet(IPv6Header, Payload) ->
    Header = buildIpv6Header(IPv6Header),
    IPv6Packet = <<Header/binary, Payload/bitstring>>,
    IPv6Packet.

%-------------------------------------------------------------------------------
%% @doc Builds an IPv6 packet with the given IPv6 header, UDP header, and payload
%% @spec build_ipv6_udp_packet(#ipv6_header{}, #udp_header{}, binary()) -> binary().
%-------------------------------------------------------------------------------
-spec buildIpv6UdpPacket(#ipv6_header{}, #udp_header{}, binary()) -> binary().
buildIpv6UdpPacket(IPv6Header, UdpHeader, Payload) ->
    IpHeader = buildIpv6Header(IPv6Header),
    UdpH = buildUdpHeader(UdpHeader),
    IPv6Packet = <<IpHeader/binary, UdpH/binary, Payload/bitstring>>,
    IPv6Packet.
