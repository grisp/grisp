-module(lowpan_core).

-include("lowpan.hrl").

-export([
    pktEncapsulation/2, fragmentIpv6Packet/3,
    reassemble/1, storeFragment/8, createIphcPckt/2, getIpv6Pkt/2, datagramInfo/1,
    compressIpv6Header/2, buildDatagramPckt/2, buildFirstFragPckt/5,
    getPcktInfo/1, getIpv6Payload/1, triggerFragmentation/3,
    decodeIpv6Pckt/4, encodeInteger/1,
    tupleToBin/1, buildFragHeader/1, getNextHop/6,
    generateChunks/0, generateChunks/1,
    buildMeshHeader/1, getMeshInfo/1, containsMeshHeader/1,
    buildFirstFragHeader/1, getUncIpv6/1, getEUI64From48bitMac/1, generateLLAddr/1, 
    getEUI64MacAddr/1, createNewMeshHeader/3, createNewMeshDatagram/3, removeMeshHeader/2, 
    convertAddrToBin/1,checkTagUnicity/2, get16bitMacAddr/1, generateMulticastAddr/1,
    getDecodeIpv6PcktInfo/1, getNextHop/2, generateEUI64MacAddr/1
]).
-export([getEUI64FromShortMac/1]).
-export([getEUI64FromExtendedMac/1]).

%-------------------------------------------------------------------------------
%% @doc Returns an Ipv6 packet
%% @spec getIpv6Pkt(Header, Payload) -> binary().
%-------------------------------------------------------------------------------
-spec getIpv6Pkt(Header, Payload) -> binary() when
      Header :: binary(),
      Payload :: binary().
getIpv6Pkt(Header, Payload) ->
    lowpan_ipv6:buildIpv6Packet(Header, Payload).

%-------------------------------------------------------------------------------
%% @doc create an uncompressed IPv6 packet 
%% @spec pktEncapsulation(Header, Payload) -> binary().
%-------------------------------------------------------------------------------
-spec pktEncapsulation(Header, Payload) -> binary() when
      Header :: binary(),
      Payload :: binary().
pktEncapsulation(Header, Payload) ->
    Ipv6Pckt = getIpv6Pkt(Header, Payload),
    DhTypebinary = <<?IPV6_DHTYPE:8, 0:16>>,
    <<DhTypebinary/binary, Ipv6Pckt/binary>>.

%-------------------------------------------------------------------------------
%% @doc Encapsulates an Uncompressed IPv6 packet
%% @spec getUncIpv6(Ipv6Pckt) -> binary().
%-------------------------------------------------------------------------------
-spec getUncIpv6(Ipv6Pckt) -> binary() when
      Ipv6Pckt :: binary().
getUncIpv6(Ipv6Pckt) ->
    <<?IPV6_DHTYPE:8, Ipv6Pckt/bitstring>>.

%-------------------------------------------------------------------------------
%
%                               Header compression
%
%-------------------------------------------------------------------------------

%-------------------------------------------------------------------------------
%% @doc Compresses an Ipv6 packet header according to the IPHC compression scheme
%% @spec compressIpv6Header(Ipv6Pckt, RouteExist) -> {binary(), map()}.
%% @returns a tuple containing the compressed header in binary form 
%% and the values that should be carried inline
%-------------------------------------------------------------------------------
-spec compressIpv6Header(Ipv6Pckt, RouteExist) -> {binary(), map()} when
      Ipv6Pckt :: binary(),
      RouteExist :: boolean().
compressIpv6Header(Ipv6Pckt, RouteExist) ->
    PcktInfo = getPcktInfo(Ipv6Pckt),

    TrafficClass = PcktInfo#ipv6PckInfo.trafficClass,
    FlowLabel = PcktInfo#ipv6PckInfo.flowLabel,
    NextHeader = PcktInfo#ipv6PckInfo.nextHeader,
    HopLimit = PcktInfo#ipv6PckInfo.hopLimit,
    SourceAddress = PcktInfo#ipv6PckInfo.sourceAddress,
    DestAddress = PcktInfo#ipv6PckInfo.destAddress,

    Map = #{},
    List = [],

    {CID, UpdateMap0, UpdatedList0} =
        encodeCid(SourceAddress, DestAddress, Map, List),

    {TF, UpdateMap1, UpdatedList1} =
        encodeTf(TrafficClass, FlowLabel, UpdateMap0, UpdatedList0),
    
    {NH, UpdateMap2, UpdatedList2} = encodeNh(NextHeader, UpdateMap1, UpdatedList1),
    
    {HLIM, UpdateMap3, UpdatedList3} = encodeHlim(HopLimit, UpdateMap2, UpdatedList2),
    
    SAC = encodeSac(SourceAddress),
    
    {SAM, UpdateMap4, UpdatedList4} =
        encodeSam(CID, SAC, SourceAddress, UpdateMap3, UpdatedList3, RouteExist),
    
    M = encodeM(DestAddress),
    
    DAC = encodeDac(DestAddress),
    
    {DAM, CarrInlineMap, CarrInlineList} =
        encodeDam(CID, M, DAC, DestAddress, UpdateMap4, UpdatedList4, RouteExist),

    %CH = {TF, NH, HLIM, CID, SAC, SAM, M, DAC, DAM, CarrInlineList},

    CarrInlineBin = list_to_binary(CarrInlineList),
    % io:format("Actual carried values: ~p ~n",[CarrInlineMap]),
    case NextHeader of
        ?UDP_PN ->
            UdpPckt = getUdpData(Ipv6Pckt),
            CompressedUdpHeaderBin = compressUdpHeader(UdpPckt, []),
            io:format("Lowpan core: UdpInline: ~p ~n",[CompressedUdpHeaderBin]),
            CompressedHeader =
                <<?IPHC_DHTYPE:3, TF:2, NH:1, HLIM:2, CID:1, SAC:1, SAM:2, M:1, DAC:1, DAM:2, CarrInlineBin/binary, CompressedUdpHeaderBin/binary>>,
            {CompressedHeader, CarrInlineMap};
        _ ->
            CompressedHeader =
                <<?IPHC_DHTYPE:3, TF:2, NH:1, HLIM:2, CID:1, SAC:1, SAM:2, M:1, DAC:1, DAM:2, CarrInlineBin/binary>>,
            {CompressedHeader, CarrInlineMap}
    end.

%-------------------------------------------------------------------------------
%% @private
%% @doc Encodes the TrafficClass and Flow label fields
%% @spec encodeTf(TrafficClass, FlowLabel, CarrInlineMap, CarrInlineList) -> {integer(), map(), list()}.
%% @returns a tuple containing the compressed values and the CarrInline values
%-------------------------------------------------------------------------------
-spec encodeTf(TrafficClass, FlowLabel, CarrInlineMap, CarrInlineList) -> {term(), map(), list()} when
      TrafficClass :: integer(),
      FlowLabel :: integer(),
      CarrInlineMap :: map(),
      CarrInlineList :: list().
encodeTf(TrafficClass, FlowLabel, CarrInlineMap, CarrInlineList) ->
    <<DSCP:6, ECN:2>> = <<TrafficClass:8>>,

    case {ECN, DSCP, FlowLabel} of
        {0, 0, 0} ->
            % Traffic Class and Flow Label are elided
            {2#11, CarrInlineMap, CarrInlineList};

        {_, 0, _} ->
            % DSCP is elided
            UpdatedMap = CarrInlineMap#{"ECN" => ECN, "FlowLabel" => FlowLabel},
            Bin = <<ECN:2, 0:2, FlowLabel:20>>, % 24 bits tot (RFC 6282 - pg12)
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            {2#01, UpdatedMap, UpdatedList};

        {_, _, 0} ->
            % Flow Label is elided
            UpdatedMap = CarrInlineMap#{"TrafficClass" => TrafficClass},
            Bin = <<ECN:2, DSCP:6>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            {2#10, UpdatedMap, UpdatedList};

        _ ->
            % ECN, DSCP, and Flow Label are carried inline
            UpdatedMap = CarrInlineMap#{"TrafficClass" => TrafficClass, "FlowLabel" => FlowLabel},
            Bin = <<ECN:2, DSCP:6, 0:4, FlowLabel:20>>, % 32 bits tot (RFC 6282 - pg12)
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            {2#00, UpdatedMap, UpdatedList}
    end.

%-------------------------------------------------------------------------------
%% @private
%% @doc Encodes the NextHeader field
%% @doc NextHeader specifies whether or not the next header is encoded using NHC
%% @spec encodeNh(NextHeader, CarrInlineMap, CarrInlineList)->{integer(), map(), list()}.
%% @returns a tuple containing the compressed value and the CarrInline values
%-------------------------------------------------------------------------------
-spec encodeNh(NextHeader, CarrInlineMap, CarrInlineList) -> {integer(), map(), list()} when
      NextHeader :: integer(),
      CarrInlineMap :: map(),
      CarrInlineList :: list().
encodeNh(NextHeader, CarrInlineMap, CarrInlineList) when NextHeader == ?UDP_PN ->
    {1, CarrInlineMap, CarrInlineList};
encodeNh(NextHeader, CarrInlineMap, CarrInlineList) when NextHeader == ?TCP_PN ->
    Bin = <<NextHeader:8>>,
    L = [Bin],
    UpdatedList = [CarrInlineList, L],
    {0, CarrInlineMap#{"NextHeader" => ?TCP_PN}, UpdatedList};
encodeNh(NextHeader, CarrInlineMap, CarrInlineList) when NextHeader == ?ICMP_PN ->
    Bin = <<NextHeader:8>>,
    L = [Bin],
    UpdatedList = [CarrInlineList, L],
    {0, CarrInlineMap#{"NextHeader" => ?ICMP_PN}, UpdatedList};
encodeNh(NextHeader, CarrInlineMap, CarrInlineList) ->
    Bin = <<NextHeader:8>>,
    L = [Bin],
    UpdatedList = [CarrInlineList, L],
    {0, CarrInlineMap#{"NextHeader" => NextHeader}, UpdatedList}.

%-------------------------------------------------------------------------------
%% @private
%% @doc Encodes the HopLimit field
%% @spec encodeHlim(HopLimit, CarrInlineMap, CarrInlineList) -> {integer(), map(), list()}.
%% @returns a tuple containing the compressed value and the CarrInline values
%-------------------------------------------------------------------------------
-spec encodeHlim(HopLimit, CarrInlineMap, CarrInlineList) -> {integer(), map(), list()} when
      HopLimit :: integer(),
      CarrInlineMap :: map(),
      CarrInlineList :: list().
encodeHlim(HopLimit, CarrInlineMap, CarrInlineList) when HopLimit == 1 ->
    {2#01, CarrInlineMap, CarrInlineList};
encodeHlim(HopLimit, CarrInlineMap, CarrInlineList) when HopLimit == 64 ->
    {2#10, CarrInlineMap, CarrInlineList};
encodeHlim(HopLimit, CarrInlineMap, CarrInlineList) when HopLimit == 255 ->
    {2#11, CarrInlineMap, CarrInlineList};
encodeHlim(HopLimit, CarrInlineMap, CarrInlineList) ->
    Bin = <<HopLimit:8>>,
    L = [Bin],
    UpdatedList = CarrInlineList ++ L,
    {2#00, CarrInlineMap#{"HopLimit" => HopLimit}, UpdatedList}.

%-------------------------------------------------------------------------------
%% @private
%% @doc Encodes the Context Identifier Extension field
%% @doc If this bit is 1, an 8 bit CIE field follows after the DAM field
%% @spec encodeCid(SrcAdd, DstAdd, CarrInlineMap, CarrInlineList) -> {integer(), map(), list()}.
%% @returns a tuple containing the compressed value and the CarrInline values
%-------------------------------------------------------------------------------
-spec encodeCid(SrcAdd, DstAdd, CarrInlineMap, CarrInlineList) -> {integer(), map(), list()} when
      SrcAdd :: binary(),
      DstAdd :: binary(),
      CarrInlineMap :: map(),
      CarrInlineList :: list().
encodeCid(SrcAdd, DstAdd, CarrInlineMap, CarrInlineList) ->
    <<SrcAddPrefix:16, _/binary>> = <<SrcAdd:128>>,
    <<DstAddPrefix:16, _/binary>> = <<DstAdd:128>>,
    SrcPrefixKey = <<SrcAddPrefix:16, 0:48>>, 
    DstPrefixKey = <<DstAddPrefix:16, 0:48>>,

    % check if prefix is in contextTable
    SrcContext = maps:find(SrcPrefixKey, ?Prefixt_id_table),
    DstContext = maps:find(DstPrefixKey, ?Prefixt_id_table),

    case {SrcContext, DstContext} of
        {{ok, SrcContextId}, {ok, DstContextId}} ->
            Bin = <<SrcContextId:4, DstContextId:4>>,
            L = [Bin],
            UpdatedList = CarrInlineList ++ L,
            {1, CarrInlineMap, UpdatedList};

        {error, {ok, DstContextId}} ->
            Bin = <<0:4, DstContextId:4>>,
            L = [Bin],
            UpdatedList = CarrInlineList ++ L,
            {1, CarrInlineMap, UpdatedList};

        {{ok, SrcContextId}, error} ->
            SrcContextId = someValue,
            Bin = <<SrcContextId:4, 0:4>>,
            L = [Bin],
            UpdatedList = CarrInlineList ++ L,
            {1, CarrInlineMap, UpdatedList};

        _-> {0, CarrInlineMap, CarrInlineList}
    end.

%-------------------------------------------------------------------------------
%% @private
%% @doc Encodes the Source Address Compression
%% @doc SAC specifies whether the compression is stateless or statefull
%% @spec encodeSac(SrcAdd) -> integer().
%% @returns the compressed value
%-------------------------------------------------------------------------------
-spec encodeSac(SrcAdd) -> integer() when
      SrcAdd :: binary().
encodeSac(SrcAdd) ->
    <<Prefix:16, _/binary>> = <<SrcAdd:128>>,

    case Prefix of
        ?LINK_LOCAL_PREFIX ->
            0;
        ?MULTICAST_PREFIX ->
            0;
        _ ->
            1
    end.

%-------------------------------------------------------------------------------
%% @private
%% @doc Encodes for the Source Address Mode
%% @spec encodeSam(integer(), integer(), binary(), map(), list(), boolean()) -> {integer(), map(), list()}.
%% @returns a tuple containing the compressed value and the CarrInline values
%-------------------------------------------------------------------------------
-spec encodeSam(integer(), integer(), binary(), map(), list(), boolean()) -> {integer(), map(), list()}.
encodeSam(_CID, SAC, SrcAdd, CarrInlineMap, CarrInlineList, RouteExist) when SAC == 0 ->
    SrcAddBits = <<SrcAdd:128>>,
    <<_:112, Last16Bits:16>> = SrcAddBits,
    <<_:64, Last64Bits:64>> = SrcAddBits,

    case {SrcAddBits, RouteExist} of
        {<<?LINK_LOCAL_PREFIX:16, 0:48, _:24, 16#FFFE:16, _:24>>, _} ->
            % the address is fully elided
            {2#11, CarrInlineMap, CarrInlineList};
        {_, true} -> 
            {2#11, CarrInlineMap, CarrInlineList};

        {<<?LINK_LOCAL_PREFIX:16, 0:48, 16#000000FFFE00:48, _:16>>, _} ->
            % the first 112 bits are elided, last 16 IID bits are carried in-line
            Bin = <<Last16Bits:16>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            UpdatedMap = CarrInlineMap#{"SAM" => Last16Bits},
            {2#10, UpdatedMap, UpdatedList};

        {<<?LINK_LOCAL_PREFIX:16, 0:48, _:64>>, _} ->
            % the first 64 bits are elided, last 64 bits (IID) are carried in-line
            Bin = <<Last64Bits:64>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            UpdatedMap = CarrInlineMap#{"SAM" => Bin},
            {2#01, UpdatedMap, UpdatedList};
        {_, _} ->
            % full address is carried in-line
            Bin = <<SrcAdd:128>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            {2#00, CarrInlineMap#{"SAM" => Bin}, UpdatedList}
    end;
encodeSam(0, 1, SrcAdd, CarrInlineMap, CarrInlineList, _RouteExist) ->
    Bin = <<SrcAdd:128>>,
    L = [Bin],
    UpdatedList = [CarrInlineList, L],
    {2#00, CarrInlineMap#{"SAM" => Bin}, UpdatedList};

encodeSam(_CID, SAC, SrcAdd, CarrInlineMap, CarrInlineList, _RouteExist) when SAC == 1 ->
    SrcAddBits = <<SrcAdd:128>>,
    <<_:112, Last16Bits:16>> = SrcAddBits,
    <<_:64, Last64Bits:64>> = SrcAddBits,

    case SrcAddBits of
        <<_Prefix:16, _:48, _:24, 16#FFFE:16, _:24>> ->
            % the address is fully elided
            {2#11, CarrInlineMap, CarrInlineList};

        <<_Prefix:16, _:48, 16#000000FFFE00:48, _:16>> ->
            % the first 112 bits are elided, last 16 IID bits are carried in-line
            Bin = <<Last16Bits:16>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            UpdatedMap = CarrInlineMap#{"SAM" => Bin},
            {2#10, UpdatedMap, UpdatedList};

        <<_Prefix:16, _:48, _:64>> ->
            % the first 64 bits are elided, last 64 bits (IID) are carried in-line
            Bin = <<Last64Bits:64>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            UpdatedMap = CarrInlineMap#{"SAM" => Bin},
            {2#01, UpdatedMap, UpdatedList};

        <<0:128>> -> % The UNSPECIFIED address, ::
            {2#00, CarrInlineMap, CarrInlineList}
    end.

%-------------------------------------------------------------------------------
%% @private
%% @doc Defines the multicast compression
%% @spec encodeM(DstAdd) -> integer().
%% @returns the compressed value
%-------------------------------------------------------------------------------
-spec encodeM(DstAdd) -> integer() when
      DstAdd :: binary().
encodeM(DstAdd) ->
    <<Prefix:16, _/bitstring>> = <<DstAdd:128>>,
    case Prefix of
        ?MULTICAST_PREFIX ->
            1;
        _ ->
            0
    end.

%-------------------------------------------------------------------------------
%% @private
%% @doc encode for the Destination Address Compression
%% @spec encodeDac(DstAdd) -> integer().
%% @doc DAC specifies whether the compression is stateless or statefull
%% @returns the compressed value
%-------------------------------------------------------------------------------
-spec encodeDac(DstAdd) -> integer() when
      DstAdd :: binary().
encodeDac(DstAdd) ->
    <<Prefix:16, _/binary>> = <<DstAdd:128>>,

    case Prefix of
        ?LINK_LOCAL_PREFIX ->
            0;
        ?MULTICAST_PREFIX ->
            0;
        _ ->
            1
    end.

%-------------------------------------------------------------------------------
%% @private
%% @doc Encodes logic for the Destination Address Mode
%% @spec encodeDam(integer(), integer(), integer(), binary(), map(), 
%% list(), boolean()) -> {integer(), map(), list()}.
%% @param Cid, M, DAC, DstAdd, CarrInlineMap
%% @returns a tuple containing the compressed value and the CarrInline values
%-------------------------------------------------------------------------------
-spec encodeDam(integer(), integer(), integer(), binary(), map(), list(), boolean()) -> {integer(), map(), list()}.
encodeDam(0, 0, 0, DstAdd, CarrInlineMap, CarrInlineList, RouteExist) ->
    DestAddBits = <<DstAdd:128>>,
    <<_:112, Last16Bits:16>> = DestAddBits,
    <<_:64, Last64Bits:64>> = DestAddBits,
    
    case {DestAddBits, RouteExist} of
        {<<?LINK_LOCAL_PREFIX:16, 0:48, _:24, 16#FFFE:16, _:24>>, _} ->
            % MAC address is split into two 24-bit parts, FFFE is inserted in the middle
            {2#11, CarrInlineMap, CarrInlineList};
        {_, true} -> {2#11, CarrInlineMap, CarrInlineList};

        {<<?LINK_LOCAL_PREFIX:16, 0:48, 16#000000FFFE00:48, _:16>>, _} ->
            % the first 112 bits are elided, last 16 bits are in-line
            Bin = <<Last16Bits:16>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            UpdatedMap = CarrInlineMap#{"DAM" => Bin},
            {2#10, UpdatedMap, UpdatedList};

        {<<?LINK_LOCAL_PREFIX:16,  0:48, _:64>>, _} ->
            % the first 64 bits are elided, last 64 bits are in-line
            Bin = <<Last64Bits:64>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            UpdatedMap = CarrInlineMap#{"DAM" => Bin},
            {2#01, UpdatedMap, UpdatedList};
        {_, _} ->
            % full address is carried in-line
            Bin = <<DstAdd:128>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            {2#00, CarrInlineMap#{"DAM" => Bin}, UpdatedList}
    end;
encodeDam(1, 0, 1, DstAdd, CarrInlineMap, CarrInlineList, _RouteExist) ->
    DestAddBits = <<DstAdd:128>>,
    <<_:112, Last16Bits:16>> = DestAddBits,
    <<_:64, Last64Bits:64>> = DestAddBits,
    
    case DestAddBits of
        <<_Prefix:16, _:48, _:24, 16#FFFE:16, _:24>> ->
            % the address is fully elided
            {2#11, CarrInlineMap, CarrInlineList};

        <<_Prefix:16, _:48, 16#000000FFFE00:48, _:16>> ->
            % the first 112 bits are elided, last 16 IID bits are carried in-line
            Bin = <<Last16Bits:16>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            UpdatedMap = CarrInlineMap#{"DAM" => Bin},
            {2#10, UpdatedMap, UpdatedList};

        <<_Prefix:16, _:48, _:64>> ->
            % the first 64 bits are elided, last 64 bits (IID) are carried in-line
            Bin = <<Last64Bits:64>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            UpdatedMap = CarrInlineMap#{"DAM" => Bin},
            {2#01, UpdatedMap, UpdatedList}
    end;
encodeDam(0, 0, 1, DstAdd, CarrInlineMap, CarrInlineList, _RouteExist) ->
    Bin = <<DstAdd:128>>,
    L = [Bin],
    UpdatedList = [CarrInlineList, L],
    {2#00, CarrInlineMap#{"DAM" => Bin}, UpdatedList};
    
encodeDam(_CID, 1, 0, DstAdd, CarrInlineMap, CarrInlineList, _RouteExist) ->
    DestAddBits = <<DstAdd:128>>,
    <<_:80, Last48Bits:48>> = DestAddBits,
    <<_:96, Last32Bits:32>> = DestAddBits,
    <<_:120, Last8Bits:8>> = DestAddBits,
    case DestAddBits of
        % ff02::00XX.
        <<?MULTICAST_PREFIX:16, 0:104, _:8>> ->
            Bin = <<Last8Bits:8>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            UpdatedMap = CarrInlineMap#{"DAM" => Bin},
            {2#11, UpdatedMap, UpdatedList};

        % ffXX::00XX:XXXX.
        <<16#FF:8, _:8, 0:80, _:32>> ->
            Bin = <<Last32Bits:32>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            UpdatedMap = CarrInlineMap#{"DAM" => Bin},
            {2#10, UpdatedMap, UpdatedList};

        % ffXX::00XX:XXXX:XXXX.
        <<16#FF:8, _:8, 0:64, _:48>> ->
            Bin = <<Last48Bits:48>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            UpdatedMap = CarrInlineMap#{"DAM" => Bin},
            {2#01, UpdatedMap, UpdatedList};
        _ ->
            % full address is carried in-line
            Bin = <<DstAdd:128>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            {2#00, CarrInlineMap#{"DAM" => Bin}, UpdatedList}
    end;
encodeDam(_CID, 1, 1, DstAdd, CarrInlineMap, CarrInlineList, _RouteExist) ->
    DestAddBits = <<DstAdd:128>>,
    <<_:80, Last48Bits:48>> = DestAddBits,
    case DestAddBits of
        <<16#FF, _:112>> ->
            Bin = <<Last48Bits:48>>,
            L = [Bin],
            UpdatedList = [CarrInlineList, L],
            UpdatedMap = CarrInlineMap#{"DAM" => Bin},
            {2#00, UpdatedMap, UpdatedList}
    end.

%-------------------------------------------------------------------------------
%
%                       Next Header compression
%
%-------------------------------------------------------------------------------

%-------------------------------------------------------------------------------
%                       UDP Packet Compression
%-------------------------------------------------------------------------------

%-------------------------------------------------------------------------------
%                 Structure of a UDP Datagram Header
%
%    0                   1                   2                   3
%    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%   |       Source Port            |       Destination Port         |
%   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%   |            Length            |           Checksum             |
%   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%

-spec compressUdpHeader(UdpPckt, CarriedInline) -> binary() when
      UdpPckt :: binary(),
      CarriedInline :: list().
compressUdpHeader(UdpPckt, CarriedInline) ->
    <<SrcPort:16, DstPort:16, _Length:16, Checksum:16>> = <<UdpPckt:64>>,

    {P, CarriedInlineList} = encodeUdpPorts(SrcPort, DstPort, CarriedInline),
    {C, CarriedIn} = encodeUdpChecksum(Checksum, CarriedInlineList),

    Inline = list_to_binary(CarriedIn),

    CompressedUdpHeader = <<?UDP_DHTYPE:5, C:1, P:2, Inline/bitstring>>,
    CompressedUdpHeader.

-spec encodeUdpPorts(SrcPort, DstPort, CarriedInline) -> {integer(), list()} when
      SrcPort :: integer(),
      DstPort :: integer(),
      CarriedInline :: list().
encodeUdpPorts(SrcPort, DstPort, CarriedInline) ->
    case {<<SrcPort:16>>, <<DstPort:16>>} of
        {<<?Oxf0b:12, Last4S_Bits:4>>, <<?Oxf0b:12, Last4D_Bits:4>>} ->
            ToCarr = <<Last4S_Bits:4, Last4D_Bits:4>>,
            L = [ToCarr],
            CarriedInlineList = CarriedInline ++ L,
            P = 2#11,
            {P, CarriedInlineList};
        {<<?Oxf0:8, Last8S_Bits:8>>, _} ->
            ToCarr = <<Last8S_Bits:8, DstPort:16>>,
            L = [ToCarr],
            CarriedInlineList = CarriedInline ++ L,
            P = 2#10,
            {P, CarriedInlineList};
        {_, <<?Oxf0:8, Last8D_Bits:8>>} ->
            ToCarr = <<SrcPort:16, Last8D_Bits:8>>,
            L = [ToCarr],
            CarriedInlineList = CarriedInline ++ L,
            P = 2#01,
            {P, CarriedInlineList};
        {_, _} ->
            P = 2#00,
            ToCarr = <<SrcPort:16, DstPort:16>>,
            L = [ToCarr],
            CarriedInlineList = CarriedInline ++ L,
            {P, CarriedInlineList}
    end.

-spec encodeUdpChecksum(Checksum, CarriedInline) -> {integer(), list()} when
      Checksum :: integer(),
      CarriedInline :: list().
encodeUdpChecksum(Checksum, CarriedInline) ->
    case Checksum of
        0 ->
            {1, CarriedInline};
        %Checksum is carried inline
        _ ->
            L = [<<Checksum:16>>],
            UpdatedList = CarriedInline ++ L,
            {0, UpdatedList}
    end.

%-------------------------------------------------------------------------------
%                       ICMP Packet Compression
%-------------------------------------------------------------------------------

%-------------------------------------------------------------------------------
%                        TCP Packet Compression
%-------------------------------------------------------------------------------

%-------------------------------------------------------------------------------
%                       Packet Compression Helper
%-------------------------------------------------------------------------------

%-------------------------------------------------------------------------------
%% @doc Creates a compressed 6lowpan packet (with iphc compression) from an Ipv6 packet
%% @spec createIphcPckt(IphcHeader, Payload) -> binary().
%-------------------------------------------------------------------------------
-spec createIphcPckt(IphcHeader, Payload) -> binary() when
      IphcHeader :: binary(),
      Payload :: binary().
createIphcPckt(IphcHeader, Payload) ->
    <<IphcHeader/binary, Payload/bitstring>>.

%-------------------------------------------------------------------------------
%% @doc Returns value field of a given Ipv6 packet
%% @spec getPcktInfo(Ipv6Pckt) -> map().
%-------------------------------------------------------------------------------
-spec getPcktInfo(Ipv6Pckt) -> map() when
      Ipv6Pckt :: binary().
getPcktInfo(Ipv6Pckt) ->
    <<Version:4, TrafficClass:8, FlowLabel:20, PayloadLength:16, NextHeader:8, HopLimit:8, SourceAddress:128, DestAddress:128, Data/bitstring>> =
        Ipv6Pckt,
    
    Payload = case NextHeader of 
                ?UDP_PN -> 
                        <<_UdpFields:64, Payld/bitstring>> = Data,
                        Payld;
                _ -> Data
             end,    
    PckInfo =
        #ipv6PckInfo{
            version = Version,
            trafficClass = TrafficClass,
            flowLabel = FlowLabel,
            payloadLength = PayloadLength,
            nextHeader = NextHeader,
            hopLimit = HopLimit,
            sourceAddress = SourceAddress,
            destAddress = DestAddress,
            payload = Payload
        },
    PckInfo.

%-------------------------------------------------------------------------------
%% @doc Returns value field of a decoded Ipv6 packet
%% @spec getDecodeIpv6PcktInfo(Ipv6Pckt) -> map().
%-------------------------------------------------------------------------------
-spec getDecodeIpv6PcktInfo(Ipv6Pckt) -> map() when
      Ipv6Pckt :: binary().
getDecodeIpv6PcktInfo(Ipv6Pckt) ->
    <<TrafficClass:8, FlowLabel:24, NextHeader:8, HopLimit:8, SourceAddress:128, DestAddress:128, Data/bitstring>> =
        Ipv6Pckt,
    
    Payload = case NextHeader of 
                ?UDP_PN -> 
                        <<_UdpFields:64, Payld/bitstring>> = Data,
                        Payld;
                _ -> Data
             end,    
    PckInfo =
        #ipv6PckInfo{
            version = 6,
            trafficClass = TrafficClass,
            flowLabel = FlowLabel,
            payloadLength = byte_size(Payload),
            nextHeader = NextHeader,
            hopLimit = HopLimit,
            sourceAddress = SourceAddress,
            destAddress = DestAddress,
            payload = Payload
        },
    PckInfo.

%-------------------------------------------------------------------------------
%% @doc Returns UDP data from a given Ipv6 packet if it contains a UDP nextHeader
%% @spec getUdpData(Ipv6Pckt) -> binary().
%-------------------------------------------------------------------------------
-spec getUdpData(Ipv6Pckt) -> binary() when
      Ipv6Pckt :: binary().
getUdpData(Ipv6Pckt) ->
    <<_:320, UdpPckt:64, _/binary>> = Ipv6Pckt,
    UdpPckt.

%-------------------------------------------------------------------------------
%% @doc Returns the payload of a given Ipv6 packet
%% @spec getIpv6Payload(Ipv6Pckt) -> binary().
%-------------------------------------------------------------------------------
-spec getIpv6Payload(Ipv6Pckt) -> binary() when
      Ipv6Pckt :: binary().
getIpv6Payload(Ipv6Pckt) ->
    <<_:192, _:128, Payload/binary>> = Ipv6Pckt,
    Payload.

%-------------------------------------------------------------------------------
%% @doc Encodes an Integer value in a binary format using an appropriate amount of bit
%% @spec encodeInteger(I) -> binary().
%-------------------------------------------------------------------------------
-spec encodeInteger(I) -> binary() when
      I :: integer().
encodeInteger(I) when I =< 255 ->
    <<I:8>>;
encodeInteger(I) when I =< 65535 ->
    <<I:16>>;
encodeInteger(I) when I =< 4294967295 ->
    <<I:32>>;
encodeInteger(I) ->
    <<I:64>>.

%-------------------------------------------------------------------------------
%
%                               Packet fragmentation
%
%-------------------------------------------------------------------------------

%-------------------------------------------------------------------------------
%% @doc Builds subsequent fragment header
%% @spec buildFragHeader(FragHeader) -> binary().
%-------------------------------------------------------------------------------
-spec buildFragHeader(FragHeader) -> binary() when
      FragHeader :: map().
buildFragHeader(FragHeader) ->
    #frag_header{
        frag_type = FragType,
        datagram_size = DatagramSize,
        datagram_tag = DatagramTag,
        datagram_offset = DatagramOffset
    } = FragHeader,
    <<FragType:5, DatagramSize:11, DatagramTag:16, DatagramOffset:8>>.

%-------------------------------------------------------------------------------
%% @doc Builds first fragment header
%% @spec buildFirstFragHeader(FragHeader) -> binary().
%-------------------------------------------------------------------------------
-spec buildFirstFragHeader(FragHeader) -> binary() when
      FragHeader :: map().
buildFirstFragHeader(FragHeader) ->
    #frag_header{
        frag_type = FragType,
        datagram_size = DatagramSize,
        datagram_tag = DatagramTag
    } = FragHeader,
    <<FragType:5, DatagramSize:11, DatagramTag:16>>.

%-------------------------------------------------------------------------------
%% @spec buildFirstFragPckt(FragType, DatagramSize, DatagramTag, CompressedHeader, Payload) -> binary().
%-------------------------------------------------------------------------------
-spec buildFirstFragPckt(integer(), integer(), integer(), binary(), binary()) -> binary().
buildFirstFragPckt(FragType, DatagramSize, DatagramTag, CompressedHeader, Payload) ->
    <<FragType:5, DatagramSize:11, DatagramTag:16, CompressedHeader/binary, Payload/bitstring>>.

%-------------------------------------------------------------------------------
%% @doc Creates a fragmented packet
%% @spec buildDatagramPckt(DtgmHeader, Payload) -> binary().
%-------------------------------------------------------------------------------
-spec buildDatagramPckt(map(), binary()) -> binary().
buildDatagramPckt(DtgmHeader, Payload) ->
    TYPE = DtgmHeader#frag_header.frag_type,
    case TYPE of
        ?FRAG1_DHTYPE ->
            Header = buildFirstFragHeader(DtgmHeader),
            <<Header/binary, Payload/bitstring>>;
        ?FRAGN_DHTYPE ->
            Header = buildFragHeader(DtgmHeader),
            <<Header/binary, Payload/bitstring>>
    end.

%-------------------------------------------------------------------------------
%% @doc Checks if a packet needs to be fragmented or not and has a valid size 
%% returns a list of fragments if yes, the orginal packet if not
%% @spec triggerFragmentation(binary(), integer()) -> {boolean(), list()} | {atom(), atom()}.
%-------------------------------------------------------------------------------
-spec triggerFragmentation(binary(), integer(), boolean()) -> {boolean(), list()} | {size_err, error_frag_size}.
triggerFragmentation(CompPckt, DatagramTag, RouteExist) when byte_size(CompPckt) =< ?MAX_DTG_SIZE ->
    PcktLengt = byte_size(CompPckt),

    ValidLength = PcktLengt =< ?MAX_FRAME_SIZE,
    case ValidLength of
        false ->
            io:format("The received Ipv6 packet needs fragmentation to be transmitted~n"),
            Fragments = fragmentIpv6Packet(CompPckt, DatagramTag, RouteExist),
            {true, Fragments};
        true ->
            io:format("No fragmentation needed~n"),
            {false, CompPckt}
    end; 

triggerFragmentation(_CompPckt, _DatagramTag, _RouteExist) ->
    {size_err, error_frag_size}.

%-------------------------------------------------------------------------------
%% @doc Fragments a given Ipv6 packet
%% @spec fragmentIpv6Packet(binary(), integer()) -> list().
%% @returns a list of fragmented packets having this form:
%% [{FragHeader1, Fragment1}, ..., {FragHeaderN, FragmentN}]
%-------------------------------------------------------------------------------
-spec fragmentIpv6Packet(binary(), integer(), boolean()) -> list().
fragmentIpv6Packet(CompIpv6Pckt, DatagramTag, RouteExist) when is_binary(CompIpv6Pckt) ->
    Size = byte_size(CompIpv6Pckt),
    fragProcess(CompIpv6Pckt, DatagramTag, Size, 0, [], RouteExist).

%-------------------------------------------------------------------------------
%% @private
%% @doc helper function to process the received packet
%% @returns a list of fragmented packets
%% [{Header1, Fragment1}, ..., {HeaderN, FragmentN}]
%% @spec fragProcess(binary(), integer(), integer(), integer(), list()) -> list().
%% Input :
%%   Ipv6Pckt := binary
%%   Pckt size := integer
%%   DatagramTag := integer
%%   Offset := integer
%%   Accumulator : list
%-------------------------------------------------------------------------------
-spec fragProcess(binary(), integer(), integer(), integer(), list(), boolean()) -> list().
fragProcess(<<>>, _DatagramTag, _PacketLen, _Offset, Acc, _RouteExist) ->
    lists:reverse(Acc);
fragProcess(CompIpv6Pckt, DatagramTag, PacketLen, Offset, Acc, RouteExist) ->
    MaxSize = case RouteExist of
        true-> ?MAX_FRAG_SIZE_MESH; 
        false -> ?MAX_FRAG_SIZE_NoMESH
    end,
    PcktSize = byte_size(CompIpv6Pckt),
    FragmentSize = min(PcktSize, MaxSize),

    <<FragPayload:FragmentSize/binary, Rest/bitstring>> = CompIpv6Pckt,

    case Offset of
        0 ->
            Header =
                buildFirstFragHeader(#frag_header{
                    frag_type = ?FRAG1_DHTYPE,
                    datagram_size = PacketLen,
                    datagram_tag = DatagramTag,
                    datagram_offset = Offset
                });
        _ ->
            Header =
                buildFragHeader(#frag_header{
                    frag_type = ?FRAGN_DHTYPE,
                    datagram_size = PacketLen,
                    datagram_tag = DatagramTag,
                    datagram_offset = Offset
                })
    end,

    fragProcess(Rest, DatagramTag, PacketLen, Offset + 1, [{Header, FragPayload} | Acc], RouteExist).

%-------------------------------------------------------------------------------
%% @doc Check if tag exist in the map, if so generate a new one and update the tag map
%% @spec checkTagUnicity(map(), integer()) -> {integer(), map()}.
%-------------------------------------------------------------------------------
-spec checkTagUnicity(map(), integer()) -> {integer(), map()}.
checkTagUnicity(Map, Tag) ->
    Exist = maps:is_key(Tag, Map),
    case Exist of
        true ->
            NewTag = rand:uniform(?MAX_TAG_VALUE),
            checkTagUnicity(Map, NewTag);
        false ->
            NewMap = maps:put(Tag, valid, Map),
            {Tag, NewMap}
    end.

%-------------------------------------------------------------------------------
%
%                                Packet Decoding
%
%-------------------------------------------------------------------------------

%-------------------------------------------------------------------------------
%% @doc decode an Ipv6 packet header commpressed according to the IPHC compression scheme
%% @spec decodeIpv6Pckt(boolean(), binary(), binary(), binary()) -> binary() | {atom(), atom()}.
%% @returns the decoded Ipv6 packet
%-------------------------------------------------------------------------------
-spec decodeIpv6Pckt(boolean(), binary(), binary(), binary()) -> binary() | {atom(), atom()}.
decodeIpv6Pckt(RouteExist, OriginatorMacAddr, CurrNodeMacAdd, CompressedPacket) ->
    <<Dispatch:3, TF:2, NH:1, HLIM:2, CID:1, SAC:1, SAM:2, M:1, DAC:1, DAM:2, Rest/bitstring>> =
        CompressedPacket,
    case Dispatch of
        ?IPHC_DHTYPE -> 
            {SrcContextId, DstContextId, Rest0} = decodeCid(CID, Rest),
            {{DSCP, ECN}, FlowLabel, Rest1} = decodeTf(TF, Rest0),
            {NextHeader, Rest2} = decodeNextHeader(NH, Rest1),
            {HopLimit, Rest3} = decodeHlim(HLIM, Rest2),
            {SourceAddress, Rest4} = decodeSam(SAC, SAM, Rest3, OriginatorMacAddr, SrcContextId, RouteExist),
            {DestAddress, Payload} = decodeDam(M, DAC, DAM, Rest4, CurrNodeMacAdd, DstContextId, RouteExist),
            PayloadLength = byte_size(Payload),
            TrafficClass = DSCP bsl 2 + ECN,

            <<Header:5, Inline/bitstring>> = Payload,

            io:format("-----------------------------------------------------~n"),
            io:format("Decoded packet~n"), 
            io:format("-----------------------------------------------------~n"),
            DecodedPckt = 
            case Header of
                ?UDP_DHTYPE-> 
                    {SrcPort, DstPort, Checksum, UdpPayload} = decodeUdpPckt(Inline),
                    Length = byte_size(UdpPayload),
                    io:format("IPv6~n"), 
                    
                    io:format("Traffic class: ~p~nFlow label: ~p~nNext header: ~p~nHop limit: ~p~nSource address: ~p~nDestination address: ~p~n", 
                                [TrafficClass, FlowLabel, NextHeader, HopLimit, convert(SourceAddress), convert(DestAddress)]),  
                    io:format("-----------------------------------------------------~n"),
                    io:format("UDP~n"), 
                    io:format("Source port: ~p~nDestination Port: ~p~nLength: ~p~nChecksum: ~p~n",[ SrcPort, DstPort, Length, Checksum]),
                    io:format("-----------------------------------------------------~n"),
                    io:format("Data: ~p~n",[UdpPayload]),                           
                    io:format("-----------------------------------------------------~n"),
                    
                    <<6:4,TrafficClass,FlowLabel:20,PayloadLength:16,NextHeader:8,HopLimit:8,
                    SourceAddress/binary,DestAddress/binary, SrcPort:16, DstPort:16, Length:16, Checksum:16, Payload/bitstring>>;
                    
                _->
                    io:format("IPv6~n"), 
                    io:format("Traffic class: ~p~nFlow label: ~p~nPayload length: ~p~nNext header: ~p~nHop limit: ~p~nSource address: ~p~nDestination address: ~p~nData: ~p~n", [TrafficClass, FlowLabel, PayloadLength, 
                                NextHeader, HopLimit, convert(SourceAddress), convert(DestAddress), Payload]),                        
                    io:format("-----------------------------------------------------~n"),
                    <<6:4,TrafficClass,FlowLabel:20,PayloadLength:16,NextHeader:8,HopLimit:8,
                    SourceAddress/binary,DestAddress/binary, Payload/bitstring>>
            end,

            DecodedPckt;

        _-> error_decoding
    end.

%-------------------------------------------------------------------------------
%% @private
%% @doc Decode logic for the CID field
%% @spec decodeCid(integer(), binary()) -> {integer(), integer(), binary()}.
%% @returns the decoded ContextID
%-------------------------------------------------------------------------------
-spec decodeCid(integer(), binary()) -> {integer(), integer(), binary()}.
decodeCid(CID, CarriedInline) when CID == 1 ->
    <<SrcContextId:4, DstContextId:4, Rest/bitstring>> = CarriedInline,
    {SrcContextId, DstContextId, Rest};
decodeCid(CID, CarriedInline) when CID == 0 ->
    DefaultPrefix = 0,
    {DefaultPrefix, DefaultPrefix, CarriedInline}.

%-------------------------------------------------------------------------------
%% @private
%% @doc decode logic for the TF field
%% @spec decodeTf(integer(), binary()) -> {{integer(), integer()}, integer(), binary()}.
%% @returns the decoded TrafficClass and FlowLabel value
%-------------------------------------------------------------------------------
-spec decodeTf(integer(), binary()) -> {{integer(), integer()}, integer(), binary()}.
decodeTf(TF, CarriedInline) ->
    case TF of
        2#11 ->
            ECN = 0, DSCP = 0, FL = 0,
            {{DSCP, ECN}, FL, CarriedInline};
        2#01 ->
            <<ECN:2, _rsv:2, FL:20, Rest/bitstring>> = CarriedInline,
            DSCP = 0,
            {{DSCP, ECN}, FL, Rest};
        2#10 ->
            <<ECN:2, DSCP:6, Rest/bitstring>> = CarriedInline,
            FL = 0,
            {{DSCP, ECN}, FL, Rest};
        2#00 ->
            <<ECN:2, DSCP:6, _rsv:4, FL:20, Rest/bitstring>> = CarriedInline,
            {{DSCP, ECN}, FL, Rest}
    end.

%-------------------------------------------------------------------------------
%% @private
%% @doc Decode logic for the NH field
%% @spec decodeNextHeader(integer(), binary()) -> {integer(), binary()}.
%% @returns the decoded NextHeader value
%-------------------------------------------------------------------------------
-spec decodeNextHeader(integer(), binary()) -> {integer(), binary()}.
decodeNextHeader(NH, CarriedInline) when NH == 0 ->
    <<NextHeader:8, Rest/bitstring>> = CarriedInline,
    {NextHeader, Rest};
decodeNextHeader(NH, CarriedInline) when NH == 1 ->
    {?UDP_PN, CarriedInline}.

%-------------------------------------------------------------------------------
%% @private
%% @doc Decode logic for the HLim field
%% @spec decodeHlim(integer(), binary()) -> {integer(), binary()}.
%% @returns the decoded Hop Limit value
%-------------------------------------------------------------------------------
-spec decodeHlim(integer(), binary()) -> {integer(), binary()}.
decodeHlim(HLim, CarriedInline) ->
    <<HopLimit:8, Rest/bitstring>> = CarriedInline,
    case HLim of
        2#11 ->
            {255, CarriedInline};
        2#10 ->
            {64, CarriedInline};
        2#01 ->
            {1, CarriedInline};
        2#00 ->
            {HopLimit, Rest}
    end.

%-------------------------------------------------------------------------------
%% @private
%% @doc decode logic for the SAC field
%% @spec decodeSam(integer(), integer(), binary(), binary(), integer(), boolean()) -> {binary(), binary()}.
%% @returns the decoded Source Address Mode value
%-------------------------------------------------------------------------------
-spec decodeSam(integer(), integer(), binary(), binary(), integer(), boolean()) -> {binary(), binary()}.
decodeSam(SAC, SAM, CarriedInline, MacIID, _Context, RouteExist) when SAC == 0 ->
    case {SAM, RouteExist} of
        {2#11, true} ->
            SrcAdd = <<?LINK_LOCAL_PREFIX:16, 0:48, MacIID/binary>>, 
            {SrcAdd, CarriedInline};
        {2#11, false} ->
            <<_:48, IID:16>> = MacIID,
            SrcAdd = <<?LINK_LOCAL_PREFIX:16, 0:48, 0:16, 16#00FF:16, 16#FE00:16, IID:16>>,
            {SrcAdd, CarriedInline};
        {2#10, _} ->
            <<Last16Bits:16, Rest/bitstring>> = CarriedInline,
            SrcAdd = <<?LINK_LOCAL_PREFIX:16, 0:48, 16#000000FFFE00:48, Last16Bits:16>>,
            {SrcAdd, Rest};
        {2#01, _} ->
            <<Last64Bits:64, Rest/bitstring>> = CarriedInline,
            SrcAdd = <<?LINK_LOCAL_PREFIX:16, 0:48, Last64Bits:64>>,
            {SrcAdd, Rest};
        {2#00, _} ->
            <<SrcAdd:128, Rest/bitstring>> = CarriedInline,
            {SrcAdd, Rest}
    end;
decodeSam(SAC, _SAM, CarriedInline, _MacIID, 0, _RouteExist) when SAC == 1 ->
    <<SrcAdd:128, Rest/bitstring>> = CarriedInline,
    {<<SrcAdd:128>>, Rest};
decodeSam(SAC, SAM, CarriedInline, MacIID, Context, _RouteExist) when SAC == 1 ->
    SrcAddrPrefix = maps:get(Context, ?Context_id_table), 
    case SAM of
        2#11 ->
            <<_:48, IID:16>> = MacIID, 
            SrcAdd = <<SrcAddrPrefix/binary, 0:16, 16#00FF:16, 16#FE00:16, IID:16>>,
            {SrcAdd, CarriedInline}; 
        2#10 ->
            <<Last16Bits:16, Rest/bitstring>> = CarriedInline,
            SrcAdd = <<SrcAddrPrefix/binary, 16#000000FFFE00:48, Last16Bits:16>>,
            {SrcAdd, Rest};
        2#01 ->
            <<Last64Bits:64, Rest/bitstring>> = CarriedInline,
            SrcAdd = <<SrcAddrPrefix/binary, Last64Bits:64>>,
            {SrcAdd, Rest};
        2#00 ->
            SrcAdd = <<0:128>>,
            {SrcAdd, CarriedInline}
    end.

%-------------------------------------------------------------------------------
%% @private
%% @doc Decode logic for the DAC field
%% @spec decodeDam(integer(), integer(), integer(), binary(), binary(), integer(), boolean()) -> {binary(), binary()}.
%% @returns the decoded Destination Address Mode value
%-------------------------------------------------------------------------------
-spec decodeDam(integer(), integer(), integer(), binary(), binary(), integer(), boolean()) -> {binary(), binary()}.
decodeDam(0, 0, DAM, CarriedInline, MacIID, _Context, RouteExist) ->
    case {DAM, RouteExist} of
        {2#11, true} ->
            DstAdd = <<?LINK_LOCAL_PREFIX:16, 0:48, MacIID/binary>>,
            {DstAdd, CarriedInline};
        {2#11, false} ->
            DstAdd = <<?LINK_LOCAL_PREFIX:16, 0:48, 0:24, 16#FFFE:16, 0:24>>,
            {DstAdd, CarriedInline};
        {2#10, _} ->
            <<Last16Bits:16, Rest/bitstring>> = CarriedInline,
            DstAdd = <<?LINK_LOCAL_PREFIX:16, 0:48, 16#000000FFFE00:48, Last16Bits:16>>,
            {DstAdd, Rest};
        {2#01, _} ->
            <<Last64Bits:64, Rest/bitstring>> = CarriedInline,
            DstAdd = <<?LINK_LOCAL_PREFIX:16, 0:48, Last64Bits:64>>,
            {DstAdd, Rest};
        {2#00, _} ->
            <<DstAdd:128, Rest/bitstring>> = CarriedInline,
            {DstAdd, Rest}
    end;
decodeDam(0, 1, _DAM, CarriedInline, _MacIID, 0, _RouteExist) ->
    <<DstAdd:128, Rest/bitstring>> = CarriedInline,
    {<<DstAdd:128>>, Rest};
decodeDam(0, 1, DAM, CarriedInline, _MacIID, Context, _RouteExist) ->
    DstAddrPrefix = maps:get(Context, ?Context_id_table), 
    case DAM of
        2#11 ->
            {<<DstAddrPrefix/binary, 0:24, 16#FFFE:16, 0:24>>, CarriedInline};
        2#10 ->
            <<Last16Bits:16, Rest/bitstring>> = CarriedInline,
            DstAdd = <<DstAddrPrefix/binary, 16#000000FFFE00:48, Last16Bits:16>>,
            {DstAdd, Rest};
        2#01 ->
            <<Last64Bits:64, Rest/bitstring>> = CarriedInline,
            DstAdd = <<DstAddrPrefix/binary, Last64Bits:64>>,
            {DstAdd, Rest};
        2#00 -> {error_reserved, CarriedInline}
    end;
decodeDam(1, 0, DAM, CarriedInline, _MacIID, _Context, _RouteExist) ->
    case DAM of
        2#11 ->
            <<Last8Bits:8, Rest/bitstring>> = CarriedInline,
            DstAdd = <<?MULTICAST_PREFIX:16, 0:104, Last8Bits>>,
            {DstAdd, Rest}; 
        2#10 ->
            <<Last32Bits:32, Rest/bitstring>> = CarriedInline,
            DstAdd = <<?MULTICAST_PREFIX:16, 0:80, Last32Bits:32>>,
            {DstAdd, Rest};
        2#01 ->
            <<Last48Bits:48, Rest/bitstring>> = CarriedInline,
            DstAdd = <<?MULTICAST_PREFIX:16, 0:64, Last48Bits:48>>,
            {DstAdd, Rest};
        2#00 ->
            <<DstAdd:128, Rest/bitstring>> = CarriedInline,
            {DstAdd, Rest}
    end;
decodeDam(1, 1, DAM, CarriedInline, _MacIID, _Context, _RouteExist) ->
    case DAM of
        2#00 ->
            <<Last48Bits:48, Rest/bitstring>> = CarriedInline,
            DstAdd = <<16#FF:16, 0:64, Last48Bits:48>>,
            {DstAdd, Rest}
    end.

-spec decodeUdpPckt(binary()) -> {integer(), integer(), integer(), binary()}.
decodeUdpPckt(Rest) ->
    <<C:1, P:2, Inline/bitstring>> = Rest, 
    {SrcPort, DstPort, Rest1} = decodePort(P, Inline), 
    {Checksum, Payload} = decodeChecksum(C, Rest1), 
    {SrcPort, DstPort, Checksum, Payload}.

-spec decodePort(integer(), binary()) -> {integer(), integer(), binary()}.
decodePort(P, Inline) ->
    case P of
        2#11 ->
            <<Last4S_Bits:4, Last4D_Bits:4, Rest/bitstring>> = Inline,
            SrcPort = <<?Oxf0b:12, Last4S_Bits:4>>,
            DstPort = <<?Oxf0b:12, Last4D_Bits:4>>,
            <<S:16>> = SrcPort,
            <<D:16>> = DstPort, 
            {S, D, Rest}; 
        2#10 -> 
            <<Last8S_Bits:8, DstPort:16, Rest/bitstring>> = Inline, 
            SrcPort = <<?Oxf0:8, Last8S_Bits:8>>, 
            <<S:16>> = SrcPort,
            {S, DstPort, Rest}; 
        2#01 -> 
            <<SrcPort:16, Last8D_Bits:8, Rest/bitstring>> = Inline, 
            DstPort = <<?Oxf0:8, Last8D_Bits:8>>, 
            <<D:16>> = DstPort,
            {SrcPort, D, Rest}; 
        2#00 -> 
            <<SrcPort:16, DstPort:16, Rest/bitstring>> = Inline, 
            {SrcPort, DstPort, Rest}
    end.

-spec decodeChecksum(integer(), binary()) -> {integer(), binary()}.
decodeChecksum(C, Inline) ->
    case C of 
        1 -> {0, Inline}; 
        0 ->  
            <<Checksum:16, Rest/bitstring>> = Inline, 
            {Checksum, Rest}
    end.

%-------------------------------------------------------------------------------
%                          Packet Decompression Helper
%-------------------------------------------------------------------------------

-spec convertAddrToBin(term()) -> binary().
convertAddrToBin(Address) ->
    DestAdd = case is_integer(Address) of
        true -> 
            encodeInteger(Address);
        false ->
            Address
    end,
    DestAdd.

-spec tupleToBin(tuple()) -> binary().
tupleToBin(Tuple) ->
    Elements = tuple_to_list(Tuple),
    Binaries = [elementToBinary(Elem) || Elem <- Elements],
    list_to_binary(Binaries).

-spec elementToBinary(term()) -> binary().
elementToBinary(Elem) when is_integer(Elem) ->
    encodeInteger(Elem);
elementToBinary(Elem) when is_binary(Elem) ->
    Elem;
elementToBinary(Elem) when is_tuple(Elem) ->
    tupleToBin(Elem);
elementToBinary(Elem) when is_list(Elem) ->
    list_to_binary(Elem).

%-------------------------------------------------------------------------------
%
%                                Reassembly
%
%-------------------------------------------------------------------------------

%-------------------------------------------------------------------------------
%% @spec datagramInfo(binary()) -> map().
%% @doc helper function to retrieve datagram info
%% @returns a tuple containing useful fragment info
%-------------------------------------------------------------------------------
-spec datagramInfo(binary()) -> map().
datagramInfo(Fragment) ->
    <<FragType:5, Rest/bitstring>> = Fragment,
    case FragType of
        ?FRAG1_DHTYPE ->
            <<DatagramSize:11, DatagramTag:16, Payload/bitstring>> = Rest,
            FragInfo =
                #datagramInfo{
                    fragtype = FragType,
                    datagramSize = DatagramSize,
                    datagramTag = DatagramTag,
                    datagramOffset = 0,
                    payload = Payload
                },
            FragInfo;
        ?FRAGN_DHTYPE ->
            <<DatagramSize:11, DatagramTag:16, DatagramOffset:8, Payload/bitstring>> = Rest,
            FragInfo =
                #datagramInfo{
                    fragtype = FragType,
                    datagramSize = DatagramSize,
                    datagramTag = DatagramTag,
                    datagramOffset = DatagramOffset,
                    payload = Payload
                },
            FragInfo
    end.



%-------------------------------------------------------------------------------
%% @doc Stores fragment in ETS and check if the datagram is complete
%% @spec storeFragment(atom(), term(), integer(), binary(), integer(), integer(), integer(), term()) -> {term(), map()}.
%-------------------------------------------------------------------------------
-spec storeFragment(map(), term(), integer(), binary(), integer(), integer(), integer(), term()) -> {term(), map()}.
storeFragment(DatagramMap, Key, Offset, Payload, CurrTime, Size, Tag, _From) ->
    {Result, Map} = case ets:lookup(DatagramMap, Key) of
        [] ->
            handleNewDatagram(DatagramMap, Key, Offset, Payload, CurrTime, Size, Tag);
        [{Key, OldDatagram}] ->
            handleExistingDatagram(DatagramMap, Key, Offset, Payload, CurrTime, Size, OldDatagram)
    end,

    io:format("------------------------------------------------------~n"),
    io:format("DatagramMap after update:~n"),
    printDatagramMap(DatagramMap),
    io:format("------------------------------------------------------~n"),
    {Result, Map}.

-spec handleNewDatagram(map(), term(), integer(), binary(), integer(), integer(), integer()) -> {term(), map()}.
handleNewDatagram(DatagramMap, Key, Offset, Payload, CurrTime, Size, Tag) ->
    if byte_size(Payload) == Size ->
        ReassembledPacket = reassemble(#datagram{
            tag = Tag,
            size = Size,
            cmpt = byte_size(Payload),
            fragments = #{Offset => Payload},
            timer = CurrTime
        }),
        ets:insert(DatagramMap, {Key, ReassembledPacket}),
        {complete_first_frag, ReassembledPacket};
    true ->
        NewDatagram = #datagram{
            tag = Tag,
            size = Size,
            cmpt = byte_size(Payload),
            fragments = #{Offset => Payload},
            timer = CurrTime
        },
        ets:insert(DatagramMap, {Key, NewDatagram}),
        {incomplete_first, Key}
    end.

-spec handleExistingDatagram(map(), term(), integer(), binary(), integer(), integer(), map()) -> {term(), map()}.
handleExistingDatagram(DatagramMap, Key, Offset, Payload, CurrTime, Size, OldDatagram) ->
    Fragments = OldDatagram#datagram.fragments,
    case maps:is_key(Offset, Fragments) of
        true ->
            {duplicate, OldDatagram};
        false ->
            NewFragments = maps:put(Offset, Payload, Fragments),
            NewCmpt = OldDatagram#datagram.cmpt + byte_size(Payload),
            UpdatedDatagram = OldDatagram#datagram{
                cmpt = NewCmpt,
                fragments = NewFragments,
                timer = CurrTime
            },
            ets:insert(DatagramMap, {Key, UpdatedDatagram}),
            if NewCmpt == Size ->
                {complete, UpdatedDatagram};
            true ->
                {incomplete, UpdatedDatagram}
            end
    end.

-spec printDatagramMap(map()) -> ok.
printDatagramMap(DatagramMap) ->
    List = ets:tab2list(DatagramMap),
    lists:foreach(fun({Key, Value}) -> printEntry(Key, Value) end, List).

-spec printEntry(term(), tuple()) -> ok.
printEntry(Key, {datagram, Tag, Size, Cmpt, Timer, Fragments}) ->
    io:format("~p -> {datagram, ~p, ~p, ~p,~n    #{~n", [Key, Tag, Size, Cmpt]),
    printFragments(Fragments),
    io:format("    }, ~p}~n", [Timer]).

-spec printFragments(map()) -> ok.
printFragments(Fragments) ->
    maps:fold(fun(Offset, Payload, Acc) ->
                      io:format("        ~p => ~p,~n", [Offset, Payload]),
                      Acc
              end, ok, Fragments).

%-------------------------------------------------------------------------------
%% @spec reassemble(map()) -> binary().
%% @doc Reassemble the datagram from stored fragments
%-------------------------------------------------------------------------------
-spec reassemble(map()) -> binary().
reassemble(Datagram) ->
    FragmentsMap = Datagram#datagram.fragments,
    SortedFragments =
        lists:sort([{Offset, Fragment} || {Offset, Fragment} <- maps:to_list(FragmentsMap)]),
    lists:foldl(
        fun({_Offset, Payload}, Acc) ->
            <<Acc/binary, Payload/binary>>
        end,
        <<>>,
        SortedFragments
    ).

%-------------------------------------------------------------------------------
%
%                                    ROUTING
%
%-------------------------------------------------------------------------------

%-------------------------------------------------------------------------------
%                      Mesh Addressing Type and Header
%
%    0                   1                   2                   3
%    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%   |1 0|V|F|HopsLft|  originator address,final destination address
%   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%

%-------------------------------------------------------------------------------
%% @doc Creates mesh header binary
%% @spec buildMeshHeader(map()) -> binary().
%-------------------------------------------------------------------------------
-spec buildMeshHeader(map()) -> binary().
buildMeshHeader(MeshHeader) ->
    #mesh_header{
        v_bit = VBit,
        f_bit = FBit,
        hops_left = HopsLeft,
        originator_address = OriginatorAddress,
        final_destination_address = FinalDestinationAddress
    } = MeshHeader,
    <<?MESH_DHTYPE:2, VBit:1, FBit:1, HopsLeft:4, 
                 OriginatorAddress/binary, FinalDestinationAddress/binary>>.

%-------------------------------------------------------------------------------
%% @spec createNewMeshDatagram(binary(), binary(), binary()) -> binary().
%% @doc Creates new mesh header and returns new datagram
%-------------------------------------------------------------------------------
-spec createNewMeshDatagram(binary(), binary(), binary()) -> binary().
createNewMeshDatagram(Datagram, SenderMacAdd, DstMacAdd) ->
    VBit =
        if
            byte_size(SenderMacAdd) =:= 8 -> 0;
            true -> 1
        end,
    FBit =
        if
            byte_size(DstMacAdd) =:= 8 -> 0;
            true -> 1
        end,

    MeshHeader =
        #mesh_header{
            v_bit = VBit,
            f_bit = FBit,
            hops_left = ?Max_Hops,
            originator_address = SenderMacAdd,
            final_destination_address = DstMacAdd
        },
    BinMeshHeader = buildMeshHeader(MeshHeader),
    <<BinMeshHeader/binary, Datagram/bitstring>>.

%-------------------------------------------------------------------------------
%% @doc Creates new mesh header
%% @spec createNewMeshHeader(binary(), binary(), boolean()) -> binary().
%-------------------------------------------------------------------------------
createNewMeshHeader(SenderMacAdd, DstMacAdd, Extended_hopsleft) ->
    VBit =
        if
            byte_size(SenderMacAdd) =:= 8 -> 0;
            true -> 1
        end,
    FBit =
        if
            byte_size(DstMacAdd) =:= 8 -> 0;
            true -> 1
        end,
    
    case Extended_hopsleft of 
        true -> 
            <<?MESH_DHTYPE:2, VBit:1, FBit:1, ?DeepHopsLeft:4, 
            SenderMacAdd/binary, DstMacAdd/binary, ?Max_DeepHopsLeft:8>>;
        false ->
            <<?MESH_DHTYPE:2, VBit:1, FBit:1, ?Max_Hops:4, 
            SenderMacAdd/binary, DstMacAdd/binary>>
    end.

%-------------------------------------------------------------------------------
%% @doc Returns routing info in mesh header
%% @spec getMeshInfo(binary()) -> map().
%-------------------------------------------------------------------------------
-spec getMeshInfo(binary()) -> map().
getMeshInfo(Datagram) ->
    <<_:2, _V:1, _F:1, Hops_left:4, _/bitstring>> = Datagram,
    
    case Hops_left of 
        ?DeepHopsLeft ->
            <<?MESH_DHTYPE:2, VBit:1, FBit:1, HopsLeft:4, OriginatorAddress:64, FinalDestinationAddress:64, DeepHopsLeft:8, Data/bitstring>> =
            Datagram;
        _ -> 
            <<?MESH_DHTYPE:2, VBit:1, FBit:1, HopsLeft:4, OriginatorAddress:64, FinalDestinationAddress:64, Data/bitstring>> =
            Datagram, 
            DeepHopsLeft = undefined
    end,
    MeshInfo =
        #meshInfo{
            v_bit = VBit,
            f_bit = FBit,
            hops_left = HopsLeft,
            originator_address = <<OriginatorAddress:64>>,
            final_destination_address = <<FinalDestinationAddress:64>>,
            deep_hops_left =  DeepHopsLeft,
            payload = Data
        },
    MeshInfo.

%-------------------------------------------------------------------------------
%% @doc Checks if datagram in mesh type, if so return true and mesh header info
%% @spec containsMeshHeader(binary()) -> {boolean(), map()} | boolean().
%-------------------------------------------------------------------------------
-spec containsMeshHeader(binary()) -> {boolean(), map()} | boolean().
containsMeshHeader(Datagram) ->
    case Datagram of
        <<Dispatch:2, _/bitstring>> when Dispatch == ?MESH_DHTYPE ->
            {true, getMeshInfo(Datagram)};
        _ ->
            false
    end.

%-------------------------------------------------------------------------------
%% @doc Removes mesh header if the datagram was meshed (used in put and reasssemble)
%% @spec removeMeshHeader(binary(), integer()) -> binary().
%-------------------------------------------------------------------------------
-spec removeMeshHeader(binary(), integer()) -> binary().
removeMeshHeader(Datagram, HopsLeft) ->
    case Datagram of
        <<?MESH_DHTYPE:2, _/bitstring>> ->
            case HopsLeft of 
                    ?DeepHopsLeft -> 
                        <<?MESH_DHTYPE:2, _Header:142, Rest/bitstring>> = Datagram,
                        Rest;
                    _->
                        <<?MESH_DHTYPE:2, _Header:134, Rest/bitstring>> = Datagram,
                        Rest
            end;
        _ ->
            Datagram
    end.

%-------------------------------------------------------------------------------
%% @doc Checks the next hop in the routing table and create new datagram with mesh
%% header if meshing is needed
%% @spec getNextHop(binary(), binary(), binary(), binary(), integer(), 
% boolean()) -> {boolean(), binary(), map()} | {boolean(), binary(), map(), map()}.
%% returns a tuple {nexthop:boolean, binary, datagram, macHeader}
%-------------------------------------------------------------------------------
-spec getNextHop(CurrNodeMacAdd, SenderMacAdd, DestMacAddress, DestAddress, SeqNum, HopsleftExtended) -> 
      {boolean(), binary(), mac_header()} 
      when 
      CurrNodeMacAdd :: binary(),
      SenderMacAdd :: binary(),
      DestMacAddress :: binary(),
      DestAddress :: binary(),
      SeqNum :: integer(),
      HopsleftExtended :: boolean().
getNextHop(CurrNodeMacAdd, SenderMacAdd, DestMacAddress, DestAddress, SeqNum, Hopsleft_extended) ->
    case <<DestAddress:128>> of 
        <<16#FF:8,_/binary>> ->
            MulticastAddr = generateMulticastAddr(<<DestAddress:128>>), 
            Multicast_EU64 = generateEUI64MacAddr(MulticastAddr),
            MHdr = #mac_header{src_addr = CurrNodeMacAdd, dest_addr = Multicast_EU64},
            BroadcastHeader = createBroadcastHeader(SeqNum),
            MeshHdrBin = createNewMeshHeader(SenderMacAdd, DestMacAddress, Hopsleft_extended),
            Header = <<MeshHdrBin/bitstring, BroadcastHeader/bitstring>>,
            {false, Header, MHdr};
        _->
            case routing_table:getRoute(DestMacAddress) of
                NextHopMacAddr when NextHopMacAddr =/= DestMacAddress ->
                    io:format("Next hop found: ~p~n", [NextHopMacAddr]),
                    MacHdr = #mac_header{src_addr = CurrNodeMacAdd, dest_addr = NextHopMacAddr},
                    MeshHdrBin = createNewMeshHeader(SenderMacAdd, DestMacAddress, Hopsleft_extended),
                    {true, MeshHdrBin, MacHdr};
                NextHopMacAddr when NextHopMacAddr == DestMacAddress ->
                    io:format("Direct link found ~n"),
                    MHdr = #mac_header{src_addr = CurrNodeMacAdd, dest_addr = DestMacAddress},
                    {false, <<>>, MHdr};
                _ ->
                    {false, <<>>, undefined, undefined}
            end
        end.

-spec getNextHop(binary(), binary()) -> {boolean(), binary(), map()} | {boolean(), binary(), map(), map()}.
getNextHop(CurrNodeMacAdd, DestMacAddress) ->
    case routing_table:getRoute(DestMacAddress) of
        NextHopMacAddr when NextHopMacAddr =/= DestMacAddress ->
        MacHdr = #mac_header{src_addr = CurrNodeMacAdd, dest_addr = NextHopMacAddr},
        MeshHdrBin = createNewMeshHeader(CurrNodeMacAdd, DestMacAddress, ?DeepHopsLeft),
        {true, MeshHdrBin, MacHdr};
    NextHopMacAddr when NextHopMacAddr == DestMacAddress ->
        MHdr = #mac_header{src_addr = CurrNodeMacAdd, dest_addr = DestMacAddress},
        {false, <<>>, MHdr};
    _ ->
        {false, <<>>, undefined, undefined}
    end.

-spec generateEUI64MacAddr(binary()) -> binary().
generateEUI64MacAddr(MacAddress) when byte_size(MacAddress) == ?SHORT_ADDR_LEN ->
    PanID = <<16#FFFF:16>>,
    Extended48Bit = <<PanID/binary, 0:16, MacAddress/binary>>, 
    <<A:8, Rest:40>> = Extended48Bit, 
    ULBSetup = A band 16#FD,
    <<First:16, Last:24>> = <<Rest:40>>,
    EUI64 = <<ULBSetup:8, First:16, 16#FF:8, 16#FE:8, Last:24>>, 
    EUI64;
generateEUI64MacAddr(MacAddress) when byte_size(MacAddress) == ?EXTENDED_ADDR_LEN ->
    <<A:8, Rest:56>> = MacAddress,  
    NewA = A bxor 2,   
    <<NewA:8, Rest:56>>.

-spec getEUI64From48bitMac(binary()) -> binary().
getEUI64From48bitMac(MacAddress) ->
    <<First:24, Last:24>> = MacAddress, 
    <<A:8, Rest:16>> = <<First:24>>,
    NewA = A bxor 2,
    EUI64 = <<NewA:8, Rest:16, 16#fffe:16, Last:24>>,
    EUI64.


-spec generateLLAddr(binary()) -> binary().
generateLLAddr(MacAddress) ->
    EUI64 = generateEUI64MacAddr(MacAddress),
    LLAdd = <<16#FE80:16, 0:48, EUI64/binary>>,
    LLAdd.

-spec getEUI64MacAddr(binary()) -> binary().
getEUI64MacAddr(Address) ->
    <<_:64, MacAddr:64/bitstring>> = <<Address:128>>,
    MacAddr.

-spec get16bitMacAddr(binary()) -> binary().
get16bitMacAddr(Address) ->
    <<_:112, MacAddr:16/bitstring>> = <<Address:128>>,
    MacAddr.


%-------------------------------------------------------------------------------
% Generates a EUI64 address from the 16bit short mac address
%-------------------------------------------------------------------------------
getEUI64FromShortMac(MacAddress)->
    PanID = <<16#FFFF:16>>,%ieee802154:get_pib_attribute(mac_pan_id),
    Extended48Bit = <<PanID/binary, 0:16, MacAddress/binary>>, 
    <<A:8, Rest:40>> = Extended48Bit, 
    ULBSetup = A band 16#FD, % replace 7th bit of first byte (U/L) by 0
    <<First:16, Last:24>> = <<Rest:40>>,
    EUI64 = <<ULBSetup:8, First:16, 16#FF:8, 16#FE:8, Last:24>>, 
    EUI64.

%-------------------------------------------------------------------------------
% Generates a EUI64 address from the 64bit extended mac address
%-------------------------------------------------------------------------------
getEUI64FromExtendedMac(MacAddress)->
    <<A:8, Rest:56>> = MacAddress,  
    NewA = A bxor 2,   
    <<NewA:8, Rest:56>>.

-spec generateMulticastAddr(binary()) -> binary().
generateMulticastAddr(DestAddress) ->
    <<_:112, DST_15:8, DST_16:8>> = DestAddress,
    <<_:3, Last5Bits:5>> = <<DST_15:8>>,
    MulticastAddr = <<2#100:3, Last5Bits:5, DST_16:8>>,
    MulticastAddr.

-spec createBroadcastHeader(integer()) -> binary().
createBroadcastHeader(SeqNum) ->
   BC0_Header = <<?BC0_DHTYPE, SeqNum:8>>,
   BC0_Header.


%-------------------------------------------------------------------------------
%
%                               Utils functions
%
%-------------------------------------------------------------------------------

-spec convert(binary()) -> list().
convert(Binary) ->
    lists:flatten(
        lists:join(":",
            [io_lib:format("~2.16.0B", [B]) || <<B:8>> <= Binary]
        )
    ).

-spec generateChunks() -> binary().
generateChunks() ->
    NumChunks = 5,
    ChunkSize = 58,
    Chunks =
        lists:map(fun(N) -> generateChunk(N, ChunkSize) end, lists:seq(NumChunks, 1, -1)),
    Result = lists:foldl(fun(A, B) -> <<A/binary, B/binary>> end, <<>>, Chunks),
    Result.

-spec generateChunks(integer()) -> binary().
generateChunks(Size) ->
    ChunkSize = 48,
    Chunks =
        lists:map(fun(N) -> generateChunk(N, ChunkSize) end, lists:seq(Size, 1, -1)),
    Result = lists:foldl(fun(A, B) -> <<A/binary, B/binary>> end, <<>>, Chunks),
    Result.

-spec generateChunk(integer(), integer()) -> binary().
generateChunk(N, Size) ->
    Prefix = list_to_binary(io_lib:format("chunk_~2..0B", [N])),
    PrefixSize = byte_size(Prefix),
    PaddingSize = Size - PrefixSize,
    Padding = list_to_binary(lists:duplicate(PaddingSize, $a)),
    <<Prefix/binary, Padding/binary>>.
