-module(lowpan_api).

-behaviour(gen_statem).

-include("lowpan.hrl").

-export([init/1, start_link/1, start/1, stop_link/0, stop/0]).
-export([callback_mode/0]).
-export([sendPacket/1, sendPacket/2, sendUncDatagram/3, tx/3, extendedHopsleftTx/1]).
-export([frameReception/0]).
-export([inputCallback/4]).
-export([idle/3]).
-export([tx_frame/3]).
-export([tx_datagram/3]).
-export([tx_packet/3]).
-export([rx_frame/3]).
-export([collect/3]).
-export([reassemble/3]).
-export([forward/3]).
-export([tx_packet_metrics/3]).

% API Functions
%% @doc Initializes the lowpan API module.
%% @spec init(map()) -> {ok, atom(), map()}.
init(Params) ->
    io:format("-------------------------------------------------------------------------~n"),
    io:format("Initialization~n"),
    MacAdd = maps:get(node_mac_addr, Params),
    CurrNodeMacAdd = lowpan_core:generateEUI64MacAddr(MacAdd),
    io:format("Current node address: ~p~n",[CurrNodeMacAdd]),
    setup_node_info_ets(),

    RoutingTable  = maps:get(routing_table, Params),

    case routing_table:start_link(RoutingTable) of
        {ok, _Pid} ->
            io:format("~p: Routing table server successfully launched~n", [node()]);
        {error, Reason} ->
            io:format("~p: Failed to start routing table server: ~p~n", [node(), Reason]),
            exit({error, Reason})
    end, 

    ieee802154_setup(CurrNodeMacAdd),

    DatagramMap = ets:new(datagram_map, [named_table, public]),

    Data = #{node_mac_addr => CurrNodeMacAdd, datagram_map => DatagramMap, 
            fragment_tag => ?DEFAULT_TAG_VALUE, seqNum => ?BC_SEQNUM, 
            metrics => #metrics{}, ack_req => false},

    set_nodeData_value(state_data, Data),

    io:format("~p: 6lowpan layer successfully launched~n", [node()]),
    io:format("-------------------------------------------------------------------------~n"),
    {ok, idle, Data}.

%% @doc Starts the lowpan API process linked to the current process.
%% @spec start_link(map()) -> {ok, pid()} | {error, Reason}.
start_link(Params) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Params, []).


%% @doc Starts the lowpan API process.
%% @spec start(map()) -> {ok, pid()} | {error, Reason}.
start(Params) ->
    gen_statem:start({local, ?MODULE}, ?MODULE, Params, []).


%% @doc Stops the lowpan API process linked to the current process.
%% @spec stop_link() -> ok.
stop_link() ->
    gen_statem:stop(?MODULE).

%% @doc Stops the lowpan API process.
%% @spec stop() -> ok.
stop() ->
    io:format("lowpan layer stopped"),
    erpc:call(node(), routing_table, stop, []),
    gen_statem:stop(?MODULE).

%-------------------------------------------------------------------------------
%% @doc API function to send an IPv6 packet.
%% @spec sendPacket(binary()) -> ok | {error_multicast_src} | {error_unspecified_addr}.
%-------------------------------------------------------------------------------
sendPacket(Ipv6Pckt) ->
    io:format("Transmission request~n"),
    PcktInfo = lowpan_core:getPcktInfo(Ipv6Pckt),
    SrcAddress = PcktInfo#ipv6PckInfo.sourceAddress,
    DstAddress = PcktInfo#ipv6PckInfo.destAddress,
   
    case {<<SrcAddress:128>>, <<DstAddress:128>>} of
       {<<16#FF:16, _:112>>, _} ->
            io:format("Error, Source address cannot be a multicast address~n"),
            error_multicast_src;
        {_, <<0:128>>} -> 
            io:format("Error, destination address cannot be the Unspecified address~n"),
            error_unspecified_addr;
        _ ->
            Extended_hopsleft = false,
            gen_statem:cast(?MODULE, {pckt_tx, Ipv6Pckt, PcktInfo, Extended_hopsleft, self()}),
            receive 
                Response -> 
                    Response
            end
    end.
%-------------------------------------------------------------------------------
%% @doc API function to send an IPv6 packet with performance metrics enabled. 
%% @spec sendPacket(binary(), boolean()) -> ok | {error_multicast_src} | {error_unspecified_addr}.
%-------------------------------------------------------------------------------
sendPacket(Ipv6Pckt, MetricEnabled) ->
    io:format("Transmission request~n"),
    PcktInfo = lowpan_core:getPcktInfo(Ipv6Pckt),
    SrcAddress = PcktInfo#ipv6PckInfo.sourceAddress,
    DstAddress = PcktInfo#ipv6PckInfo.destAddress,
   
    case {<<SrcAddress:128>>, <<DstAddress:128>>} of
       {<<16#FF:16, _:112>>, _} ->
            io:format("Error, Source address cannot be a multicast address~n"),
            error_multicast_src;
        {_, <<0:128>>} -> 
            io:format("Error, destination address cannot be the Unspecified address~n"),
            error_unspecified_addr;
        _ ->
            Extended_hopsleft = false,
            Response = case MetricEnabled of 
                            true ->
                                gen_statem:cast(?MODULE, {pckt_tx_with_metrics, Ipv6Pckt, PcktInfo, Extended_hopsleft, self()});
                            false -> 
                                gen_statem:cast(?MODULE, {pckt_tx, Ipv6Pckt, PcktInfo, Extended_hopsleft, self()})
                        end,
            receive 
                Response -> 
                    Response;
                {ok, NewMetrics} -> 
                    {ok, RTT, SuccessRate, CompressionRatio} = handle_ack(NewMetrics), 
                    _MetricsResult = {RTT, SuccessRate, CompressionRatio}, 
                    io:format("-----------------Metrics report--------------------~n"),
                    io:format("RTT: ~p ms~nSuccessRate: ~p~nCompressionRatio: ~p~n", [RTT, SuccessRate, CompressionRatio]),
                    io:format("----------------------------------------------------~n"),
                    ok; 
                error_frag_size ->
                    error_frag_size
            end
    end.

%-------------------------------------------------------------------------------
%% @doc API function to send an IPv6 packet with extended hops left option enabled.
%% @spec extendedHopsleftTx(binary()) -> ok | {error_multicast_src} | {error_unspecified_addr} | {error_timeout}.
%-------------------------------------------------------------------------------
extendedHopsleftTx(Ipv6Pckt) ->
    io:format("New packet transmission ~n"),
    PcktInfo = lowpan_core:getPcktInfo(Ipv6Pckt),
    SrcAddress = PcktInfo#ipv6PckInfo.sourceAddress,
    DstAddress = PcktInfo#ipv6PckInfo.destAddress,

    case {<<SrcAddress:128>>, <<DstAddress:128>>} of
        {<<?MULTICAST_PREFIX:16, _Rest:112>>, _} ->
            io:format("Error, Source address cannot be a multicast address~n"),
            error_multicast_src;
        {_, <<0:128>>} -> 
            io:format("Error, destination address cannot be the Unspecified address~n"),
            error_unspecified_addr;
        _ ->
            Extended_hopsleft = true,
            Response = gen_statem:cast(?MODULE, {pckt_tx, Ipv6Pckt, PcktInfo, Extended_hopsleft, self()}),
            receive 
                error_frag_size -> 
                    error_frag_size;
                Response -> 
                    Response
            end
    end.

%-------------------------------------------------------------------------------
%% @doc API function to send an uncompressed IPv6 datagram.
%% @spec sendUncDatagram(binary(), term(), map()) -> ok | {error_timeout}.
%-------------------------------------------------------------------------------
sendUncDatagram(Ipv6Pckt, FrameControl, MacHeader) ->
    gen_statem:cast(?MODULE, {datagram_tx, Ipv6Pckt, FrameControl, MacHeader, self()}),
    receive 
        Response -> 
            Response
    after 1000 ->
            io:format("Timeout~n"),
            error_timeout
    end.

%-------------------------------------------------------------------------------
%% @doc API function to send a frame.
%% @spec tx(binary(), term(), map()) -> ok | error_nalp.
%-------------------------------------------------------------------------------
tx(Frame, FrameControl, MacHeader) ->
    case Frame of 
        <<?NALP_DHTYPE,_/bitstring>> -> 
            io:format("The received frame is not a lowpan frame~n"),
            error_nalp;
        _-> 
            gen_statem:cast(?MODULE, {frame_tx, Frame, FrameControl, MacHeader, self()}),
            receive 
                Response -> 
                    Response
            end
    end.

%-------------------------------------------------------------------------------
%% @doc API function to handle frame reception
%% @spec frameReception() -> term().
%-------------------------------------------------------------------------------
frameReception() ->
    io:format("Reception mode~n"),
    gen_statem:cast(?MODULE, {frame_rx, self()}),
    receive
        {reassembled_packet, IsMeshedPckt, OriginatorMacAddr, CurrNodeMacAdd, ReassembledPacket} ->
            io:format("Datagram reassembled, start packet decoding ~n"),
            _DecodedPacket = lowpan_core:decodeIpv6Pckt(IsMeshedPckt, OriginatorMacAddr, CurrNodeMacAdd, ReassembledPacket),
            % DecodedPacket; % Uncomment this line and comment out the next one to retrieve the decoded packet instead of the reassembled one
            ReassembledPacket; 
        dtg_discarded -> 
            io:format("Datagram successfully discarded ~n"),
            dtg_discarded; 
        {reassembly_timeout, DatagramMap, EntryKey} -> 
            io:format("Reassembly timeout for entry ~p~n", [EntryKey]),
            ets:delete(DatagramMap, EntryKey),
            io:format("Entry deleted~n"),
            reassembly_timeout;             
        error_nalp->
            error_nalp
    after ?REASSEMBLY_TIMEOUT ->
        reassembly_timeout
    end.
     
%-------------------------------------------------------------------------------
% Input callback to handle new received frame.
%-------------------------------------------------------------------------------
inputCallback(Frame, _, _, _) ->
    {FC, MH, Datagram} = Frame,
    {IsMeshedPckt, FinalDstMacAdd, MeshPckInfo} = case lowpan_core:containsMeshHeader(Datagram) of
            {true, MeshInfo} ->
                {true, MeshInfo#meshInfo.final_destination_address, MeshInfo};
            false ->
                {false, MH#mac_header.dest_addr, #{}}
    end,

    OriginatorAddr = case MeshPckInfo of
                        #{}-> MH#mac_header.src_addr;
                        _ -> MeshPckInfo#meshInfo.originator_address
                    end,
    
    StateData = get_nodeData_value(state_data),
    
    processFrame(IsMeshedPckt, MeshPckInfo, OriginatorAddr, FinalDstMacAdd, FC, MH, Datagram, StateData).

%-------------------------------------------------------------------------------
%% @doc Processes new frame.
%% @spec handleDatagram(boolean(), map(), binary(), binary(), term(), map(), binary(), map()) -> term().
%-------------------------------------------------------------------------------
processFrame(IsMeshedPckt, MeshPckInfo, OriginatorAddr, FinalDstMacAdd, FC, MH, Datagram, StateData) ->
    DestAdd = lowpan_core:convertAddrToBin(FinalDstMacAdd),
    #{node_mac_addr := CurrNodeMacAdd} = StateData,

    case DestAdd of
        CurrNodeMacAdd ->
            io:format("New frame received~n"),
            io:format("Originator               : ~p~n",[OriginatorAddr]),
            io:format("Final destination address: ~p~n", [DestAdd]),
            io:format("Current node address     : ~p~n", [CurrNodeMacAdd]),

            io:format("Final destination node reached, Forwarding to lowpan layer~n"),
            case IsMeshedPckt of
                true -> 
                    HopsLeft = MeshPckInfo#meshInfo.hops_left,
                    Rest = lowpan_core:removeMeshHeader(Datagram,HopsLeft),
                    gen_statem:cast(?MODULE, {new_frame_rx, IsMeshedPckt, OriginatorAddr, Rest});
                false-> 
                    HopsLeft = 1,
                    Rest = lowpan_core:removeMeshHeader(Datagram,HopsLeft),
                    gen_statem:cast(?MODULE, {new_frame_rx, IsMeshedPckt, OriginatorAddr, Rest})

            end;
        ?BroadcastAdd ->
            {keep_state, rx_frame};
        _ ->
            io:format("New frame received~n"),
            io:format("Originator               : ~p~n",[OriginatorAddr]),
            io:format("Final destination address: ~p~n", [DestAdd]),
            io:format("Current node address     : ~p~n", [CurrNodeMacAdd]),
            io:format("The datagram needs to be meshed~n"),
            gen_statem:cast(?MODULE, {forward, Datagram, IsMeshedPckt, MeshPckInfo, FinalDstMacAdd, CurrNodeMacAdd, FC, MH})
    end.

%---------- States ---------------------------------------------

%-------------------------------------------------------------------------------
%% @doc In this state the machine waits to received transmission/reception request
%% @spec idle(atom(), term(), map()) -> {next_state, atom(), map(), list()}.
%-------------------------------------------------------------------------------
idle(cast, {pckt_tx, Ipv6Pckt, PcktInfo, Extended_hopsleft, From}, Data) ->
    {next_state, tx_packet, Data#{data => {Ipv6Pckt, PcktInfo, Extended_hopsleft, From}}, [{next_event, internal, {tx_packet}}]}; 

idle(cast, {pckt_tx_with_metrics, Ipv6Pckt, PcktInfo, Extended_hopsleft, From}, Data) ->
    {next_state, tx_packet_metrics, Data#{data => {Ipv6Pckt, PcktInfo, Extended_hopsleft, From}}, [{next_event, internal, {tx_packet_metrics}}]}; 

idle(cast, {frame_tx, Frame, FrameControl, MacHeader, From}, Data) ->
    {next_state, tx_frame, Data#{data => {Frame, FrameControl, MacHeader, From}}, [{next_event, internal, {tx_frame}}]}; 


idle(cast, {datagram_tx, Ipv6Pckt, FrameControl, MacHeader, From}, Data) ->
    {next_state, tx_datagram, Data#{data => {Ipv6Pckt, FrameControl, MacHeader, From}}, [{next_event, internal, {tx_datagram}}]}; 

idle(cast, {frame_rx, From}, Data) ->
    {next_state, rx_frame, Data#{caller => From}, [{next_event, internal, {rx_frame}}]}.


%---------- Tx frame state --------------------------------

%-------------------------------------------------------------------------------
%% @doc Handles the transmission of a frame.
%% @spec tx_frame(atom(), term(), map()) -> {next_state, atom(), map()}.
%-------------------------------------------------------------------------------
tx_frame(internal, {tx_frame}, Data) ->
    #{data := {Frame, FrameControl, MacHeader, From}} = Data,
    Transmit = ieee802154:transmission({FrameControl, MacHeader, Frame}),
    case Transmit of
        {ok, _} ->
            io:format("Packet sent successfully~n"),
            From ! ok,
            {next_state, idle, Data};
        {error, Error} ->
            io:format("Transmission error: ~p~n", [Error]),
            From ! {error, Error},
            {next_state, idle, Data}
    end.

%---------- Tx datagram state ------------------------------

%-------------------------------------------------------------------------------
%% @doc Handles the transmission of a datagram.
%% @spec tx_datagram(atom(), term(), map()) -> {next_state, atom(), map()}.
%-------------------------------------------------------------------------------
tx_datagram(internal, {tx_datagram}, Data) ->
    #{data := {Ipv6Pckt, FrameControl, MacHeader, From}} = Data,
    Transmit = ieee802154:transmission({FrameControl, MacHeader, <<?IPV6_DHTYPE:8, Ipv6Pckt/bitstring>>}),
    case Transmit of
        {ok, _} ->
            From ! ok,
            {next_state, idle, Data};
        {error, Error} ->
            From ! {error, Error},
            {next_state, idle, Data}
    end.

%---------- Tx packet state -------------------------------

%-------------------------------------------------------------------------------
%% @doc Handles the transmission of a packet.
%% @spec tx_packet(atom(), term(), map()) -> {next_state, atom(), map()}.
%-------------------------------------------------------------------------------
tx_packet(internal, {tx_packet}, Data) ->
    #{data := {Ipv6Pckt, PcktInfo, Extended_hopsleft, From}, 
        node_mac_addr := CurrNodeMacAdd, seqNum := SeqNum, fragment_tag := Tag} = Data,
    DestAddress = PcktInfo#ipv6PckInfo.destAddress,
    SrcAddress = PcktInfo#ipv6PckInfo.sourceAddress,
    Payload = PcktInfo#ipv6PckInfo.payload,
    DestMacAddress = lowpan_core:getEUI64MacAddr(DestAddress),
    SenderMacAdd = lowpan_core:getEUI64MacAddr(SrcAddress),
    io:format("Final destination: ~p~n", [DestMacAddress]),
    io:format("Searching next hop...~n"),
    {RouteExist, MeshedHdrBin, MH} = lowpan_core:getNextHop(CurrNodeMacAdd, SenderMacAdd, DestMacAddress, DestAddress, SeqNum+1, Extended_hopsleft),
    {CompressedHeader, _} = lowpan_core:compressIpv6Header(Ipv6Pckt, RouteExist),
    CompressedPacket = <<CompressedHeader/binary, Payload/bitstring>>,
    _CompressedPacketLen = byte_size(CompressedPacket),
    {FragReq, Fragments} = lowpan_core:triggerFragmentation(CompressedPacket, Tag, RouteExist),
    FC = #frame_control{ack_req = ?ENABLED, 
                        frame_type = ?FTYPE_DATA,
                        src_addr_mode = ?EXTENDED,
                        dest_addr_mode = ?EXTENDED},

    case FragReq of
        true ->
            {Response, _NoAckCnt} = sendFragments(RouteExist, Fragments, 1, MeshedHdrBin, MH, FC, Tag, 0),
            NewTag = Tag+1 rem ?MAX_TAG_VALUE,
            From ! Response,
            {next_state, idle, Data#{fragments => Fragments, fragment_tag => NewTag}};
        false ->
            {Response, _NoAckCnt} = sendFragment(RouteExist, Fragments, MeshedHdrBin, MH, FC, Tag),
            NewTag = Tag+1 rem ?MAX_TAG_VALUE,
            From ! Response,
            {next_state, idle, Data#{fragments => Fragments, fragment_tag => NewTag}}; 
        size_err -> 
            io:format("The datagram size exceed the authorized length~n"),
            From ! error_frag_size,
            {next_state, idle, Data}
    end. 
tx_packet_metrics(internal, {tx_packet_metrics}, Data) ->
    #{data := {Ipv6Pckt, PcktInfo, Extended_hopsleft, From}, 
        node_mac_addr := CurrNodeMacAdd, seqNum := SeqNum, metrics := Metrics, fragment_tag := Tag} = Data,
    DestAddress = PcktInfo#ipv6PckInfo.destAddress,
    SrcAddress = PcktInfo#ipv6PckInfo.sourceAddress,
    Payload = PcktInfo#ipv6PckInfo.payload,
    DestMacAddress = lowpan_core:getEUI64MacAddr(DestAddress),
    SenderMacAdd = lowpan_core:getEUI64MacAddr(SrcAddress),
    PcktHeader = lowpan_ipv6:getHeader(Ipv6Pckt),
    io:format("Final destination: ~p~n", [DestMacAddress]),
    io:format("Searching next hop...~n"),
    {RouteExist, MeshedHdrBin, MH} = lowpan_core:getNextHop(CurrNodeMacAdd, SenderMacAdd, DestMacAddress, DestAddress, SeqNum+1, Extended_hopsleft),
    {CompressedHeader, _} = lowpan_core:compressIpv6Header(Ipv6Pckt, RouteExist),
    CompressedPacket = <<CompressedHeader/binary, Payload/bitstring>>,
    CompressedPacketLen = byte_size(CompressedPacket),
    io:format("Compressed packet len: ~p bytes~n",[CompressedPacketLen]),
    {FragReq, Fragments} = lowpan_core:triggerFragmentation(CompressedPacket, Tag, RouteExist),
    FC = #frame_control{ack_req = ?ENABLED, 
                        frame_type = ?FTYPE_DATA,
                        src_addr_mode = ?EXTENDED,
                        dest_addr_mode = ?EXTENDED},

    case FragReq of
        true ->
            NewTag = Tag+1 rem ?MAX_TAG_VALUE,
            StartTime = os:system_time(millisecond),
            
            NewData = Data#{caller => From, fragment_tag => NewTag, ack_req => true}, 
            set_nodeData_value(state_data, NewData),
            {ok, NoAckCnt} = sendFragments(RouteExist, Fragments, 1, MeshedHdrBin, MH, FC, Tag, 0),
            FragmentsNbr = length(Fragments),
            AckCounter = FragmentsNbr - NoAckCnt, 
            NewMetrics = Metrics#metrics{fragments_nbr = FragmentsNbr, ack_counter = AckCounter, start_time = StartTime, 
                                        pckt_len = byte_size(PcktHeader), compressed_pckt_len = byte_size(CompressedHeader)},
            MetricsResult = {ok, NewMetrics},         
            ResetMetrics = Metrics#metrics{fragments_nbr = 0, ack_counter = 0, start_time = 0, 
                                        pckt_len = 0, compressed_pckt_len = 0}, 

            ResetData = Data#{caller => From, ack_req => false, metrics => ResetMetrics},  
            From ! MetricsResult,
            {next_state, idle, ResetData#{fragments => Fragments, fragment_tag => NewTag}};

        false ->
            NewTag = Tag+1 rem ?MAX_TAG_VALUE,
            StartTime = os:system_time(millisecond),
            NewData = Data#{caller => From, fragment_tag => NewTag, ack_req => true}, 
            set_nodeData_value(state_data, NewData),
            {_R, NoAckCnt}= sendFragment(RouteExist, Fragments, MeshedHdrBin, MH, FC, Tag),
            FragmentsNbr = 1,
            AckCounter = FragmentsNbr - NoAckCnt, 
            NewMetrics = Metrics#metrics{fragments_nbr = FragmentsNbr, ack_counter = AckCounter, start_time = StartTime, 
                                        pckt_len = byte_size(PcktHeader), compressed_pckt_len = byte_size(CompressedHeader)},
            MetricsResult = {ok, NewMetrics},         
            ResetMetrics = Metrics#metrics{fragments_nbr = 0, ack_counter = 0, start_time = 0, 
                                        pckt_len = 0, compressed_pckt_len = 0}, 
            ResetData = Data#{caller => From, ack_req => false, metrics => ResetMetrics},  
            From ! MetricsResult,
            {next_state, idle, ResetData#{fragments => Fragments, fragment_tag => NewTag}};
        size_err -> 
            io:format("The datagram size exceed the authorized length~n"),
            From ! error_frag_size,
            {next_state, idle, Data}
    end.

%---------- Rx frame state ------------------------------------

%-------------------------------------------------------------------------------
%% @doc Handles the reception of a frame.
%% @spec rx_frame(atom(), term(), map()) -> {next_state, atom(), map()} | {keep_state, map()}.
%-------------------------------------------------------------------------------
rx_frame(internal, {rx_frame}, Data) ->
    #{caller := From} = Data,
    {keep_state, Data#{caller => From}};

rx_frame(cast, {frame_rx, _From}, Data) ->
    {keep_state, Data};

rx_frame(cast, {new_frame_rx, IsMeshedPckt, OriginatorAddr, Datagram}, Data) ->
    #{caller := From, node_mac_addr := CurrNodeMacAdd} = Data,
    case Datagram of
        <<?IPHC_DHTYPE:3, _Rest/bitstring>> -> 
            io:format("Received a compressed datagram, starting reassembly~n"),
            From ! {reassembled_packet, IsMeshedPckt, OriginatorAddr, CurrNodeMacAdd, Datagram},
            {next_state, idle, Data};

        <<?IPV6_DHTYPE:8, Payload/bitstring>> -> 
            io:format("Received a uncompressed IPv6 datagram, starting reassembly~n"),
            From ! {reassembled_packet, IsMeshedPckt, OriginatorAddr, CurrNodeMacAdd, Payload},
            {next_state, idle, Data};

        <<Type:5, _Rest/bitstring>> when Type =:= ?FRAG1_DHTYPE; Type =:= ?FRAGN_DHTYPE -> 
            FragInfo = lowpan_core:datagramInfo(Datagram),
            Info = FragInfo#datagramInfo.datagramTag,
            NewData = Data#{additional_info => Info},
            io:format("Storing fragment~n"),
            gen_statem:cast(?MODULE, {add_fragment, IsMeshedPckt, OriginatorAddr, Datagram}),
            {keep_state, NewData} 
    end;

rx_frame(cast, {add_fragment, IsMeshedPckt, OriginatorAddr, Datagram}, Data) ->
    {next_state, collect, Data#{is_meshed_pckt => IsMeshedPckt, originator_addr => OriginatorAddr, datagram => Datagram}, 
    [{next_event, internal, {start_collect}}]}; 

rx_frame(cast, {forward, Datagram, IsMeshedPckt, MeshPckInfo, FinalDstMacAdd, CurrNodeMacAdd, FC, MH}, Data) ->
    NewData = Data#{datagram => Datagram, is_meshed_pckt => IsMeshedPckt, 
                    mesh_pck_info => MeshPckInfo, final_dst_mac_add => FinalDstMacAdd, 
                    curr_node_mac_add => CurrNodeMacAdd, fc => FC, mh => MH}, 

    {next_state, forward, NewData, [{next_event, internal, {start_forward}}]}.


%---------- Rx new frame state ------------------------------


%---------- Collect state -----------------------------------

%-------------------------------------------------------------------------------
%% @doc Handles the collection of fragments.
%% @spec collect(atom(), term(), map()) -> {next_state, atom(), map()} | {keep_state, map()}.
%-------------------------------------------------------------------------------
collect(internal, {start_collect}, Data) ->
    #{is_meshed_pckt := IsMeshedPckt, originator_addr := OriginatorAddr, datagram := Datagram, 
    datagram_map := DatagramMap, caller := From, node_mac_addr := CurrNodeMacAdd} = Data,

    DtgInfo = lowpan_core:datagramInfo(Datagram),

    Size = DtgInfo#datagramInfo.datagramSize,
    Tag = DtgInfo#datagramInfo.datagramTag,
    Offset = DtgInfo#datagramInfo.datagramOffset,
    Payload = DtgInfo#datagramInfo.payload,

    Key = {OriginatorAddr, Tag}, 
    CurrTime = os:system_time(second),
    case lowpan_core:storeFragment(DatagramMap, Key, Offset, Payload, CurrTime, Size, Tag, From) of
        {complete_first_frag, ReassembledPacket} ->
            io:format("Complete for pckt ~p~n", [Key]),
            From ! {reassembled_packet, IsMeshedPckt, OriginatorAddr, CurrNodeMacAdd, ReassembledPacket},
            {next_state, idle, Data};

        {complete, UpdatedDatagram} ->
            gen_statem:cast(?MODULE, {complete, IsMeshedPckt, OriginatorAddr, Key, UpdatedDatagram}),
            NewData = Data#{key => Key},
            {keep_state, NewData};

        {duplicate, _} ->
            io:format("Duplicate frame detected~n"),
            NewData = Data#{key => Key},
            {next_state, rx_frame, NewData};

         {incomplete_first, EntryKey} ->
            io:format("Incomplete first datagram, waiting for other fragments ~n"),
            erlang:send_after(?REASSEMBLY_TIMEOUT, From, {reassembly_timeout, DatagramMap, EntryKey}),
            NewData = Data#{key => Key},
            {next_state, rx_frame, NewData};

        {incomplete, _} ->
            io:format("Incomplete datagram, waiting for other fragments ~n"),
            NewData = Data#{key => Key},
            {next_state, rx_frame, NewData}
    end; 

collect(cast, {complete, IsMeshedPckt, OriginatorAddr, Key, UpdatedDatagram}, Data) ->
    NewData =  Data#{is_meshed_pckt => IsMeshedPckt, originator_addr => OriginatorAddr, 
                    key => Key, updated_datagram => UpdatedDatagram},
    {next_state, reassemble, NewData, [{next_event, internal, {start_reassemble}}]}.

%---------- Reassembly state ------------------------------

%-------------------------------------------------------------------------------
%% @doc Handles the reassembly of fragments.
%% @spec reassemble(atom(), term(), map()) -> {next_state, atom(), map()}.
%-------------------------------------------------------------------------------
reassemble(internal, {start_reassemble}, Data) ->
    %io:format("Data: ~p~n", [Data]),
    #{datagram_map := DatagramMap, caller := From, additional_info:=Info,  node_mac_addr := CurrNodeMacAdd, 
       is_meshed_pckt := IsMeshedPckt, originator_addr := OriginatorAddr, 
       key := Key, updated_datagram := UpdatedDatagram} = Data,

    ReassembledPacket = lowpan_core:reassemble(UpdatedDatagram),
    io:format("Complete for pckt ~p~n", [Key]),
    ets:delete(DatagramMap, Key),
    case Info of
        ?INFO_ON -> 
            From ! {additional_info, Info, ReassembledPacket};
        _ -> 
            From ! {reassembled_packet, IsMeshedPckt, OriginatorAddr, CurrNodeMacAdd, ReassembledPacket}
    end,
    {next_state, idle, Data}.

%---------- Forward state ---------------------------------

%-------------------------------------------------------------------------------
%% @doc Handles the forwarding of datagrams.
%% @spec forward(atom(), term(), map()) -> {next_state, atom(), map()}.
%-------------------------------------------------------------------------------
forward(internal, {start_forward}, Data) ->
    #{datagram := Datagram, is_meshed_pckt := IsMeshedPckt, 
                    mesh_pck_info := MeshPckInfo, final_dst_mac_add := FinalDstMacAdd, 
                    curr_node_mac_add := CurrNodeMacAdd, fc := FC, mh := MH} = Data,
    NewDatagram =
        case IsMeshedPckt of
            true ->
                update_datagram(MeshPckInfo, Datagram, Data);
            false ->
                SenderMacAdd = MH#mac_header.src_addr,
                lowpan_core:createNewMeshDatagram(Datagram, SenderMacAdd, FinalDstMacAdd)
        end,
    case NewDatagram of
        {discard, _} ->
            {next_state, rx_frame, Data};
        _ ->
            DestMacAddress = lowpan_core:convertAddrToBin(FinalDstMacAdd),
            io:format("Searching next hop in the routing table...~n"),
            NextHopAddr = routing_table:getRoute(DestMacAddress),

            case NextHopAddr of
                DestMacAddress ->
                    io:format("Direct link found~nForwarding to node: ~p~n", [NextHopAddr]);
                _ ->
                    io:format("Next hop found~nForwarding to node: ~p~n", [NextHopAddr])
            end,
            NewMH = MH#mac_header{src_addr = CurrNodeMacAdd, dest_addr = NextHopAddr},
            io:format("------------------------------------------------------~n"),
            forward_datagram(NewDatagram, FC, NewMH, Data)
    end.
    
%---------- Utility functions -----------------------------------

%-------------------------------------------------------------------------------
%% @doc Sends a fragment
%% @spec sendFragment(boolean(), binary(), binary(), map(), term(), integer()) -> {ok, integer()} | {Error, integer()}.
%-------------------------------------------------------------------------------
sendFragment(RouteExist, CompressedPacket, MeshedHdrBin, MH, FC, Tag) ->
    Pckt = case RouteExist of
                true ->
                    <<MeshedHdrBin/binary, CompressedPacket/bitstring>>;
                false ->
                    CompressedPacket
            end,
    MacHeader = MH#mac_header{seqnum = Tag},
    case ieee802154:transmission({FC, MacHeader, Pckt}) of
        {ok, _} ->
            io:format("~p-byte packet successfully sent~n", [ byte_size(Pckt)]),
            {ok, 0};
        {error, Error} ->
            io:format("Transmission error: ~p~n", [Error]),
            NoAck = 1, 
            {Error, NoAck}
    end.

%-------------------------------------------------------------------------------
%% @doc Sends list of fragments 
%% @spec sendFragments(boolean(), list(), integer(), binary(), map(), term(), integer(), integer()) -> {ok, integer()}.
%-------------------------------------------------------------------------------
sendFragments(RouteExist, [{FragHeader, FragPayload} | Rest], PcktCounter, MeshedHdrBin, MH, FC, Tag, NoAckCnt) ->
    Pckt = case RouteExist of
                true ->
                    <<MeshedHdrBin/binary, FragHeader/binary, FragPayload/bitstring>>;
                false ->
                    <<FragHeader/binary, FragPayload/bitstring>>
            end, 
    MacHeader = MH#mac_header{seqnum = Tag+PcktCounter},
    case ieee802154:transmission({FC, MacHeader, Pckt}) of
        {ok, _} ->
            io:format("~pth fragment: ~p bytes sent~n", [PcktCounter, byte_size(Pckt)]),
            sendFragments(RouteExist, Rest, PcktCounter + 1, MeshedHdrBin, MacHeader, FC, Tag, NoAckCnt);
        {error, Error} ->
            io:format("Error during transmission of fragment ~p: ~p~n", [PcktCounter, Error]),
            sendFragments(RouteExist, Rest, PcktCounter+1, MeshedHdrBin, MacHeader, FC, Tag, NoAckCnt + 1)
    end;          
sendFragments(_RouteExist, [], _PcktCounter, _MeshedHdrBin, _MH, _FC, _Tag, NoAckCnt) ->
    case NoAckCnt of
        0 -> 
            io:format("Packet successfully sent~n"); 
        _-> 
            io:format("Issue during transmission~n")
    end,
    {ok, NoAckCnt}.

%-------------------------------------------------------------------------------
%% @doc Updates the datagram with new mesh header information.
%% @spec update_datagram(map(), binary(), map()) -> binary() | {discard, term()}.
%-------------------------------------------------------------------------------
update_datagram(MeshInfo, Datagram, Data) ->
    HopsLeft = MeshInfo#meshInfo.hops_left, 
    
    {Is_Extended_hopsleft, HopLft} = 
        case HopsLeft of 
                ?DeepHopsLeft -> 
                    HopsLft = MeshInfo#meshInfo.deep_hops_left-1,
                    {true, HopsLft}; 
                 _ -> HopsLft = HopsLeft-1,
                    {false, HopsLft}
        end,

    case {Is_Extended_hopsleft, HopLft}  of
        {_, 0} ->
            {discard, discard_datagram(Datagram, Data)};
        {false, _} ->
            Payload = MeshInfo#meshInfo.payload,
            MeshHeader =
                #mesh_header{v_bit = MeshInfo#meshInfo.v_bit,
                             f_bit = MeshInfo#meshInfo.f_bit,
                             hops_left = HopsLft,
                             originator_address = MeshInfo#meshInfo.originator_address,
                             final_destination_address =  MeshInfo#meshInfo.final_destination_address},

            BinMeshHeader = lowpan_core:buildMeshHeader(MeshHeader),
            <<BinMeshHeader/binary, Payload/bitstring>>; 
        {true, _} ->
            Payload = MeshInfo#meshInfo.payload,
            VBit = MeshInfo#meshInfo.v_bit,
            FBit = MeshInfo#meshInfo.f_bit,
            OriginatorAddress = MeshInfo#meshInfo.originator_address,
            FinalDestinationAddress =  MeshInfo#meshInfo.final_destination_address,

            BinMeshHeader = <<?MESH_DHTYPE:2, VBit:1, FBit:1, ?DeepHopsLeft:4, 
                            OriginatorAddress/binary, FinalDestinationAddress/binary, HopLft:8>>,
            <<BinMeshHeader/binary, Payload/bitstring>>
    end.

%-------------------------------------------------------------------------------
%% @doc Discards the datagram when hop count reaches zero.
%% @spec discard_datagram(binary(), map()) -> {next_state, atom(), map()}.
%-------------------------------------------------------------------------------
discard_datagram(_, Data = #{caller := From})->
    io:format("Hop left value: 0, discarding the datagram~n"),
    From ! dtg_discarded,
    {next_state, rx_frame, Data}.

%-------------------------------------------------------------------------------
%% @doc Forwards a datagram to the next hop.
%% @spec forward_datagram(binary(), term(), map(), map()) -> {next_state, atom(), map()}.
%-------------------------------------------------------------------------------
forward_datagram(Frame, FrameControl, MacHeader, Data = #{caller := From}) ->
    case Frame of 
        <<?NALP_DHTYPE,_/bitstring>> ->
            io:format("The received frame is not a lowpan frame~n"), 
            From ! error_nalp;
        _->
            Transmit = ieee802154:transmission({FrameControl, MacHeader, Frame}),
            case Transmit of
                {ok, _} ->
                    io:format("Packet sent successfully~n");           
                {error, Error} ->
                    io:format("Transmission error: ~p~n", [Error])
            end
    end, 
    io:format("------------------------------------------------------~n"),
    {next_state, rx_frame, Data}.

handle_ack(Metrics) ->
    TotalFragments = Metrics#metrics.fragments_nbr,
    AckCounter = Metrics#metrics.ack_counter,
    EndTime = os:system_time(millisecond),

    RTT = EndTime - Metrics#metrics.start_time,
    SuccessRate = AckCounter / TotalFragments,
    _LossRate = 1 - SuccessRate,

    OrigPcktLen = Metrics#metrics.pckt_len, 
    CompPcktLen = Metrics#metrics.compressed_pckt_len, 
    CompressionRatio = (CompPcktLen/OrigPcktLen),
    {ok, RTT, SuccessRate, CompressionRatio}.

callback_mode() ->
    [state_functions].
%-------------------------------------------------------------------------------
%% @doc Sets up ETS table for node information.
%% @spec setup_node_info_ets() -> atom().
%-------------------------------------------------------------------------------
setup_node_info_ets() ->
    ets:new(nodeData, [named_table, public, {keypos, 1}]).

%-------------------------------------------------------------------------------
%% @doc Sets a value in the node data ETS table.
%% @spec set_nodeData_value(term(), term()) -> ok.
%-------------------------------------------------------------------------------
set_nodeData_value(Key, Value) ->
    ets:insert(nodeData, {Key, Value}).

%-------------------------------------------------------------------------------
%% @doc Retrieves a value from the node data ETS table.
%% @spec get_nodeData_value(term()) -> term() | undefined.
%-------------------------------------------------------------------------------
get_nodeData_value(Key) ->
    case ets:lookup(nodeData, Key) of
        [] ->
            undefined;
        [{_, Value}] ->
            Value
    end.

%-------------------------------------------------------------------------------
%% @doc Sets up the IEEE 802.15.4 layer.
%% @spec ieee802154_setup(binary()) -> ok.
%-------------------------------------------------------------------------------
ieee802154_setup(MacAddr)->
    ieee802154:start(#ieee_parameters{
        phy_layer = mock_phy_network, % comment for deployment on the GRiSP
        duty_cycle = ieee802154_duty_cycle_non_beacon,
        input_callback = fun lowpan_api:inputCallback/4
    }),

    case application:get_env(robot, pan_id) of
        {ok, PanId} ->
            ieee802154:set_pib_attribute(mac_pan_id, PanId);
        _ ->
            ok
    end,

    case byte_size(MacAddr) of 
        ?EXTENDED_ADDR_LEN -> ieee802154:set_pib_attribute(mac_extended_address, MacAddr); 
        ?SHORT_ADDR_LEN -> ieee802154:set_pib_attribute(mac_short_address, MacAddr)
    end, 

    ieee802154:rx_on(), 
    io:format("~p: IEEE 802.15.4 layer successfully launched ~n",[node()]).

