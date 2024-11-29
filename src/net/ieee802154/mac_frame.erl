-module(mac_frame).

-include("mac_frame.hrl").

-export([encode/2]).
-export([encode/3]).
-export([encode_ack/2]).
-export([decode/1]).

%-------------------------------------------------------------------------------
% @doc builds a mac frame without a payload
% @equiv encode(FrameControl, MacHeader, <<>>)
% @end
%-------------------------------------------------------------------------------
-spec encode(FrameControl :: #frame_control{}, MacHeader :: #mac_header{}) -> bitstring().
encode(FrameControl, MacHeader) ->
    encode(FrameControl, MacHeader, <<>>).

%-------------------------------------------------------------------------------
% @doc builds a mac frame
% @returns a MAC frame ready to be transmitted in a bitstring (not including the CRC automatically added by the DW1000)
% @end
%-------------------------------------------------------------------------------
-spec encode(FrameControl :: frame_control(), MacHeader :: mac_header(), Payload :: bitstring()) -> bitstring().
encode(FrameControl, MacHeader, Payload) ->
    Header = build_mac_header(FrameControl, MacHeader),
    <<Header/bitstring, Payload/bitstring>>.

%-------------------------------------------------------------------------------
% @doc Builds an ACK frame 
% @returns a MAC frame ready to be transmitted in a bitstring (not including the CRC automatically added by the DW1000)
% @end
%-------------------------------------------------------------------------------
encode_ack(FramePending, Seqnum) ->
    FC = build_frame_control(#frame_control{frame_type = ?FTYPE_ACK, frame_pending = FramePending, dest_addr_mode = ?NONE, src_addr_mode = ?NONE}),
    <<FC/bitstring, Seqnum:8>>.

%-------------------------------------------------------------------------------
% @doc builds a mac header based on the FrameControl and the MacHeader structures given in the args.
% <b> The MAC header doesn't support security fields yet </b>
% @returns the MAC header in a bitstring
% @end
%-------------------------------------------------------------------------------
-spec build_mac_header(FrameControl, MacHeader) -> binary() when
      FrameControl :: frame_control(),
      MacHeader    :: mac_header().
build_mac_header(FrameControl, MacHeader) ->
    FC = build_frame_control(FrameControl),

    DestPan = reverse_byte_order(MacHeader#mac_header.dest_pan),
    DestAddr= reverse_byte_order(MacHeader#mac_header.dest_addr),
    DestAddrFields = case FrameControl#frame_control.dest_addr_mode of
                         ?NONE -> <<>>;
                         _ -> <<DestPan/bitstring, DestAddr/bitstring>> 
                    end,

    SrcPan = reverse_byte_order(MacHeader#mac_header.src_pan),
    SrcAddr= reverse_byte_order(MacHeader#mac_header.src_addr),
    SrcAddrFields = case {FrameControl#frame_control.src_addr_mode, FrameControl#frame_control.pan_id_compr, FrameControl#frame_control.dest_addr_mode} of
                        {?NONE, _, _} -> <<>>;
                        {_, ?DISABLED, _} -> <<SrcPan/bitstring, SrcAddr/bitstring>>; % if no compression is applied on PANID and SRC addr is present
                        {_, ?ENABLED, ?NONE}  -> <<SrcPan/bitstring, SrcAddr/bitstring>>; % if there is a compression of the PANID but the dest addr isn't present
                        {_, ?ENABLED, _} -> <<SrcAddr/bitstring>> % if there is a compression of the PANID and the dest addr is present
                    end,
    <<FC/bitstring,  (MacHeader#mac_header.seqnum):8, DestAddrFields/bitstring, SrcAddrFields/bitstring>>.


%-------------------------------------------------------------------------------
% @doc decodes the MAC frame given in the arguments
% @return A tuple containing the decoded frame control, the decoded mac header and the payload
% @end
%-------------------------------------------------------------------------------
-spec decode(Data) -> {FrameControl, MacHeader, Payload} when
      Data         :: binary(),
      FrameControl :: frame_control(),
      MacHeader    :: mac_header(),
      Payload      :: bitstring(). 
decode(Data) ->
    <<FC:16/bitstring, Seqnum:8, Rest/bitstring>> = Data,
    FrameControl = decode_frame_control(FC),
    decode_rest(FrameControl, Seqnum, Rest).

%-------------------------------------------------------------------------------
% @private
% @doc Decodes the remaining sequence of bit present in the payload after the seqnum
% @end
%-------------------------------------------------------------------------------
-spec decode_rest(FrameControl :: frame_control(), Seqnum::integer(), Rest::binary()) -> {FrameControl::frame_control(), MacHeader::mac_header(), Payload::binary()}.  
decode_rest(#frame_control{frame_type = ?FTYPE_ACK} = FrameControl, Seqnum, _Rest) -> % Might cause an issue if piggybacking is used (allowed in IEEE 802.15.4?)
    {FrameControl, #mac_header{seqnum = Seqnum}, <<>>};
decode_rest(FrameControl, Seqnum, Rest) ->
    {DestPAN, DestAddr, SrcPAN, SrcAddr, Payload} = decode_mac_header(FrameControl#frame_control.dest_addr_mode, FrameControl#frame_control.src_addr_mode, FrameControl#frame_control.pan_id_compr, Rest),
    MacHeader = #mac_header{seqnum = Seqnum, dest_pan = reverse_byte_order(DestPAN), dest_addr = reverse_byte_order(DestAddr), src_pan = reverse_byte_order(SrcPAN), src_addr = reverse_byte_order(SrcAddr)},
    {FrameControl, MacHeader, Payload}.

% Note extended addresses and PAN ID are used in the case of inter-PAN communication
% In inter PAN communication, it can be omitted but it's not mandatory
-spec decode_mac_header(DestAddrMode, SrcAddrMode, PanIdCompr, Bits) -> {DestPAN, DestAddr, SrcPAN, SrcAddr, Payload} when
      DestAddrMode :: flag(),
      SrcAddrMode  :: flag(),
      PanIdCompr   :: flag(),
      Bits         :: bitstring(),
      DestPAN      :: binary(),
      DestAddr     :: binary(),
      SrcPAN       :: binary(),
      SrcAddr      :: binary(),
      Payload      :: binary().
decode_mac_header(?EXTENDED, ?EXTENDED, ?DISABLED, <<DestPAN:16/bitstring, DestAddr:64/bitstring, SrcPAN:16/bitstring, SrcAddr:64/bitstring, Payload/bitstring>>) -> 
    {DestPAN, DestAddr, SrcPAN, SrcAddr, Payload};

decode_mac_header(?EXTENDED, ?EXTENDED, ?ENABLED, <<DestPAN:16/bitstring, DestAddr:64/bitstring, SrcAddr:64/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, DestPAN, SrcAddr, Payload};

decode_mac_header(?EXTENDED, ?SHORT_ADDR, ?DISABLED, <<DestPAN:16/bitstring, DestAddr:64/bitstring, SrcPAN:16/bitstring, SrcAddr:16/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, SrcPAN, SrcAddr, Payload};

decode_mac_header(?EXTENDED, ?SHORT_ADDR, ?ENABLED, <<DestPAN:16/bitstring, DestAddr:64/bitstring, SrcAddr:16/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, DestPAN, SrcAddr, Payload};

decode_mac_header(?EXTENDED, ?NONE, _, <<DestPAN:16/bitstring, DestAddr:64/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, <<>>, <<>>, Payload};

decode_mac_header(?SHORT_ADDR, ?EXTENDED, ?DISABLED, <<DestPAN:16/bitstring, DestAddr:16/bitstring, SrcPAN:16/bitstring, SrcAddr:64/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, SrcPAN, SrcAddr, Payload};

decode_mac_header(?SHORT_ADDR, ?EXTENDED, ?ENABLED, <<DestPAN:16/bitstring, DestAddr:16/bitstring, SrcAddr:64/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, DestPAN, SrcAddr, Payload};

decode_mac_header(?SHORT_ADDR, ?SHORT_ADDR, ?DISABLED, <<DestPAN:16/bitstring, DestAddr:16/bitstring, SrcPAN:16/bitstring, SrcAddr:16/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, SrcPAN, SrcAddr, Payload};

decode_mac_header(?SHORT_ADDR, ?SHORT_ADDR, ?ENABLED, <<DestPAN:16/bitstring, DestAddr:16/bitstring, SrcAddr:16/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, DestPAN, SrcAddr, Payload};

decode_mac_header(?SHORT_ADDR, ?NONE, _, <<DestPAN:16/bitstring, DestAddr:16/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, <<>>, <<>>, Payload};

decode_mac_header(?NONE, ?EXTENDED, _, <<SrcPAN:16/bitstring, SrcAddr:64/bitstring, Payload/bitstring>>) ->
    {<<>>, <<>>, SrcPAN, SrcAddr, Payload};

decode_mac_header(?NONE, ?SHORT_ADDR, _, <<SrcPAN:16/bitstring, SrcAddr:16/bitstring, Payload/bitstring>>) ->
    {<<>>, <<>>, SrcPAN, SrcAddr, Payload};

decode_mac_header(_SrcAddrMode, _DestAddrMode, _PanIdCompr, _Bits) ->
    error(internal_decoding_error).

%-------------------------------------------------------------------------------
% @private
% @doc Creates a MAC frame control 
% @param FrameType: MAC frame type
% @param AR: ACK request
% @end
%-------------------------------------------------------------------------------
-spec build_frame_control(FrameControl) -> <<_:16>> when
      FrameControl :: frame_control().
build_frame_control(FrameControl) ->
    #frame_control{pan_id_compr=PanIdCompr,ack_req=AckReq,frame_pending=FramePending,sec_en=SecEn,
                   frame_type=FrameType,src_addr_mode=SrcAddrMode,frame_version=FrameVersion,dest_addr_mode=DestAddrMode} = FrameControl,
    <<2#0:1, PanIdCompr:1, AckReq:1, FramePending:1, SecEn:1, FrameType:3, SrcAddrMode:2, FrameVersion:2, DestAddrMode:2, 2#0:2>>.


%-------------------------------------------------------------------------------
% @private
% @doc Decode the frame control given in a bitstring form in the parameters
% @end
%-------------------------------------------------------------------------------
-spec decode_frame_control(FC) -> frame_control() when
      FC :: <<_:16>>.
decode_frame_control(FC) -> 
    <<_:1, PanIdCompr:1, AckReq:1, FramePending:1, SecEn:1, FrameType:3, SrcAddrMode:2, FrameVersion:2, DestAddrMode:2, _:2>> = FC,
    #frame_control{frame_type = FrameType, sec_en = SecEn, frame_pending = FramePending, ack_req = AckReq, pan_id_compr = PanIdCompr, dest_addr_mode = DestAddrMode, frame_version = FrameVersion, src_addr_mode = SrcAddrMode}.

%--- Tool functions ------------------------------------------------------------

% reverse_byte_order(Bitstring) -> 
%     Size = bit_size(Bitstring),
%     <<X:Size/integer-little>> = Bitstring,
%     <<X:Size/integer-big>>.
reverse_byte_order(Bitstring) -> reverse_byte_order(Bitstring, <<>>).
reverse_byte_order(<<>>, Acc) -> Acc;
reverse_byte_order(<<Head:8>>, Acc) ->
    <<Head:8, Acc/bitstring>>;
reverse_byte_order(<<Head:8, Tail/bitstring>>, Acc) ->
    reverse_byte_order(Tail, <<Head:8, Acc/bitstring>>).