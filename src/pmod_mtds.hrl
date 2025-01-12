% Protocol constants for Digilent's Pmod Multi-Touch Display Shield.
%
% The leading part of a typical packet looks like:
%
%     15 14  | 13 12 11 10 9 8 | 7 6 5 4 3 2 1 0 | ...
%     HEADER | CLASS           | COMMAND         | ...
%
% The C++ driver awkwardly separates these values to achieve byte-alignemnt, but
% we use Erlang's bitstrings to organize them more natively.

-define(SPI_MODE, #{clock => {low, leading}}).

-define(HEADER_CONTROL, 2#00).  % clsProtoChan
-define(HEADER_COMMAND, 2#01).  % clsPacketCmd
-define(HEADER_STATUS,  2#10).  % clsPacketSta
-define(HEADER_DATA,    2#11).  % clsPacketData

% classes in the CONTROL group: 6 bits wide
-define(CONTROL_READ,  16#1).  % chnCmdRead
-define(CONTROL_START, 16#2).  % chnCmdStart
-define(CONTROL_SYNC,  16#3).  % chnCmdSync

% responses in the CONTROL group: 6 bits wide
-define(CONTROL_READY,          16#20).  % chnStaIdle
-define(CONTROL_BUSY,           16#21).  % chnStaBusy
-define(CONTROL_DONE,           16#22).  % chnStaDone
-define(CONTROL_READY_FOR_DATA, 16#23).  % chnStaReady
-define(CONTROL_SYNCING,        16#25).  % chnStaSync

% status codes
-define(STATUS_OK, 16#0).  % staCmdSuccess

% commands in the utility class: 14 bits wide.
-define(UTILITY_READ_STATUS, 16#101).  % cmdUtilReadStatusPacket
-define(UTILITY_INIT,        16#102).  % cmdUtilInit
-define(UTILITY_CLEAR,       16#10B).  % cmdUtilClearDisplay
-define(UTILITY_EVENT_CHECK, 16#111).  % cmdUtilGetMsgStatus
-define(UTILITY_EVENT_POP,   16#114).  % cmdUtilGetMsg

% commands in the gdi / graphics class: 14 bits wide.
-define(GRAPHICS_DS_DISPLAY,     16#201).  % cmdGdiGetDisplayDs
-define(GRAPHICS_DS_NEW,         16#202).  % cmdGdiGetDs
-define(GRAPHICS_DS_RELEASE,     16#203).  % cmdGdiReleaseDs
-define(GRAPHICS_BITMAP_NEW,     16#209).  % cmdGdiCreateBitmap
-define(GRAPHICS_BITMAP_RELEASE, 16#20A).  % cmdGdiDestroyBitmap
-define(GRAPHICS_DS_FG_SET,      16#210).  % cmdGdiSetFgColor
-define(GRAPHICS_DS_BG_SET,      16#212).  % cmdGdiSetBgColor
-define(GRAPHICS_DS_ROP_SET,     16#21C).  % cmdGdiSetDrwRop
-define(GRAPHICS_DS_FONT_SET,    16#220).  % cmdGdiSetFont
-define(GRAPHICS_DS_FONT_GET,    16#221).  % cmdGdiGetFont
-define(GRAPHICS_DS_BITMAP_SET,  16#222).  % cmdGdiSetDrawingSurface
-define(GRAPHICS_DS_MOVE,        16#233).  % cmdGdiMoveTo
-define(GRAPHICS_DS_LINE_TO,     16#235).  % cmdGdiLineTo
% NOTE: 16#240 (cmdGdiTextOutCch) appears unimplemented, reports 16#81.
-define(GRAPHICS_TEXT,           16#241).  % cmdGdiTextOutXcoYco
-define(GRAPHICS_DS_DRAW,        16#24A).  % cmdGdiDrawBitmap

% classes in the HEADER_DATA group: 6 bits wide
-define(DATA_OUT, 16#1).  % clsDataOut

-define(PACKET_OUT_MAX_SIZE, 512).  % cbDhdrDataOutMax

% Device-side memory region constants.
-define(REGION_SURFACE, 16#C0000000).  % sigHds
-define(REGION_BITMAP,  16#C1000000).  % sigHbmp
-define(REGION_BRUSH,   16#C2000000).  % sigHbr
-define(REGION_FONT,    16#C3000000).  % sigHfnt
-define(REGION_WINDOW,  16#C4000000).  % sigHwin
-define(HANDLE_SPACE,   16#00FFFFFF).  % size of each handle space

-define(STOCK_FONT,   16#00300000).  % sigHfntStock
% NOTE: hfntXSmall through hfntXLarge appear to be unimplemented, crash MTDS.
-define(STOCK_WINDOW, 16#00400000).  % sigHwinStock
