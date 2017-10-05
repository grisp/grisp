
%--- Registers -----------------------------------------------------------------

-define(COMMUNICATIONS, 2#000).			% for write operations
-define(STATUS,         2#000).			% for read operations
-define(MODE,           2#001).
-define(CONFIGURATION,  2#010).
-define(DATA,           2#011).
-define(ID,             2#100).

%--- Bit Descriptions ----------------------------------------------------------

% COMMUNICATIONS
-define(WRITE_ENABLE,      2#0).
-define(WRITE_DISABLE,     2#1).

-define(RW_WRITE,          2#0).
-define(RW_READ,           2#1).

-define(CONT_READ_DISABLE, 2#0).
-define(CONT_READ_ENABLE,  2#1).

-define(RDY_READY,         2#0).
-define(RDY_WAIT,          2#1).

% ID
-define(DEVID, 2).

% CONFIG
-define(CONFIG_CHANNELS, [short, temp, ch7, ch6, ch5, ch4, ch3, ch2, ch1, ch0]).
-define(CONFIG_BITS, [
    %Flag       Size    Default
    {chop,      1,      0},
    {0,         1,      0},
    {0,         1,      0},
    {refsel,    1,      0},
    {0,         1,      0},
    {pseudo,    1,      0},
    {short,     1,      0},
    {temp,      1,      0},
    {ch7,       1,      0},
    {ch6,       1,      0},
    {ch5,       1,      0},
    {ch4,       1,      0},
    {ch3,       1,      0},
    {ch2,       1,      0},
    {ch1,       1,      0},
    {ch0,       1,      0},
    {burn,      1,      0},
    {refdet,    1,      0},
    {0,         1,      0},
    {buf,       1,      1},
    {unb,       1,      0},
    {gain,      3,      7}
]).

% MODE

-define(DAT_STA_ENABLE,  2#1).
-define(DAT_STA_DISABLE, 2#0).

-define(MODE_BITS, [
    %Flag       Size    Default
    {md,        3,      0},
    {dat_sta,   1,      ?DAT_STA_DISABLE},
    {clk,       2,      2},
    {avg,       2,      0},
    {sinc3,     1,      0},
    {0,         1,      0},
    {enpar,     1,      0},
    {clk_div,   1,      0},
    {single,    1,      0},
    {rej60,     1,      0},
    {fs,        10,     16#60}
]).
