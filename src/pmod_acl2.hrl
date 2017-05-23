%--- Commands ------------------------------------------------------------------

-define(WRITE_REGISTER,   16#0A).
-define(READ_REGISTER,    16#0B).
-define(READ_FIFO,        16#0D).

%--- Registers -----------------------------------------------------------------

-define(XDATA,            16#08).
-define(YDATA,            16#09).
-define(ZDATA,            16#0A).

-define(XDATA_L,          16#0E).
-define(XDATA_H,          16#0F).
-define(YDATA_L,          16#10).
-define(YDATA_H,          16#11).
-define(ZDATA_L,          16#12).
-define(ZDATA_H,          16#13).

-define(SOFT_RESET,       16#1F).
-define(POWER_CTL,        16#2D).
-define(SELF_TEST,        16#2E).

%--- Bit Descriptions ----------------------------------------------------------

% POWER_CTL
-define(STANDBY,          2#00).
-define(MEASUREMENT_MODE, 2#10).
