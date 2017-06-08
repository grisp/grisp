%--- Commands ------------------------------------------------------------------

-define(RW_WRITE,         2#0).
-define(RW_READ,          2#1).

-define(MS_SAME,          2#0).
-define(MS_INCR,          2#1).

%--- Registers -----------------------------------------------------------------

-define(WHO_AM_I,         16#0F).
-define(CTRL_REG1,        16#20).
-define(OUT_TEMP,         16#26).
-define(OUT_X_L,          16#28).

%--- Bit Descriptions ----------------------------------------------------------

% WHO_AM_I
-define(DEVID,            2#11010011).

% CTRL_REG1
-define(PD_POWER_DOWN,    2#0).
-define(PD_NORMAL,        2#1).
-define(Zen_ENABLED,      2#1).
-define(Yen_ENABLED,      2#1).
-define(Xen_ENABLED,      2#1).
