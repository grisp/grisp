%--- Commands ------------------------------------------------------------------

-define(RW_WRITE,         2#0).
-define(RW_READ,          2#1).

-define(MS_SAME,          2#0).
-define(MS_INCR,          2#1).

%=== Accelerometer / Gyroscope =================================================

%--- Registers -----------------------------------------------------------------

-define(ACC_WHO_AM_I, 16#0F).
-define(ACC_OUT_X_XL, 16#28).
-define(ACC_OUT_Y_XL, 16#2A).
-define(ACC_OUT_Z_XL, 16#2C).
-define(CTRL_REG5_XL, 16#1F).
-define(CTRL_REG6_XL, 16#20).

%--- Bit Descriptions ----------------------------------------------------------

-define(ACC_WHO_AM_I_DEFAULT, 2#01101000).

%=== Magnetometer ==============================================================

%--- Registers -----------------------------------------------------------------

-define(MAG_WHO_AM_I, 16#0F).

%--- Bit Descriptions ----------------------------------------------------------

-define(MAG_WHO_AM_I_DEFAULT, 2#00111101).

%=== Altimeter =================================================================

%--- Registers -----------------------------------------------------------------

-define(ALT_WHO_AM_I, 16#0F).

%--- Bit Descriptions ----------------------------------------------------------

-define(ALT_WHO_AM_I_DEFAULT, 2#10111101).
