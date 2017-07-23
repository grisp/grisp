%%% Flags 

%% Needs to be in sync with lib/include/linux/i2c.h

-define(I2C_M_TEN, 16#0010).
-define(I2C_M_RD, 16#0001).
-define(I2C_M_STOP, 16#8000).
-define(I2C_M_NOSTART, 16#4000).
-define(I2C_M_REV_DIR_ADDR, 16#2000).
-define(I2C_M_IGNORE_NAK, 16#1000).
-define(I2C_M_NO_RD_ACK, 16#0800).
-define(I2C_M_RECV_LEN, 16#0400).
