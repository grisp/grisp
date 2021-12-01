-define(GRISP_EEPROM_SOM_PROC, grisp_eeprom_som).
-define(GRISP_EEPROM_SOM_ADDR, 16#52).
-define(GRISP_EEPROM_SOM_BUS, i2c0).
-define(GRISP_EEPROM_SOM_WAIT, 5).
-define(GRISP_EEPROM_BOARD_PROC, grisp_eeprom_board).
-define(GRISP_EEPROM_BOARD_ADDR, 16#57).
-define(GRISP_EEPROM_BOARD_BUS, i2c0).
-define(GRISP_EEPROM_BOARD_WAIT, 5).

-type grisp_eeprom() :: som | board.
