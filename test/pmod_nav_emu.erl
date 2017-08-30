-module(pmod_nav_emu).

-include("../src/pmod_nav.hrl").

% Callbacks
-export([init/0]).
-export([message/2]).
-export([broadcast/2]).

-define(SPI_MODE, #{cpol := low, cpha := trailing}).

%--- Records -------------------------------------------------------------------

-record(state, {
    pins    = #{
        spi1_pin9  => 1,
        spi1_pin10 => 1,
        ss1        => 0
    },
    acc_gyro = default_acc_gyro(),
    mag      = default_mag()
}).

%--- Callbacks -----------------------------------------------------------------

init() -> #state{}.

message(State, {spi, ?SPI_MODE, <<>>}) -> {<<0, 0, 0>>, State}.

broadcast(#state{pins = Pins} = State, {gpio, Pin, {configure, Mode, _}}) ->
    State#state{pins = maps:update(Pin, value(Mode), Pins)}.

%--- Internal ------------------------------------------------------------------

default_acc_gyro() ->
    << % Default            Reg    Name                  Type
        0,                % 16#00  Reserved              --
        0,                % 16#01  Reserved              --
        0,                % 16#02  Reserved              --
        0,                % 16#03  Reserved              --
        2#00000000,       % 16#04  ACT_THS               r/w
        2#00000000,       % 16#05  ACT_DUR               r/w
        2#00000000,       % 16#06  INT_GEN_CFG_XL        r/w
        2#00000000,       % 16#07  INT_GEN_THS_X_XL      r/w
        2#00000000,       % 16#08  INT_GEN_THS_Y_XL      r/w
        2#00000000,       % 16#09  INT_GEN_THS_Z_XL      r/w
        2#00000000,       % 16#0A  INT_GEN_DUR_XL        r/w
        2#00000000,       % 16#0B  REFERENCE_G           r/w
        2#00000000,       % 16#0C  INT1_CTRL             r/w
        2#00000000,       % 16#0D  INT2_CTRL             r/w
        0,                % 16#0E  Reserved              --
        2#01101000,       % 16#0F  WHO_AM_I              r
        2#00000000,       % 16#10  CTRL_REG1_G           r/w
        2#00000000,       % 16#11  CTRL_REG2_G           r/w
        2#00000000,       % 16#12  CTRL_REG3_G           r/w
        2#00000000,       % 16#13  ORIENT_CFG_G          r/w
        0,                % 16#14  INT_GEN_SRC_G         r
        0,                % 16#15  OUT_TEMP_L            r
        0,                % 16#16  OUT_TEMP_H            r
        0,                % 16#17  STATUS_REG            r
        0,                % 16#18  OUT_X_L_G             r
        0,                % 16#19  OUT_X_H_G             r
        0,                % 16#1A  OUT_Y_L_G             r
        0,                % 16#1B  OUT_Y_H_G             r
        0,                % 16#1C  OUT_Z_L_G             r
        0,                % 16#1D  OUT_Z_H_G             r
        2#00111000,       % 16#1E  CTRL_REG4             r/w
        2#00111000,       % 16#1F  CTRL_REG5_XL          r/w
        2#00000000,       % 16#20  CTRL_REG6_XL          r/w
        2#00000000,       % 16#21  CTRL_REG7_XL          r/w
        2#00000100,       % 16#22  CTRL_REG8             r/w
        2#00000000,       % 16#23  CTRL_REG9             r/w
        2#00000000,       % 16#24  CTRL_REG10            r/w
        0,                % 16#25  Reserved              --
        0,                % 16#26  INT_GEN_SRC_XL        r
        0,                % 16#27  STATUS_REG            r
        0,                % 16#28  OUT_X_L_XL            r
        0,                % 16#29  OUT_X_H_XL            r
        0,                % 16#2A  OUT_Y_L_XL            r
        0,                % 16#2B  OUT_Y_H_XL            r
        0,                % 16#2C  OUT_Z_L_XL            r
        0,                % 16#2D  OUT_Z_H_XL            r
        2#00000000,       % 16#2E  FIFO_CTRL             r/w
        0,                % 16#2F  FIFO_SRC              r
        2#00000000,       % 16#30  INT_GEN_CFG_G         r/w
        2#00000000,       % 16#31  INT_GEN_THS_XH_G      r/w
        2#00000000,       % 16#32  INT_GEN_THS_XL_G      r/w
        2#00000000,       % 16#33  INT_GEN_THS_YH_G      r/w
        2#00000000,       % 16#34  INT_GEN_THS_YL_G      r/w
        2#00000000,       % 16#35  INT_GEN_THS_ZH_G      r/w
        2#00000000,       % 16#36  INT_GEN_THS_ZL_G      r/w
        2#00000000        % 16#37  INT_GEN_DUR_G         r/w
    >>.

default_mag() ->
    << % Default            Reg    Name                  Type
        0,                % 16#00  Reserved              --
        0,                % 16#01  Reserved              --
        0,                % 16#02  Reserved              --
        0,                % 16#03  Reserved              --
        0,                % 16#04  Reserved              --
        2#00000000,       % 16#05  OFFSET_X_REG_L_M      r/w
        2#00000000,       % 16#06  OFFSET_X_REG_H_M      r/w
        2#00000000,       % 16#07  OFFSET_Y_REG_L_M      r/w
        2#00000000,       % 16#08  OFFSET_Y_REG_H_M      r/w
        2#00000000,       % 16#09  OFFSET_Z_REG_L_M      r/w
        2#00000000,       % 16#0A  OFFSET_Z_REG_H_M      r/w
        0,                % 16#0B  Reserved              --
        0,                % 16#0C  Reserved              --
        0,                % 16#0D  Reserved              --
        0,                % 16#0E  Reserved              --
        2#00111101,       % 16#0F  WHO_AM_I_M            r
        0,                % 16#10  Reserved              --
        0,                % 16#11  Reserved              --
        0,                % 16#12  Reserved              --
        0,                % 16#13  Reserved              --
        0,                % 16#14  Reserved              --
        0,                % 16#15  Reserved              --
        0,                % 16#16  Reserved              --
        0,                % 16#17  Reserved              --
        0,                % 16#18  Reserved              --
        0,                % 16#19  Reserved              --
        0,                % 16#1A  Reserved              --
        0,                % 16#1B  Reserved              --
        0,                % 16#1C  Reserved              --
        0,                % 16#1D  Reserved              --
        0,                % 16#1E  Reserved              --
        0,                % 16#1F  Reserved              --
        2#00010000,       % 16#20  CTRL_REG1_M           r/w
        2#00000000,       % 16#21  CTRL_REG2_M           r/w
        2#00000011,       % 16#22  CTRL_REG3_M           r/w
        2#00000000,       % 16#23  CTRL_REG4_M           r/w
        2#00000000,       % 16#24  CTRL_REG5_M           r/w
        0,                % 16#25  Reserved              --
        0,                % 16#26  Reserved              --
        0,                % 16#27  STATUS_REG_M          r
        0,                % 16#28  OUT_X_L_M             r
        0,                % 16#29  OUT_X_H_M             r
        0,                % 16#2A  OUT_Y_L_M             r
        0,                % 16#2B  OUT_Y_H_M             r
        0,                % 16#2C  OUT_Z_L_M             r
        0,                % 16#2D  OUT_Z_H_M             r
        0,                % 16#2E  Reserved              --
        0,                % 16#2F  Reserved              --
        2#00001000,       % 16#30  INT_CFG_M             r/w
        2#00000000,       % 16#31  INT_SRC_M             r
        2#00000000,       % 16#32  INT_THS_L_M           r
        2#00000000        % 16#33  INT_THS_H_M           r
    >>.

value(output_0) -> 0;
value(output_1) -> 1.
