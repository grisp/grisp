-module(pmod_nav_emu).

-include("../src/pmod_nav.hrl").

% Callbacks
-export([init/0]).
-export([message/2]).
-export([broadcast/2]).

-define(SPI_MODE, #{cpol := high, cpha := trailing}).

-define(ACC(State), State#state.pins =:= #{
    ss1        => {output_1, 0},
    spi1_pin9  => {output_1, 1},
    spi1_pin10 => {output_1, 1}
}).
-define(MAG(State), State#state.pins =:= #{
    ss1        => {output_1, 1},
    spi1_pin9  => {output_1, 0},
    spi1_pin10 => {output_1, 1}
}).
-define(ALT(State), State#state.pins =:= #{
    ss1        => {output_1, 1},
    spi1_pin9  => {output_1, 1},
    spi1_pin10 => {output_1, 0}
}).
-define(PIN(Pin), Pin == ss1 orelse Pin == spi1_pin9 orelse Pin == spi1_pin10).

%--- Records -------------------------------------------------------------------

-record(state, {
    pins    = #{
        ss1        => {periph_c, 1},
        spi1_pin9  => {input_1, 1},
        spi1_pin10 => {input_1, 1}
    },
    bitmaps = #{
        acc_gyro => default_acc_gyro(),
        mag      => default_mag(),
        alt      => default_alt()
    }
}).

%--- Callbacks -----------------------------------------------------------------

init() -> #state{}.

message(State, {spi, ?SPI_MODE, Req}) when ?ACC(State) ->
    component(State, acc_gyro, Req);
message(State, {spi, ?SPI_MODE, Req}) when ?MAG(State) ->
    component(State, mag, Req);
message(State, {spi, ?SPI_MODE, Req}) when ?ALT(State) ->
    component(State, alt, Req);
message(State, {spi, ?SPI_MODE, <<_Req, Value/binary>>}) ->
    {<<0, (binary:copy(<<0>>, byte_size(Value)))/binary>>, State}.

broadcast(#state{pins = Pins} = State, {gpio, Pin, {configure, Mode, _}}) when ?PIN(Pin) ->
    State#state{pins = maps:update(Pin, {Mode, value(Mode)}, Pins)};
broadcast(#state{pins = Pins} = State, {gpio, Pin, clear}) ->
    NewPins = maps:update_with(Pin, fun({Mode, _}) -> {Mode, 0} end, Pins),
    State#state{pins = NewPins};
broadcast(#state{pins = Pins} = State, {gpio, Pin, set}) ->
    NewPins = maps:update_with(Pin, fun({Mode, _}) -> {Mode, 1} end, Pins),
    State#state{pins = NewPins};
% FIXME: Remove specific pins and add catchall clause in the end
broadcast(State, {gpio, jumper_1, get}) ->
    State;
broadcast(State, {gpio, jumper_2, get}) ->
    State;
broadcast(State, {gpio, jumper_3, get}) ->
    State;
broadcast(State, {gpio, jumper_4, get}) ->
    State;
broadcast(State, {gpio, jumper_5, get}) ->
    State.

%--- Internal ------------------------------------------------------------------

component(#state{bitmaps = Bitmaps} = State, Component, Req) ->
    {Result, Bitmap} = call(Component, maps:get(Component, Bitmaps), Req),
    {Result, State#state{bitmaps = maps:put(Component, Bitmap, Bitmaps)}}.

call(acc_gyro, Bin, <<?RW_READ:1, Reg:7, Val/binary>>) ->
    read(shake(acc_gyro, Bin), Reg, byte_size(Val));
call(acc_gyro, Bin, <<?RW_WRITE:1, Reg:7, Val/binary>>) ->
    write(Bin, Reg, Val);
call(mag, Bin, <<?RW_READ:1, ?MS_INCR:1, Reg:6, Val/binary>>) ->
    read(Bin, Reg, byte_size(Val));
call(mag, Bin, <<?RW_READ:1, ?MS_SAME:1, Reg:6, Val/binary>>) ->
    Result = grisp_bitmap:get_bytes(Bin, Reg, 1),
    {<<0, (binary:copy(Result, byte_size(Val)))/binary>>, Bin};
call(mag, Bin, <<?RW_WRITE:1, ?MS_INCR:1, Reg:6, Val/binary>>) ->
    write(Bin, Reg, Val);
call(mag, Bin, <<?RW_WRITE:1, ?MS_SAME:1, Reg:6, Val/binary>>) ->
    write(Bin, Reg, binary:last(Val));
call(alt, Bin, <<?RW_READ:1, ?MS_INCR:1, Reg:6, Val/binary>>) ->
    read(Bin, Reg, byte_size(Val));
call(alt, Bin, <<?RW_READ:1, ?MS_SAME:1, Reg:6, Val/binary>>) ->
    Result = grisp_bitmap:get_bytes(Bin, Reg, 1),
    {<<0, (binary:copy(Result, byte_size(Val)))/binary>>, Bin};
call(alt, Bin, <<?RW_WRITE:1, ?MS_INCR:1, Reg:6, Val/binary>>) ->
    write(Bin, Reg, Val);
call(alt, Bin, <<?RW_WRITE:1, ?MS_SAME:1, Reg:6, Val/binary>>) ->
    write(Bin, Reg, binary:last(Val)).

read(Bin, Reg, Length) ->
    Result = grisp_bitmap:get_bytes(Bin, Reg, Length),
    {<<0, Result/binary>>, Bin}.

write(Bin, Reg, Value) ->
    NewBin = grisp_bitmap:set_bytes(Bin, Reg, Value),
    {<<0, (binary:copy(<<0>>, byte_size(Value)))/binary>>, NewBin}.

value(output_1) -> 1;
value(_)        -> undefined.

shake(acc_gyro, Bin) ->
    case grisp_bitmap:get_bytes(Bin, 16#20, 1) of
        <<2#000:3, _:5>> -> % ODR in power-down mode
            Bin;
        _ ->
            grisp_bitmap:set_bytes(Bin, 16#28, crypto:strong_rand_bytes(6))
    end.

default_acc_gyro() ->
    << % Default          Reg    Name                   Type
        0,              % 16#00  Reserved               --
        0,              % 16#01  Reserved               --
        0,              % 16#02  Reserved               --
        0,              % 16#03  Reserved               --
        2#00000000,     % 16#04  ACT_THS                r/w
        2#00000000,     % 16#05  ACT_DUR                r/w
        2#00000000,     % 16#06  INT_GEN_CFG_XL         r/w
        2#00000000,     % 16#07  INT_GEN_THS_X_XL       r/w
        2#00000000,     % 16#08  INT_GEN_THS_Y_XL       r/w
        2#00000000,     % 16#09  INT_GEN_THS_Z_XL       r/w
        2#00000000,     % 16#0A  INT_GEN_DUR_XL         r/w
        2#00000000,     % 16#0B  REFERENCE_G            r/w
        2#00000000,     % 16#0C  INT1_CTRL              r/w
        2#00000000,     % 16#0D  INT2_CTRL              r/w
        0,              % 16#0E  Reserved               --
        2#01101000,     % 16#0F  WHO_AM_I               r
        2#00000000,     % 16#10  CTRL_REG1_G            r/w
        2#00000000,     % 16#11  CTRL_REG2_G            r/w
        2#00000000,     % 16#12  CTRL_REG3_G            r/w
        2#00000000,     % 16#13  ORIENT_CFG_G           r/w
        0,              % 16#14  INT_GEN_SRC_G          r
        0,              % 16#15  OUT_TEMP_L             r
        0,              % 16#16  OUT_TEMP_H             r
        0,              % 16#17  STATUS_REG             r
        0,              % 16#18  OUT_X_L_G              r
        0,              % 16#19  OUT_X_H_G              r
        0,              % 16#1A  OUT_Y_L_G              r
        0,              % 16#1B  OUT_Y_H_G              r
        0,              % 16#1C  OUT_Z_L_G              r
        0,              % 16#1D  OUT_Z_H_G              r
        2#00111000,     % 16#1E  CTRL_REG4              r/w
        2#00111000,     % 16#1F  CTRL_REG5_XL           r/w
        2#00000000,     % 16#20  CTRL_REG6_XL           r/w
        2#00000000,     % 16#21  CTRL_REG7_XL           r/w
        2#00000100,     % 16#22  CTRL_REG8              r/w
        2#00000000,     % 16#23  CTRL_REG9              r/w
        2#00000000,     % 16#24  CTRL_REG10             r/w
        0,              % 16#25  Reserved               --
        0,              % 16#26  INT_GEN_SRC_XL         r
        0,              % 16#27  STATUS_REG             r
        0,              % 16#28  OUT_X_L_XL             r
        0,              % 16#29  OUT_X_H_XL             r
        0,              % 16#2A  OUT_Y_L_XL             r
        0,              % 16#2B  OUT_Y_H_XL             r
        0,              % 16#2C  OUT_Z_L_XL             r
        0,              % 16#2D  OUT_Z_H_XL             r
        2#00000000,     % 16#2E  FIFO_CTRL              r/w
        0,              % 16#2F  FIFO_SRC               r
        2#00000000,     % 16#30  INT_GEN_CFG_G          r/w
        2#00000000,     % 16#31  INT_GEN_THS_XH_G       r/w
        2#00000000,     % 16#32  INT_GEN_THS_XL_G       r/w
        2#00000000,     % 16#33  INT_GEN_THS_YH_G       r/w
        2#00000000,     % 16#34  INT_GEN_THS_YL_G       r/w
        2#00000000,     % 16#35  INT_GEN_THS_ZH_G       r/w
        2#00000000,     % 16#36  INT_GEN_THS_ZL_G       r/w
        2#00000000      % 16#37  INT_GEN_DUR_G          r/w
    >>.

default_mag() ->
    << % Default          Reg    Name                   Type
        0,              % 16#00  Reserved               --
        0,              % 16#01  Reserved               --
        0,              % 16#02  Reserved               --
        0,              % 16#03  Reserved               --
        0,              % 16#04  Reserved               --
        2#00000000,     % 16#05  OFFSET_X_REG_L_M       r/w
        2#00000000,     % 16#06  OFFSET_X_REG_H_M       r/w
        2#00000000,     % 16#07  OFFSET_Y_REG_L_M       r/w
        2#00000000,     % 16#08  OFFSET_Y_REG_H_M       r/w
        2#00000000,     % 16#09  OFFSET_Z_REG_L_M       r/w
        2#00000000,     % 16#0A  OFFSET_Z_REG_H_M       r/w
        0,              % 16#0B  Reserved               --
        0,              % 16#0C  Reserved               --
        0,              % 16#0D  Reserved               --
        0,              % 16#0E  Reserved               --
        2#00111101,     % 16#0F  WHO_AM_I_M             r
        0,              % 16#10  Reserved               --
        0,              % 16#11  Reserved               --
        0,              % 16#12  Reserved               --
        0,              % 16#13  Reserved               --
        0,              % 16#14  Reserved               --
        0,              % 16#15  Reserved               --
        0,              % 16#16  Reserved               --
        0,              % 16#17  Reserved               --
        0,              % 16#18  Reserved               --
        0,              % 16#19  Reserved               --
        0,              % 16#1A  Reserved               --
        0,              % 16#1B  Reserved               --
        0,              % 16#1C  Reserved               --
        0,              % 16#1D  Reserved               --
        0,              % 16#1E  Reserved               --
        0,              % 16#1F  Reserved               --
        2#00010000,     % 16#20  CTRL_REG1_M            r/w
        2#00000000,     % 16#21  CTRL_REG2_M            r/w
        2#00000011,     % 16#22  CTRL_REG3_M            r/w
        2#00000000,     % 16#23  CTRL_REG4_M            r/w
        2#00000000,     % 16#24  CTRL_REG5_M            r/w
        0,              % 16#25  Reserved               --
        0,              % 16#26  Reserved               --
        0,              % 16#27  STATUS_REG_M           r
        0,              % 16#28  OUT_X_L_M              r
        0,              % 16#29  OUT_X_H_M              r
        0,              % 16#2A  OUT_Y_L_M              r
        0,              % 16#2B  OUT_Y_H_M              r
        0,              % 16#2C  OUT_Z_L_M              r
        0,              % 16#2D  OUT_Z_H_M              r
        0,              % 16#2E  Reserved               --
        0,              % 16#2F  Reserved               --
        2#00001000,     % 16#30  INT_CFG_M              r/w
        2#00000000,     % 16#31  INT_SRC_M              r
        2#00000000,     % 16#32  INT_THS_L_M            r
        2#00000000      % 16#33  INT_THS_H_M            r
    >>.

default_alt() ->
    << % Default          Reg    Name                   Type
        0,              % 16#00  Reserved               --
        0,              % 16#01  Reserved               --
        0,              % 16#02  Reserved               --
        0,              % 16#03  Reserved               --
        0,              % 16#04  Reserved               --
        0,              % 16#05  Reserved               --
        0,              % 16#06  Reserved               --
        0,              % 16#07  Reserved               --
        0,              % 16#08  REF_P_XL               R/W
        0,              % 16#09  REF_P_L                R/W
        0,              % 16#0A  REF_P_H                R/W
        0,              % 16#0B  ?                      --
        0,              % 16#0C  ?                      --
        0,              % 16#0D  Reserved               --
        0,              % 16#0E  Reserved               --
        2#10111101,     % 16#0F  WHO_AM_I               R
        2#00001111,     % 16#10  RES_CONF               R/W
        0,              % 16#11  Reserved               --
        0,              % 16#12  Reserved               --
        0,              % 16#13  Reserved               --
        0,              % 16#14  Reserved               --
        0,              % 16#15  Reserved               --
        0,              % 16#16  Reserved               --
        0,              % 16#17  Reserved               --
        0,              % 16#18  Reserved               --
        0,              % 16#19  Reserved               --
        0,              % 16#1A  Reserved               --
        0,              % 16#1B  Reserved               --
        0,              % 16#1C  Reserved               --
        0,              % 16#1D  Reserved               --
        0,              % 16#1E  Reserved               --
        0,              % 16#1F  Reserved               --
        0,              % 16#20  CTRL_REG1              R/W
        0,              % 16#21  CTRL_REG2              R/W
        0,              % 16#22  CTRL_REG3              R/W
        0,              % 16#23  CTRL_REG4              R/W
        0,              % 16#24  INTERRUPT_CFG          R/W
        0,              % 16#25  INT_SOURCE             R
        0,              % 16#26  Reserved               --
        0,              % 16#27  STATUS_REG             R
        0,              % 16#28  PRESS_OUT_XL           R
        0,              % 16#29  PRESS_OUT_L            R
        0,              % 16#2A  PRESS_OUT_H            R
        0,              % 16#2B  TEMP_OUT_L             R
        0,              % 16#2C  TEMP_OUT_H             R
        0,              % 16#2D  Reserved               --
        0,              % 16#2E  FIFO_CTRL              R/W
        2#00100000,     % 16#2F  FIFO_STATUS            R
        0,              % 16#30  THS_P_L                R/W
        0,              % 16#31  THS_P_H                R/W
        0,              % 16#32  Reserved               --
        0,              % 16#33  Reserved               --
        0,              % 16#34  Reserved               --
        0,              % 16#35  Reserved               --
        0,              % 16#36  Reserved               --
        0,              % 16#37  Reserved               --
        0,              % 16#38  Reserved               --
        0,              % 16#39  RPDS_L                 R/W
        0               % 16#3A  RPDS_H                 R/W
    >>.
