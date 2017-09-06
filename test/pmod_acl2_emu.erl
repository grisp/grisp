-module(pmod_acl2_emu).

-include("../src/pmod_acl2.hrl").

% Callbacks
-export([init/0]).
-export([message/2]).
-export([broadcast/2]).

-define(SPI_MODE, #{cpol := low, cpha := leading}).

%--- Callbacks -----------------------------------------------------------------

init() -> default().

message(State, {spi, ?SPI_MODE, <<?WRITE_REGISTER, Reg, Value>>})
  when Reg >= ?SOFT_RESET andalso Reg =< ?SELF_TEST ->
    NewState = grisp_bitmap:set_bits(State, Reg*8, <<Value>>),
    {<<0, 0, 0>>, NewState};
message(State, {spi, ?SPI_MODE, <<?READ_REGISTER, Reg, RespBytes/binary>>}) ->
    NewState = shake(State),
    Result = grisp_bitmap:get_bits(NewState, Reg*8, bit_size(RespBytes)),
    {<<0, 0, Result/binary>>, NewState};
message(State, {spi, ?SPI_MODE, _Command}) ->
    {<<0, 0, 0>>, State}.

broadcast(State, _Message) ->
    State.

%--- Internal ------------------------------------------------------------------

shake(State) ->
    case grisp_bitmap:get_bits(State, ?POWER_CTL*8, 8) of
        <<_:6, ?MEASUREMENT_MODE:2>> ->
            lists:foldl(fun({ShortReg, LongReg}, S) ->
                shake_axis(S, ShortReg, LongReg)
            end, State, [
                {?XDATA, ?XDATA_L},
                {?YDATA, ?YDATA_L},
                {?ZDATA, ?ZDATA_L}
            ]);
        _ ->
            State
    end.

shake_axis(State, ShortReg, LongReg) ->
    <<Low, High>> = Long = axis_data_12bit(),
    Short = grisp_bitmap:get_bits(<<High, Low>>, 4, 8),
    NewState = grisp_bitmap:set_bits(State, ShortReg*8, Short),
    grisp_bitmap:set_bits(NewState, LongReg*8, Long).

axis_data_12bit() ->
    <<MSB:1, High:3, Low:8, _:4>> = crypto:strong_rand_bytes(2),
    <<Low:8, MSB:1, MSB:1, MSB:1, MSB:1, MSB:1, High:3>>.

default() ->
    <<% Reset    Reg    Name            RW
        16#AD, % 16#00  DEVID_ID        R
        16#1D, % 16#01  DEVID_MST       R
        16#F2, % 16#02  PARTID          R
        16#01, % 16#03  REVID           R
        16#00, % 16#04
        16#00, % 16#05
        16#00, % 16#06
        16#00, % 16#07
        16#00, % 16#08  XDATA           R
        16#00, % 16#09  YDATA           R
        16#00, % 16#0A  ZDATA           R
        16#40, % 16#0B  STATUS          R
        16#00, % 16#0C  FIFO_ENTRIES_L  R
        16#00, % 16#0D  FIFO_ENTRIES_H  R
        16#00, % 16#0E  XDATA_L         R
        16#00, % 16#0F  XDATA_H         R
        16#00, % 16#10  YDATA_L         R
        16#00, % 16#11  YDATA_H         R
        16#00, % 16#12  ZDATA_L         R
        16#00, % 16#13  ZDATA_H         R
        16#00, % 16#14  TEMP_L          R
        16#00, % 16#15  TEMP_H          R
        16#00, % 16#16
        16#00, % 16#17
        16#00, % 16#18
        16#00, % 16#19
        16#00, % 16#1A
        16#00, % 16#1B
        16#00, % 16#1C
        16#00, % 16#1F  SOFT_RESET      W
        16#00, % 16#1D
        16#00, % 16#1E
        16#00, % 16#20  THRESH_ACT_L    RW
        16#00, % 16#21  THRESH_ACT_H    RW
        16#00, % 16#22  TIME_ACT        RW
        16#00, % 16#23  THRESH_INACT_L  RW
        16#00, % 16#24  THRESH_INACT_H  RW
        16#00, % 16#25  TIME_INACT_L    RW
        16#00, % 16#26  TIME_INACT_H    RW
        16#00, % 16#27  ACT_INACT_CTL   RW
        16#00, % 16#28  FIFO_CONTROL    RW
        16#80, % 16#29  FIFO_SAMPLES    RW
        16#00, % 16#2A  INTMAP1         RW
        16#00, % 16#2B  INTMAP2         RW
        16#13, % 16#2C  FILTER_CTL      RW
        16#00, % 16#2D  POWER_CTL       RW
        16#00  % 16#2E  SELF_TEST       RW
    >>.
