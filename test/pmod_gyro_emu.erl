-module(pmod_gyro_emu).

-include("../src/pmod_gyro.hrl").

% Callbacks
-export([init/0]).
-export([message/2]).
-export([broadcast/2]).

-define(SPI_MODE, #{cpol := high, cpha := trailing}).

%--- Callbacks -----------------------------------------------------------------

init() -> default().

message(State, {spi, ?SPI_MODE, <<?RW_READ:1, ?MS_INCR:1, Reg:6, RespBytes/binary>>}) ->
    NewState = rotate(State),
    Result = grisp_bitmap:get_bytes(NewState, Reg, byte_size(RespBytes)),
    {<<0, Result/binary>>, NewState};
message(State, {spi, ?SPI_MODE, <<?RW_READ:1, ?MS_SAME:1, Reg:6, RespBytes/binary>>}) ->
    {Result, NewState} = lists:foldl(fun(_, {R, S}) ->
        NewS = rotate(S),
        IR = grisp_bitmap:get_bytes(NewS, Reg, 1),
        {<<R/binary, IR/binary>>, NewS}
    end, {<<>>, State}, lists:seq(1, byte_size(RespBytes))),
    {<<0, Result/binary>>, NewState};
message(State, {spi, ?SPI_MODE, <<?RW_WRITE:1, ?MS_INCR:1, Reg:6, Value/binary>>}) ->
    NewState = grisp_bitmap:set_bytes(State, Reg, Value),
    {<<0, 0:(bit_size(Value))>>, NewState}.

broadcast(State, _Message) ->
    State.

%--- Internal ------------------------------------------------------------------

rotate(State) ->
    case grisp_bitmap:get_bytes(State, ?CTRL_REG1, 1) of
        <<_:4, ?PD_NORMAL:1, _:3>> ->
            lists:foldl(fun({Reg, Len}, S) ->
                grisp_bitmap:set_bytes(S, Reg, crypto:strong_rand_bytes(Len))
            end, State, [
                {?OUT_TEMP, 1},
                {?OUT_X_L, 6}
            ]);
        _ ->
            State
    end.

default() ->
    <<  % Default     Addr  Name            Type
        0,          % 00    Reserved        -
        0,          % 01    Reserved        -
        0,          % 02    Reserved        -
        0,          % 03    Reserved        -
        0,          % 04    Reserved        -
        0,          % 05    Reserved        -
        0,          % 06    Reserved        -
        0,          % 07    Reserved        -
        0,          % 08    Reserved        -
        0,          % 09    Reserved        -
        0,          % 0A    Reserved        -
        0,          % 0B    Reserved        -
        0,          % 0C    Reserved        -
        0,          % 0D    Reserved        -
        0,          % 0E    Reserved        -
        2#11010011, % 0F    WHO_AM_I        R
        0,          % 10    Reserved        -
        0,          % 11    Reserved        -
        0,          % 12    Reserved        -
        0,          % 13    Reserved        -
        0,          % 14    Reserved        -
        0,          % 15    Reserved        -
        0,          % 16    Reserved        -
        0,          % 17    Reserved        -
        0,          % 18    Reserved        -
        0,          % 19    Reserved        -
        0,          % 1A    Reserved        -
        0,          % 1B    Reserved        -
        0,          % 1C    Reserved        -
        0,          % 1D    Reserved        -
        0,          % 1E    Reserved        -
        0,          % 1F    Reserved        -
        2#00000111, % 20    CTRL_REG1       RW
        2#00000000, % 21    CTRL_REG2       RW
        2#00000000, % 22    CTRL_REG3       RW
        2#00000000, % 23    CTRL_REG4       RW
        2#00000000, % 24    CTRL_REG5       RW
        2#00000000, % 25    REFERENCE       RW
        0,          % 26    OUT_TEMP        R
        0,          % 27    STATUS_REG      R
        0,          % 28    OUT_X_L         R
        0,          % 29    OUT_X_H         R
        0,          % 2A    OUT_Y_L         R
        0,          % 2B    OUT_Y_H         R
        0,          % 2C    OUT_Z_L         R
        0,          % 2D    OUT_Z_H         R
        2#00000000, % 2E    FIFO_CTRL_REG   RW
        0,          % 2F    FIFO_SRC_REG    R
        2#00000000, % 30    INT1_CFG        RW
        0,          % 31    INT1_SRC        R
        2#00000000, % 32    INT1_TSH_XH     RW
        2#00000000, % 33    INT1_TSH_XL     RW
        2#00000000, % 34    INT1_TSH_YH     RW
        2#00000000, % 35    INT1_TSH_YL     RW
        2#00000000, % 36    INT1_TSH_ZH     RW
        2#00000000, % 37    INT1_TSH_ZL     RW
        2#00000000  % 38    INT1_DURATION   RW
    >>.
