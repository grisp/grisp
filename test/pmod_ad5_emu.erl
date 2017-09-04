-module(pmod_ad5_emu).

-include("../src/pmod_ad5.hrl").

% Callbacks
-export([init/0]).
-export([message/2]).
-export([broadcast/2]).

-define(SPI_MODE, #{cpol := high, cpha := trailing}).

%--- Callbacks -----------------------------------------------------------------

init() -> state.

message(State, {spi, ?SPI_MODE, <<?WRITE_ENABLE:1, ?RW_READ:1, Reg:3, ?CONT_READ_DISABLE:1, 0:2, Value/binary>>}) ->
    {<<0, (read(Reg, Value))/binary>>, State}.

broadcast(State, _Message) -> State.

%--- Internal ------------------------------------------------------------------

read(?ID, <<_>>) -> <<2#111:4, ?DEVID:4>>.
