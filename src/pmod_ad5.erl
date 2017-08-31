-module(pmod_ad5).

-behavior(gen_server).

-include("grisp.hrl").
-include("pmod_ad5.hrl").

% API
-export([start_link/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(SPI_MODE, #{cpol => high, cpha => trailing}).

%--- Records -------------------------------------------------------------------

-record(state, {
    slot
}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot) -> gen_server:start_link(?MODULE, Slot, []).



%--- Callbacks -----------------------------------------------------------------

% @private
init(Slot) ->
    verify_device(Slot),
    grisp_devices:register(Slot, ?MODULE),
    {ok, #state{slot = Slot}}.

% @private
handle_call(tbd, _From, State) ->
    {reply, not_implemented, State}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

verify_device(Slot) ->
    <<_:4, 2:4>> = read(Slot, 2#100, 1).

read(Slot, Reg, Size) ->
    grisp_spi:send_recv(Slot, ?SPI_MODE, <<0:1, 1:1, Reg:3, 0:1, 0:2>>, 
			1, Size).
