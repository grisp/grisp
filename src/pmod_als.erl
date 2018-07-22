-module(pmod_als).

-behavior(gen_server).

-include("grisp.hrl").

% API
-export([start_link/2]).
-export([raw/0]).
-export([precise/0]).

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
    slot,
    value
}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot, _Opts) -> gen_server:start_link(?MODULE, Slot, []).

raw() ->
    Result = call(raw),
    Result.

precise() ->
     Result = call(precise),
     if
      Result < 34 -> grisp_led:color(1,blue);
      Result < 67 -> grisp_led:color(1,green);
      true ->  grisp_led:color(1,red)
    end,
    Result.


%--- Callbacks -----------------------------------------------------------------

% @private
init(Slot) ->
    grisp_devices:register(Slot, ?MODULE),
    erlang:send_after(5000, self(), precise),
    {ok, #state{slot = Slot}}.

% @private
handle_call(raw, _From, State) ->
    Raw = get_value(State#state.slot),
    {reply,  Raw, State}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info(precise, State) ->
  Raw = get_value(State#state.slot),
  Precise = (Raw/255) * 100,
    {noreply, State,1000};

handle_info(timeout, State) ->
  Raw = get_value(State#state.slot),
  Precise = (Raw/255) * 100,
  if
   Precise < 34 -> grisp_led:color(1,blue);
   Precise < 67 -> grisp_led:color(1,green);
   true ->  grisp_led:color(1,red)
 end,
  {noreply, State, 1000}.
% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------

call(Call) ->
    Dev = grisp_devices:default(?MODULE),
    gen_server:call(Dev#device.pid, Call).

get_value(Slot) ->
    <<_:3,Resp:8,Pad:5>> = grisp_spi:send_recv(Slot, ?SPI_MODE, <<0:8>>, 0, 1),
    Resp.
