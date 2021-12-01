% @private
-module(grisp_gpio_poller).

-behavior(gen_server).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- Macros --------------------------------------------------------------------

-define(INTERVAL, 100).

-define(MONITORED_PINS, [
    jumper_1,
    jumper_2,
    jumper_3,
    jumper_4,
    jumper_5
]).

%--- API -----------------------------------------------------------------------

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

%--- gen_server Callbacks ------------------------------------------------------

init(undefined) ->
    erlang:send_after(0, self(), trigger),
    Pins = lists:map(
        fun(P) ->
            {P, grisp_ngpio:open(P, #{mode => input}), undefined}
        end,
        ?MONITORED_PINS
    ),
    {ok, refresh_state(Pins)}.

handle_call(Request, From, _State) -> error({unknown_request, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info(trigger, State) ->
    NewState = refresh_state(State),
    erlang:send_after(?INTERVAL, self(), trigger),
    {noreply, NewState}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------

refresh_state(Pins) ->
    [maybe_notify(P, R, S, grisp_ngpio:get(R)) || {P, R, S} <- Pins].

maybe_notify(Pin, Ref, OldState, OldState) ->
    {Pin, Ref, OldState};
maybe_notify(Pin, Ref, _OldState, NewState) ->
    gen_event:notify(grisp_gpio_events, {Pin, NewState}),
    {Pin, Ref, NewState}.
