-module(ieee802154_events).

-behaviour(gen_event).

%--- Exports -------------------------------------------------------------------

-export([start/1]).
-export([stop/0]).
-export([rx_event/2]).

% gen_event callbacks
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([terminate/2]).

%--- Includes ------------------------------------------------------------------

-include("ieee802154.hrl").
-include("pmod_uwb.hrl").

%--- Records -------------------------------------------------------------------

-record(state, {input_callback :: ieee802154:input_callback()}).

%--- Types ---------------------------------------------------------------------

-type state() :: #state{}.

%--- API -----------------------------------------------------------------------
% TODO:
% * Notify an event
% * Subscribe to an event => Wait for an event to happen
% * Add a callback for an event ? => Are these events ?
-spec start(Args :: map()) -> ok.
start(Args) ->
    gen_event:add_handler(?GEN_EVENT, ?MODULE, Args).

stop() ->
    gen_event:delete_handler(?GEN_EVENT, ?MODULE, []).

% @doc triggers a rx event
% -spec rx_event(Frame, Metadata) -> ok when
%       Frame :: {integer(), bitstring()},
%       Metadata :: #{snr := float(),
%                     prf := uwb_PRF(),
%                     pre := uwb_preamble_symbol_repetition(),
%                     data_rate := data_rate(),
%                     rng := flag(),
%                     rx_stamp := integer(),
%                     tx_stamp := integer(),
%                     rxtofs := integer(),
%                     rxttcki := integer()}.
rx_event({_, Frame}, Metadata) ->
    gen_event:notify(?GEN_EVENT, {rx, Frame, Metadata}).

%--- gen_event callbacks -------------------------------------------------------
-spec init(InitArgs :: map()) ->
    {ok, State :: state()}.
init(State) ->
    #{input_callback := InputCallback} = State,
    {ok, #state{input_callback = InputCallback}}.

handle_event({rx, Frame, Metadata}, State) ->
    #state{input_callback = InputCallback} = State,
    DecodedFrame = mac_frame:decode(Frame),
    #{snr := Snr,
      rng := Rng,
      rx_stamp := RxStamp,
      tx_stamp := TxStamp,
      rxtofs := Rxtofs,
      rxttcki := Rxttcki} = Metadata,
    RngInfo = rng_infos(Rng, RxStamp, TxStamp,Rxtofs, Rxttcki),
    InputCallback(DecodedFrame, Snr, #security{}, RngInfo),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_, State) ->
    % TODO user should be able to register a callback
    {ok, ok, State}.

handle_info(_, State) ->
    % TODO: nothing here
    {ok, State}.

terminate(_, _) ->
    ok.

%--- Internal ------------------------------------------------------------------

rng_infos(?ENABLED, RxStamp, TxStamp, Rxtofs, Rxttcki) ->
    #ranging_informations{
       ranging_received = ?RANGING_ACTIVE,
       ranging_counter_start = RxStamp,
       ranging_counter_stop = TxStamp,
       ranging_tracking_interval = Rxttcki,
       ranging_offset = Rxtofs,
       ranging_FOM = <<0:8>>
      };
rng_infos(?DISABLED, _, _, _, _) ->
    #ranging_informations{ranging_received = ?NO_RANGING_REQUESTED}.
