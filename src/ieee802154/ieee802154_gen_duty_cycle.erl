% @doc This module defines a generic behaviour for duty cycling on the IEEE 802.15.4
%
% The module implementing the behaviour will be responsible to manage the duty cylcing of the IEEE 802.15.4 stack (not the power optimization of the pmod)
% For example, the module implementing this behaviour for a beacon enabled network will have the task to manage the CFP, the CAP and the beacon reception
% When an application will request a transmission the module has to suspend the rx before transmitting
% At the transmission of a frame, the module will have the task to check if there is enough time to transmit the frame (e.g. before the next beacon)
% At the transmisson of a data frame with AR=1 the module has to manage the retransmission of the frame if the ACK isn't correctly received
%   This is because this module will be responsible to check if the retransmission can be done (no beacons or not a CAP) and the reception can't be resumed between retransmission (both are responsabilities of this module) 
%
% Beacon enabled
% No transmission during beacon
% TX during CAP
% No TX during CFP unless a slot is attributed to the node
%
% Manage the RX loop (suspend/resume)
%
% @end
-module(ieee802154_gen_duty_cycle).

-include("ieee802154.hrl").
-include("ieee802154_pib.hrl").
-include("pmod_uwb.hrl").

-callback init(PhyModule) -> State when
      PhyModule :: module(),
      State     :: term().
-callback on(State) -> Result when
      State       :: term(),
      Result      :: {ok, State}
                     | {error, State, Error},
      Error       :: atom().
-callback off(State) -> {ok, State} when
      State :: term().
% Add suspend and resume later
-callback tx(State, Frame, Pib, Ranging) -> Result when
      State       :: term(),
      Frame       :: bitstring(),
      Pib         :: pib_state(),
      Ranging     :: ranging_tx(),
      Result      :: {ok, State, RangingInfo}
                    | {error, State, Error},
      RangingInfo :: ranging_informations(),
      Error       :: tx_error().
-callback terminate(State, Reason) -> ok when
      State  :: term(),
      Reason :: term().

-export([start/2]).
-export([turn_on/1]).
-export([turn_off/1]).
-export([tx_request/4]).
-export([stop/2]).

%--- Types ---------------------------------------------------------------------

-export_type([state/0, input_callback_raw_frame/0]).

-opaque state() :: {Module::module(), Sub::term()}.

-type input_callback_raw_frame() :: fun((Frame                  :: binary(),
                                         LQI                    :: integer(),
                                         UWBPRF                 :: uwb_PRF(),
                                         Security               :: ieee802154:security(),
                                         UWBPreambleRepetitions :: uwb_preamble_symbol_repetition(),
                                         DataRate               :: data_rate(),
                                         Ranging                :: ieee802154:ranging_informations())
                                        -> ok).

%--- API -----------------------------------------------------------------------

% @doc initialize the duty cycle module
% @end
-spec start(Module, PhyModule) -> State when
      Module :: module(),
      PhyModule :: module(),
      State :: state().
start(Module, PhyModule) ->
    {Module, Module:init(PhyModule)}.

% @doc turns on the continuous reception
% @TODO specify which RX module has to be used
-spec turn_on(State) -> Result when
      State    :: state(),
      Result   :: {ok, State} | {error, State, Error},
      Error    :: atom().
turn_on({Mod, Sub}) ->
    case Mod:on(Sub) of
        {ok, Sub2} -> {ok, {Mod, Sub2}};
        {error, Sub2, Error} -> {error, {Mod, Sub2}, Error}
    end.

% @doc turns off the continuous reception
-spec turn_off(State) -> State when
      State :: state().
turn_off({Mod, Sub}) ->
    {ok, Sub2} = Mod:off(Sub),
    {Mod, Sub2}.

% @doc request a transmission to the duty cycle
% The frame is an encoded MAC frame ready to be transmitted
% If the frame request an ACK, the retransmission is managed by the module
%
% Errors:
% <li> `no_ack': No acknowledgment received after macMaxFrameRetries</li>
% <li> `frame_too_long': The frame was too long for the CAP or GTS</li>
% <li> `channel_access_failure': the CSMA-CA algorithm failed</li>
% @end
-spec tx_request(State, Frame, Pib, Ranging) -> Result when
      State       :: state(),
      Frame       :: bitstring(),
      Pib         :: pib_state(),
      Ranging     :: ranging_tx(),
      State       :: state(),
      Result      :: {ok, State, RangingInfo}
                     | {error, State, Error},
      RangingInfo :: ranging_informations(),
      Error       :: tx_error().
tx_request({Mod, Sub}, Frame, Pib, Ranging) ->
    case Mod:tx(Sub, Frame, Pib, Ranging) of
        {ok, Sub2, RangingInfo} ->
            {ok, {Mod, Sub2}, RangingInfo};
        {error, Sub2, Err} ->
            {error, {Mod, Sub2}, Err}
    end.

% @doc stop the duty cycle module
-spec stop(State, Reason) -> ok when
      State  :: state(),
      Reason :: atom().
stop({Mod, Sub}, Reason) ->
    Mod:terminate(Sub, Reason).