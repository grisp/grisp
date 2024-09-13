%--- Macros --------------------------------------------------------------------

%--- MCPS-DATA.indication Parameters

% Ranging Received values:
-define(NO_RANGING_REQUESTED, 0).
-define(RANGING_REQUESTED_BUT_NOT_SUPPORTED, 1).
-define(RANGING_ACTIVE, 2).

% Ranging Transmission values:
-define(NON_RANGING, 0).
-define(ALL_RANGING, 1).
% PHY_HEADER_ONLY => Not supported in our case

% CSMA constants
-define(MACMAXFRAMERETRIES, 5).
-define(MACACKWAITDURATION, 4000).  % works with 2000 µs but calculations give me 4081µs
% -define(MACACKWAITDURATION, 2000).  % works with 2000 µs but calculations give me 4081µs
%
-define(GEN_EVENT, gen_event).

%--- Types ---------------------------------------------------------------------
%--- Record types
-record(ieee_parameters, {duty_cycle = ieee802154_duty_cycle_non_beacon :: module(),
                          phy_layer = pmod_uwb :: module(),
                          input_callback = fun(_, _, _, _) -> ok end :: input_callback()}).


-record(ranging_informations, {ranging_received = ?NO_RANGING_REQUESTED :: ranging_received() | boolean(),
                               ranging_counter_start = 0                :: integer(),
                               ranging_counter_stop = 0                 :: integer(),
                               ranging_tracking_interval = 0            :: integer(),
                               ranging_offset = 0                       :: integer(),
                               ranging_FOM = <<16#00:8>>                :: bitstring()}).

-type ranging_informations() :: #ranging_informations{}.

% For now security isn't enabled
-record(security, {security_level = 0       :: integer(),
                   key_id_mode = 0          :: integer(),
                   key_source = <<16#00:8>> :: bitstring()}).

-type security() :: #security{}.

%--- IEEE 802.15.4 parameter types
-export_type([ieee_parameters/0, ranging_informations/0, security/0, input_callback/0, ranging_tx/0, tx_error/0]).

-type ranging_received() :: ?NO_RANGING_REQUESTED | ?RANGING_REQUESTED_BUT_NOT_SUPPORTED | ?RANGING_ACTIVE.
-type ranging_tx() :: ?NON_RANGING | ?ALL_RANGING. % PHY_HEADER_ONLY no used in our case

% *** indicates unusefull parameters for higher layers for now
-type input_callback() :: fun((Frame                  :: mac_frame:frame(),
                               LQI                    :: integer(),
                               % UWBPRF                 :: gen_mac_layer:uwb_PRF(),
                               Security               :: security(),
                               % UWBPreambleRepetitions :: pmod_uwb:uwb_preamble_symbol_repetition(),
                               % DataRate               :: pmod_uwb:data_rate(),
                               Ranging                :: ranging_informations())
                              -> ok).

-type ieee_parameters() :: #ieee_parameters{}.

-type tx_error() :: invalid_address | invalid_gts | transaction_overflow | transaction_expired | no_ack | frame_too_long | channel_access_failure.
