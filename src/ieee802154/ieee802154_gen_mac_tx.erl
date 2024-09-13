%% @doc This generic module defines the behaviour for any module implementing the transmission of the IEEE 802.15.4 stack
%% @end

-module(ieee802154_gen_mac_tx).

-export([start/2]).
-export([transmit/4]).
-export([stop/2]).

-include("pmod_uwb.hrl").
-include("ieee802154.hrl").
-include("ieee802154_pib.hrl").

%--- Callbacks -----------------------------------------------------------------

-callback init(PhyMod::module()) -> State::term().
-callback tx(State::term(),
             Frame::bitstring(),
             Pib  :: pib_state(),
             TxOptions::#tx_opts{}) -> {ok, Newstate::term()}
                                       | {error,
                                          Newstate::term(),
                                          Error::tx_error()}.
-callback terminate(State::term(), Reason::atom()) -> ok.

%--- Types ---------------------------------------------------------------------

-export_type([state/0]).

-opaque state() :: {Module::module(), Sub::term()}.

%--- API -----------------------------------------------------------------------
-spec start(Module, PhyMod) -> State when
      PhyMod :: module(),
      Module :: module(),
      State  :: state().
start(Module, PhyMod) ->
    {Module, Module:init(PhyMod)}.

-spec transmit(State, Frame, Pib, TxOptions) -> Result when
      State     :: state(),
      Frame     :: bitstring(),
      Pib       :: pib_state(),
      TxOptions :: tx_opts(),
      Result    :: {ok, State} | {error, State, Error},
      Error     :: tx_error().
transmit({Module, Sub}, Frame, Pib, TxOptions) ->
    case Module:tx(Sub, Frame, Pib, TxOptions) of
        {ok, Sub2} ->
            {ok, {Module, Sub2}};
        {error, Sub2, Error} -> {error, {Module, Sub2}, Error}
    end.

-spec stop(State, Reason) -> ok when
      State  :: state(),
      Reason :: atom().
stop({Module, Sub}, Reason) ->
    Module:terminate(Sub, Reason).