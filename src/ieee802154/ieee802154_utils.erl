-module(ieee802154_utils).

%--- Export --------------------------------------------------------------------

-export([pckt_duration/2]).
-export([t_dsym/1]).
-export([symbols_to_usec/2]).

%--- Include -------------------------------------------------------------------

-include("pmod_uwb.hrl").

%--- Macros --------------------------------------------------------------------

-define(N_PHR, 19).
-define(T_D_SYM_1M, 1025.64).

%--- API -----------------------------------------------------------------------

% @doc get the packet duration in ns
pckt_duration(PcktSize, Conf) ->
    #phy_cfg{prf = PRF, psr = PSR, sfd = SFD} = Conf,
    TShr = t_psym(PRF) * (PSR + SFD),
    TPhr = ?N_PHR * ?T_D_SYM_1M,
    TDsym = t_dsym(Conf),
    TPsdu = TDsym * PcktSize * 8,
    TShr + TPhr + TPsdu.

t_psym(16) ->
    993.6;
t_psym(64) ->
    1017.6;
t_psym(PRF) ->
    error({non_supported_prf, PRF}).

% t_dsym according to values of table 99 Std. IEEE.802.15.4
% Over all the possible values supported for the PRF and the channels
% only the bit rate determines T_dsym
t_dsym(#phy_cfg{data_rate = ?DATA_RATE_11KHZ}) ->
    8205.13;
t_dsym(#phy_cfg{data_rate = ?DATA_RATE_84KHZ}) ->
    1025.64;
t_dsym(#phy_cfg{data_rate = ?DATA_RATE_6MHZ}) ->
    128.21;
t_dsym(BitRate) ->
    error({non_supported_bit_rate, BitRate}).

% @doc Converts a value in symbols to usec
% It uses t_psym to perform the convertion (defined in table 99)
% @end
symbols_to_usec(Symbols, #phy_cfg{prf = PRF}) ->
    Symbols * t_psym(PRF) / 1000.