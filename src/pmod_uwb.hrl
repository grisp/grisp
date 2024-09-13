-define(ENABLED, 2#1).
-define(DISABLED, 2#0).

-define(DATA_RATE_11KHZ, 2#00).
-define(DATA_RATE_84KHZ, 2#01).
-define(DATA_RATE_6MHZ, 2#10).

% w4r_tim is the delay between the tx is done and the moment the rx will be enabled (it is not a timeout)
-record(tx_opts, {wait4resp = ?DISABLED:: flag(),
                  w4r_tim = 0 :: miliseconds(),
                  txdlys = ?DISABLED:: flag(),
                  tx_delay = 300 :: integer(),
                  ranging = ?DISABLED :: flag()}).

-type tx_opts() :: #tx_opts{}.

-record(phy_cfg, {psr = 1024                  :: uwb_preamble_symbol_repetition(),
                  prf = 16                    :: uwb_PRF(),
                  sfd = 8                     :: uwb_sfd(),
                  data_rate = ?DATA_RATE_6MHZ :: data_rate(),
                  channel = 5                 :: uwb_channel(),
                  pac_size = 8                :: uwb_pac_size()}).

-type phy_cfg() :: #phy_cfg{}.

-type flag() :: ?DISABLED| ?ENABLED.

-type miliseconds() :: integer().
-type microseconds() :: integer().

% map the r/w bit of the transaction header

-type writeOnly() :: tx_buffer.
-type readOnly() :: dev_id | sys_time | rx_finfo | rx_buffer | rx_fqual | rx_ttcki | rx_ttcko | rx_time | tx_time | sys_state | acc_mem.

-type uwb_PRF() :: 0 | 4 | 16 | 64.
-type uwb_sfd() :: 8 | 64.
-type uwb_preamble_symbol_repetition() :: 0 | 16 | 64 | 1024 | 4096. % Or PSR
-type data_rate() :: 0..4. % Mapping defined in Std. at table 105 p. 206
-type uwb_channel() :: 1..5 | 7.
-type uwb_pac_size() :: 8 | 16 | 32 | 64.