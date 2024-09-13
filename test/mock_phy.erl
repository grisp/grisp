-module(mock_phy).
-behaviour(gen_server).

-export([start_link/2]).
-export([start/2]).
-export([stop_link/0]).

-export([transmit/2]).
-export([reception_async/0]).
-export([reception/0]).
-export([reception/1]).
-export([disable_rx/0]).

-export([set_frame_timeout/1]).
-export([set_preamble_timeout/1]).
-export([disable_preamble_timeout/0]).

-export([suspend_frame_filtering/0]).
-export([resume_frame_filtering/0]).

-export([read/1]).
-export([write/2]).

-export([rx_ranging_info/0]).
-export([signal_power/0]).
-export([rx_preamble_repetition/0]).
-export([rx_data_rate/0]).
-export([prf_value/0]).
-export([get_conf/0]).

%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

%--- Include -------------------------------------------------------------------

-include("pmod_uwb.hrl").

%--- Macros --------------------------------------------------------------------

-define(NAME, mock_phy).

% --- API -----------------------------------------

start_link(_Connector, Params) ->
    gen_server:start_link({local, ?NAME}, ?MODULE, Params, []).

start(_Connector, Params) ->
    gen_server:start({local, ?NAME}, ?MODULE, Params, []).

stop_link() ->
    gen_server:stop(?NAME).

transmit(Data, Options) ->
    gen_server:call(?NAME, {transmit, Data, Options}).

reception_async() ->
    Frame =  gen_server:call(?NAME, {reception}),
    Metadata = #{snr => 10.0,
                  prf => 4,
                  pre => 16,
                  data_rate => 1,
                  rng => ?DISABLED,
                  rx_stamp => 1,
                  tx_stamp => 1,
                  rxtofs => 1,
                  rxttcki => 1},
    ieee802154_events:rx_event(Frame, Metadata).

reception() ->
    gen_server:call(?NAME, {reception}).

reception(_) ->
    gen_server:call(?NAME, {reception}).

set_frame_timeout(Timeout) ->
    write(rx_fwto, #{rxfwto => Timeout}),
    write(sys_cfg, #{rxwtoe => 2#1}). % enable receive wait timeout

set_preamble_timeout(Timeout) ->
    write(drx_conf, #{drx_pretoc => Timeout}).

disable_preamble_timeout() ->
    write(drx_conf, #{drx_pretoc => 0}).

suspend_frame_filtering() ->
    write(sys_cfg, #{ffen => 0}).

resume_frame_filtering() ->
    write(sys_cfg, #{ffen => 1}).

read(Reg) ->
    gen_server:call(?NAME, {read, Reg}).

write(Reg, Val) ->
    gen_server:call(?NAME, {write, Reg, Val}).

disable_rx() ->
    gen_server:call(?NAME, {rx_off}).

%--- API: Getters --------------------------------------------------------------
rx_ranging_info() ->
    #{rng := RNG} = read(rx_finfo),
    RNG.

%% @doc Returns the estimated value of the signal power in dBm
%% cf. user manual section 4.7.2
signal_power() ->
    C = channel_impulse_resp_pow() , % Channel impulse resonse power value (CIR_PWR)
    A = case prf_value() of
            16 -> 113.77;
            64 -> 121.74
        end, % Constant. For PRF of 16 MHz = 113.77, for PRF of 64MHz = 121.74
    N = preamble_acc(), % Preamble accumulation count value (RXPACC but might be ajusted)
    Num = C* math:pow(2, 17),
    Dem = math:pow(N, 2),
    Log = math:log10(Num / Dem),
    10 *  Log - A.

preamble_acc() ->
    #{rxpacc := RXPACC} = read(rx_finfo),
    #{rxpacc_nosat := RXPACC_NOSAT} = read(drx_conf),
    if 
        RXPACC == RXPACC_NOSAT -> RXPACC;
        true -> RXPACC - 5
    end.

channel_impulse_resp_pow() ->
    #{cir_pwr := CIR_POW} = read(rx_fqual),
    CIR_POW.

%% @doc Gives the value of the PRF in MHz 
-spec prf_value() -> 16 | 64.
prf_value() ->
    #{agc_tune1 := AGC_TUNE1} = read(agc_ctrl),
    case AGC_TUNE1 of
        16#8870 -> 16;
        16#889B -> 64
    end.

%% @doc returns the preamble symbols repetition
rx_preamble_repetition() ->
    #{rxpsr := RXPSR} = read(rx_finfo),
    case RXPSR of
        0 -> 16;
        1 -> 64;
        2 -> 1024;
        3 -> 4096
    end.

%% @doc returns the data rate of the received frame in kbps
rx_data_rate() ->
    #{rxbr := RXBR} = read(rx_finfo),
    case RXBR of
        0 -> 110;
        1 -> 850;
        3 -> 6800
    end.

get_conf() ->
    gen_server:call(?NAME, {get_conf}).

%--- gen_server callbacks ------------------------------------------------------
init(_Params) ->
    {ok, #{regs => pmod_uwb_registers:default(),
           conf => #phy_cfg{}}}.

handle_call({transmit, Data, Options}, _From, State) -> {reply, tx(Data, Options), State};
handle_call({reception}, _From, State) -> {reply, rx(), State};
handle_call({read, Reg}, _From, #{regs := Regs} = State) -> {reply, maps:get(Reg, Regs), State};
handle_call({write, Reg, Val}, _From, #{regs := Regs} = State) -> {reply, ok, State#{regs => pmod_uwb_registers:update_reg(Regs, Reg, Val)}};
handle_call({rx_off}, _From, State) -> {reply, ok, State};
handle_call({get_conf}, _From, #{conf := Conf} = State) -> {reply, Conf, State};
handle_call(_Request, _From, _State) -> error(not_implemented).

handle_cast(_Request, _State) ->
  error(not_implemented).


% --- Internal -----------------------------------------
tx(_Data, _Options) ->
    ok.

rx() ->
    {14, <<16#6188:16, 0:8, 16#CADE:16, "XR", "XT", "Hello">>}.