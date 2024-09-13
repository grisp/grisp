-module(pmod_uwb).
-behaviour(gen_server).

% API
-export([start_link/2]).
-export([read/1, write/2, write_tx_data/1, get_received_data/0, transmit/1, transmit/2, wait_for_transmission/0, reception/0, reception/1]).
-export([reception_async/0]).
-export([set_frame_timeout/1]).
-export([set_preamble_timeout/1, disable_preamble_timeout/0]).
-export([softreset/0, clear_rx_flags/0]).
-export([disable_rx/0]).
-export([suspend_frame_filtering/0, resume_frame_filtering/0]).
-export([signal_power/0]).
-export([prf_value/0]).
-export([rx_preamble_repetition/0]).
-export([rx_data_rate/0]).
-export([rx_ranging_info/0]).
-export([std_noise/0]).
-export([first_path_power_level/0]).
-export([get_conf/0]).
-export([get_rx_metadata/0]).

% gen_server callback
-export([init/1, handle_call/3, handle_cast/2]).

-compile({nowarn_unused_function, [debug_read/2, debug_write/2, debug_write/3, debug_bitstring/1, debug_bitstring_hex/1]}).

% Includes
-include("grisp_internal.hrl").

-include("pmod_uwb.hrl").

%--- Macros --------------------------------------------------------------------

% Define the polarity and the phase of the clock
-define(SPI_MODE, #{clock => {low, leading}}).

-define(WRITE_ONLY_REG_FILE(RegFileID), RegFileID == tx_buffer).

-define(READ_ONLY_REG_FILE(RegFileID), RegFileID==dev_id; 
                                       RegFileID==sys_time; 
                                       RegFileID==rx_finfo;
                                       RegFileID==rx_buffer;
                                       RegFileID==rx_fqual;
                                       RegFileID==rx_ttcko;
                                       RegFileID==rx_time;
                                       RegFileID==tx_time;
                                       RegFileID==sys_state;
                                       RegFileID==acc_mem).

%% The congifurations of the subregisters of these register files are different
%% (some sub-registers are RO, some are RW and some have reserved bytes 
%% that can't be written)
%% Thus, some registers files require to write their sub-register independently 
%% => Write the sub-registers one by one instead of writting 
%%    the whole register file directly
-define(IS_SRW(RegFileID), RegFileID==agc_ctrl;
                           RegFileID==ext_sync;
                           RegFileID==ec_ctrl;
                           RegFileID==gpio_ctrl;
                           RegFileID==drx_conf;
                           RegFileID==rf_conf;
                           RegFileID==tx_cal;
                           RegFileID==fs_ctrl;
                           RegFileID==aon;
                           RegFileID==otp_if;
                           RegFileID==lde_if;
                           RegFileID==dig_diag;
                           RegFileID==pmsc).

-define(READ_ONLY_SUB_REG(SubRegister), SubRegister==irqs;
                                        SubRegister==agc_stat1;
                                        SubRegister==ec_rxtc;
                                        SubRegister==ec_glop;
                                        SubRegister==drx_car_int;
                                        SubRegister==rf_status;
                                        SubRegister==tc_sarl;
                                        SubRegister==sarw;
                                        SubRegister==tc_pg_status;
                                        SubRegister==lde_thresh;
                                        SubRegister==lde_ppindx;
                                        SubRegister==lde_ppampl;
                                        SubRegister==evc_phe;
                                        SubRegister==evc_rse;
                                        SubRegister==evc_fcg;
                                        SubRegister==evc_fce;
                                        SubRegister==evc_ffr;
                                        SubRegister==evc_ovr;
                                        SubRegister==evc_sto;
                                        SubRegister==evc_pto;
                                        SubRegister==evc_fwto;
                                        SubRegister==evc_txfs;
                                        SubRegister==evc_hpw;
                                        SubRegister==evc_tpw).


%--- Types ---------------------------------------------------------------------
-export_type([register_values/0]).

-type regFileID() :: atom().
-opaque register_values() :: map().

%--- API -----------------------------------------------------------------------

start_link(Connector, _Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Connector, []).


%% @doc read a register file
%%
%% === Example ===
%% To read the register file DEV_ID
%% ``` 
%% 1> pmod_uwb:read(dev_id).
%% #{model => 1,rev => 0,ridtag => "DECA",ver => 3}
%% '''
-spec read(RegFileID) -> Result when
    RegFileID :: regFileID(),
    Result    :: map() | {error, any()}.
read(RegFileID) when ?WRITE_ONLY_REG_FILE(RegFileID) ->
    error({read_on_write_only_register, RegFileID});
read(RegFileID) -> call({read, RegFileID}).

%% @doc Write values in a register
%% 
%% === Examples ===
%% To write in a simple register file (i.e. a register without any sub-register)
%% ```
%% 1> pmod_uwb:write(eui, #{eui => <<16#AAAAAABBBBBBBBBB>>}).
%% ok
%% ''' 
%% To write in one sub-register of a register file:
%% ```
%% 2> pmod_uwb:write(panadr, #{pan_id => <<16#AAAA>>}). 
%% ok
%% '''
%% The previous code will only change the values inside the sub-register PAN_ID
%%
%% To write in multiple sub-register of a register file in the same burst:
%% ```
%% 3> pmod_uwb:write(panadr, #{pan_id => <<16#AAAA>>,
%%                             short_addr => <<16#BBBB>>}).
%% ok
%% '''
%% Some sub-registers have their own fields. For example to set the value of 
%% the DIS_AM field in the sub-register AGC_CTRL1 of the register file AGC_CTRL:
%% ```
%% 4> pmod_uwb:write(agc_ctrl, #{agc_ctrl1 => #{dis_am => 2#0}}).
%% '''
-spec write(RegFileID, Value) -> Result when
    RegFileID :: regFileID(),
    Value     :: map(),
    Result    :: ok | {error, any()}.
write(RegFileID, Value) when ?READ_ONLY_REG_FILE(RegFileID) ->
    error({write_on_read_only_register, RegFileID, Value});
write(RegFileID, Value) when is_map(Value) ->
    call({write, RegFileID, Value}).

%% @doc Writes the data in the TX_BUFFER register
%%
%% Value is expected to be a <b>Binary</b>
%% That choice was made to make the transmission of frames easier later on
%%
%% === Examples ===
%% Send "Hello" in the buffer
%% ```
%% 1> pmod_uwb:write_tx_data(<<"Hello">>).
%% '''
-spec write_tx_data(Value) -> Result when 
    Value  :: binary(),
    Result :: ok | {error, any()}.
write_tx_data(Value) -> call({write_tx, Value}).

%% @doc Retrieves the data received on the UWB antenna
%% @returns {DataLength, Data}
-spec get_received_data() -> Result when
    Result :: {integer(), bitstring()} | {error, any()}.
get_received_data() -> call({get_rx_data}).

get_rx_metadata() ->
    #{rng := Rng} = read(rx_finfo),
    #{rx_stamp := RxStamp} = read(rx_time),
    #{tx_stamp := TxStamp} = read(tx_time),
    #{rxtofs := Rxtofs} = read(rx_ttcko),
    #{rxttcki := Rxttcki} = read(rx_ttcki),
    #{snr => snr(),
      prf => prf_value(),
      pre => rx_preamble_repetition(),
      data_rate => rx_data_rate(),
      rng => Rng,
      rx_stamp => RxStamp,
      tx_stamp => TxStamp,
      rxtofs => Rxtofs,
      rxttcki => Rxttcki}.

% Source: https://forum.qorvo.com/t/how-to-calculate-the-signal-to-noise-ratio-snr-of-dw1000/5585/3
snr() ->
    Delta = 87-7.5,
    RSL = pmod_uwb:signal_power(),
    RSL + Delta.

%% @doc Transmit data with the default options (i.e. don't wait for resp, ...)
%%
%% === Examples ===
%% To transmit a frame:
%% ```
%% 1> pmod_uwb:transmit(<Version:4, NextHop:8>>).
%% ok.
%% '''
-spec transmit(Data) -> Result when
    Data   :: bitstring(),
    Result :: ok.
transmit(Data) when is_bitstring(Data) ->
    call({transmit, Data, #tx_opts{}}),
    wait_for_transmission().

%% @doc Performs a transmission with the specified options
%%
%% === Options ===
%% * wait4resp: It specifies that the reception must be enabled after 
%%              the transmission in the expectation of a response
%% * w4r-tim: Specifies the turn around time in microseconds. That is the time
%%            the pmod will wait before enabling rx after a tx. 
%%            Note that it won't be set if wit4resp is disabled
%% * txdlys: Specifies if the transmitter delayed sending should be set 
%% * tx_delay: Specifies the delay of the transmission (see register DX_TIME)
%%
%% === Examples ===
%% To transmit a frame with default options:
%% ```
%% 1> pmod_uwb:transmit(<Version:4, NextHop:8>>, #tx_opts{}).
%% ok.
%% '''
-spec transmit(Data, Options) -> Result when
    Data :: bitstring(),
    Options :: tx_opts(),
    Result :: ok.
transmit(Data, Options) ->
    case Options#tx_opts.wait4resp of
        ?ENABLED -> clear_rx_flags();
        _ -> ok
    end,
    call({transmit, Data, Options}),
    case read(sys_status) of
        #{hdpwarn := 2#1} -> error({hdpwarn});
        _ -> ok
    end,
    wait_for_transmission().

%% Wait for the transmission to be performed
%% usefull in the case of a delayed transmission
wait_for_transmission() ->
    case read(sys_status) of
        #{txfrs := 1} -> ok;
        _ -> wait_for_transmission()
    end.

%% @doc Receive data using the pmod 
%% @equiv reception(false)
-spec reception() -> Result when 
    Result :: {integer(), bitstring()} | {error, any()}.
reception() ->
    reception(false).

%% @doc Receive data using the pmod 
%%
%% The function will hang until a frame is received on the board
%%
%% The CRC of the received frame <b>isn't</b> included in the returned value
%%
%% @param RXEnabled: specifies if the reception is already enabled on the board 
%%                   (or set with delay)
%%
%% === Example ===
%% ```
%% 1> pmod_uwb:reception().
%% % Some frame is transmitted
%% {11, <<"Hello world">>}.
%% '''
-spec reception(RXEnabled) -> Result when
      RXEnabled :: boolean(),
      Result    :: {integer(), bitstring()} | {error, any()}.
reception(RXEnabled) ->
    if not RXEnabled -> enable_rx();
       true -> ok
    end,
    case wait_for_reception() of
        ok ->
            get_received_data();
        Err ->
            {error, Err}
    end.

-spec reception_async() -> Result when
      Result    :: ok | {error, any()}.
reception_async() ->
    case reception() of
        {error, _} = Err -> Err;
        Frame ->
            Metadata = get_rx_metadata(),
            ieee802154_events:rx_event(Frame, Metadata)
    end.

%% @private
enable_rx() ->
    % io:format("Enabling reception~n"),
    clear_rx_flags(),
    call({write, sys_ctrl, #{rxenab => 2#1}}).

%% @doc Disables the reception on the pmod
disable_rx() ->
    call({write, sys_ctrl, #{trxoff => 2#1}}).

wait_for_reception() ->
    % io:format("Wait for resp~n"),
    case read(sys_status) of 
        #{rxrfto := 1} -> rxrfto;
        #{rxphe := 1} -> rxphe;
        #{rxfce := 1} -> rxfce;
        #{rxrfsl := 1} -> rxrfsl;
        #{rxpto := 1} -> rxpto; 
        #{rxsfdto := 1} -> rxsfdto;
        #{ldeerr := 1} -> ldeerr;
        #{affrej := 1} -> affrej;
        #{rxdfr := 0} -> wait_for_reception();
        #{rxfce := 1} -> rxfce;
        #{rxfcg := 1} -> ok;
        #{rxfcg := 0} -> wait_for_reception();
        % #{rxdfr := 1, rxfcg := 1} -> ok; % The example driver doesn't do that but the user manual says that how you should check the reception of a frame
        _ -> error({error_wait_for_reception})
    end.

%% @doc Set the frame wait timeout and enables it
%% The unit is roughtly 1us (cf. user manual)
%% If a float is given, it's decimal part is removed using trunc/1
%% @end
-spec set_frame_timeout(Timeout) -> Result when
      Timeout :: microseconds(),
      Result  :: ok.
set_frame_timeout(Timeout) when is_float(Timeout) ->
    set_frame_timeout(trunc(Timeout));
set_frame_timeout(Timeout) when is_integer(Timeout) ->
    write(rx_fwto, #{rxfwto => Timeout}),
    write(sys_cfg, #{rxwtoe => 2#1}). % enable receive wait timeout

%% @doc Sets the preamble timeout. (PRETOC register of the DW1000)
%% The unit of `Timeout' is in units usec
%% If the value is a float, trunc is called to remove the decimal part
%% Internally, it's converted in untis of PAC size
-spec set_preamble_timeout(Timeout) -> ok when
      Timeout :: non_neg_integer().
set_preamble_timeout(TO) when is_float(TO) ->
    set_preamble_timeout(trunc(TO));
set_preamble_timeout(TO) when is_integer(TO) ->
    call({preamble_timeout, TO}),
    write(drx_conf, #{drx_pretoc => 0}).

disable_preamble_timeout() ->
    write(drx_conf, #{drx_pretoc => 0}).

%% @doc Performs a reset of the IC following the procedure (cf. sec. 7.2.50.1) 
softreset() -> 
    write(pmsc, #{pmsc_ctrl0 => #{sysclks => 2#01}}),
    write(pmsc, #{pmsc_ctrl0 => #{softrest => 16#0}}),
    write(pmsc, #{pmsc_ctrl0 => #{softreset => 16#FFFF}}).


clear_rx_flags() ->
    write(sys_status, #{rxsfdto => 2#1, 
                        rxpto => 2#1,
                        rxrfto => 2#1,
                        rxrfsl => 2#1,
                        rxfce => 2#1,
                        rxphe => 2#1,
                        rxprd => 2#1,
                        rxdsfdd => 2#1,
                        rxphd => 2#1,
                        rxdfr => 2#1,
                        rxfcg => 2#1}).

suspend_frame_filtering() ->
    write(sys_cfg, #{ffen => 2#0}).

resume_frame_filtering() ->
    write(sys_cfg, #{ffen => 2#1}).

%% @doc Returns the estimated value of the signal power in dBm
%% cf. user manual section 4.7.2
signal_power() ->
    C = channel_impulse_resp_pow() , % Channel impulse resonse power value (CIR_PWR)
    A = case prf_value() of
            16 -> 113.77;
            64 -> 121.74
        end, % Constant. For PRF of 16 MHz = 113.77, for PRF of 64MHz = 121.74
    N = preamble_acc(), % Preamble accumulation count value (RXPACC but might be ajusted)
    % io:format("C: ~w~n A:~w~n N:~w~n", [C, A, N]),
    Res = 10 * math:log10((C* math:pow(2, 17))/math:pow(N, 2)) - A,
    % io:format("Estimated signal power: ~p dBm~n", [Res]),
    % io:format("Std noise: ~w~n", [pmod_uwb:read(rx_fqual)]),
    Res.

preamble_acc() ->
    #{rxpacc := RXPACC} = read(rx_finfo),
    #{rxpacc_nosat := RXPACC_NOSAT} = read(drx_conf),
    if 
        RXPACC == RXPACC_NOSAT -> RXPACC - 5;
        true -> RXPACC
    end.

channel_impulse_resp_pow() ->
    #{cir_pwr := CIR_PWR} = read(rx_fqual),
    CIR_PWR.

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
        2 -> 6800
    end.

% @doc returns the value of the `Ranging' bit of the received frame
rx_ranging_info() ->
    #{rng := RNG} = read(rx_finfo),
    RNG.

std_noise() ->
    #{std_noise := STD_NOISE} = read(rx_fqual),
    STD_NOISE.

first_path_power_level() ->
    #{fp_ampl1 := F1} = read(rx_time),
    #{fp_ampl2 := F2, pp_ampl3 := F3} = read(rx_fqual),
    A = 113.77,
    N = preamble_acc(),
    10 * math:log10((math:pow(F1,2) + math:pow(F2, 2) + math:pow(F3, 2))/math:pow(N, 2)) - A. 

get_conf() ->
    call({get_conf}).

%--- gen_server Callbacks ------------------------------------------------------

%% @private
init(Slot) ->
    % Verify the slot used
    case {grisp_hw:platform(), Slot} of
        {grisp2, spi2} -> ok;
        {P, S} -> error({incompatible_slot, P, S})
    end,
    grisp_devices:register(Slot, ?MODULE),
    Bus = grisp_spi:open(Slot),
    case verify_id(Bus) of
        ok -> softreset(Bus);
        Val -> error({dev_id_no_match, Val})
    end,
    ldeload(Bus),
    % TODO Merge the next 4 cfg commands into one
    write_default_values(Bus),
    config(Bus),
    setup_sfd(Bus),
    Conf =  #phy_cfg{},
    {ok, #{bus => Bus, conf => Conf}}.

%% @private
handle_call({read, RegFileID}, _From, #{bus := Bus} = State)               ->
    {reply, read_reg(Bus, RegFileID), State};
handle_call({write, RegFileID, Value}, _From, #{bus := Bus} = State)       ->
    {reply, write_reg(Bus, RegFileID, Value), State};
handle_call({write_tx, Value}, _From, #{bus := Bus} = State)               ->
    {reply, write_tx_data(Bus, Value), State};
handle_call({transmit, Data, Options}, _From, #{bus := Bus} = State)       ->
    {reply, tx(Bus, Data, Options), State};
handle_call({delayed_transmit, Data, Delay}, _From, #{bus := Bus} = State) ->
    {reply, delayed_tx(Bus, Data, Delay), State};
handle_call({get_rx_data}, _From, #{bus := Bus} = State)                   ->
    {reply, get_rx_data(Bus), State};
handle_call({get_conf}, _From, #{conf := Conf} = State)                    ->
    {reply, Conf, State};
handle_call({preamble_timeout, TOus}, _From, State)                        ->
    #{bus := Bus, conf := Conf} = State,
    PACSize = Conf#phy_cfg.pac_size,
    case TOus of
        0 ->
            write_reg(Bus, drx_conf, #{drx_pretoc => 0});
        _ ->
            % Remove 1 because DW1000 counter auto. adds 1 (cf. 7.2.40.9 user manual)
            To = math:ceil(TOus / PACSize)-1,
            write_reg(Bus, drx_conf, #{drx_pretoc => round(To)})
    end,
    {reply, ok, State};
handle_call(Request, _From, _State)                                        ->
    error({unknown_call, Request}).

%% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

%--- Internal ------------------------------------------------------------------

call(Call) ->
    Dev = grisp_devices:default(?MODULE),
    gen_server:call(Dev#device.pid, Call).


%% @doc Varify the dev_id register of the pmod
%% @returns ok if the value is correct, otherwise the value read
verify_id(Bus) ->
    #{ridtag := RIDTAG, model := MODEL} = read_reg(Bus, dev_id),
    case {RIDTAG, MODEL} of
        {"DECA", 1} -> ok;
        _ -> {RIDTAG, MODEL}
    end.

%% @private
%% Performs a softreset on the pmod
-spec softreset(Bus::grisp_spi:ref()) -> ok.
softreset(Bus) ->
    write_reg(Bus, pmsc, #{pmsc_ctrl0 => #{sysclks => 2#01}}),
    write_reg(Bus, pmsc, #{pmsc_ctrl0 => #{softrest => 16#0}}),
    write_reg(Bus, pmsc, #{pmsc_ctrl0 => #{softreset => 16#FFFF}}).

%% @private
%% Writes the default values described in section 2.5.5 of the user manual
-spec write_default_values(Bus::grisp_spi:ref()) -> ok.
write_default_values(Bus) ->
    write_reg(Bus, lde_if, #{lde_cfg1 => #{ntm => 16#D}, lde_cfg2 => 16#1607}),
    write_reg(Bus, agc_ctrl, #{agc_tune1 => 16#8870, agc_tune2 => 16#2502A907}),
    write_reg(Bus, drx_conf, #{drx_tune2 => 16#311A002D}),
    write_reg(Bus, tx_power, #{tx_power => 16#0E082848}),
    write_reg(Bus, rf_conf, #{rf_txctrl => 16#001E3FE3}),
    write_reg(Bus, tx_cal, #{tc_pgdelay => 16#B5}),
    write_reg(Bus, fs_ctrl, #{fs_plltune => 16#BE}).

%% @private
config(Bus) ->
    write_reg(Bus, ext_sync, #{ec_ctrl => #{pllldt => 2#1}}),
    %write_reg(Bus, pmsc, #{pmsc_ctrl1 => #{lderune => 2#0}}),
    % Now enable RX and TX leds
    write_reg(Bus, gpio_ctrl, #{gpio_mode => #{msgp2 => 2#01, msgp3 => 2#01}}),
    % Enable RXOK and SFD leds
    write_reg(Bus, gpio_ctrl, #{gpio_mode => #{msgp0 => 2#01, msgp1 => 2#01}}),
    write_reg(Bus, pmsc, #{pmsc_ctrl0 => #{gpdce => 2#1, khzclken => 2#1}}),
    write_reg(Bus, pmsc, #{pmsc_ledc => #{blnken => 2#1}}),
    write_reg(Bus, dig_diag, #{evc_ctrl => #{evc_en => 2#1}}), % enable counting event for debug purposes
    % write_reg(Bus, sys_cfg, #{rxwtoe => 2#1}),
    write_reg(Bus, tx_fctrl, #{txpsr => 2#10}). % Setting preamble symbols to 1024

%% @private
%% Load the microcode from ROM to RAM
%% It follows the steps described in section 2.5.5.10 of the DW1000 user manual
ldeload(Bus) ->
    write_reg(Bus, pmsc, #{pmsc_ctrl0 => #{sysclks => 2#01}}),
    write_reg(Bus, pmsc, #{pmsc_ctrl0 => #{otp => 2#1, res8 => 2#1}}), % Writes 0x0301 in pmsc_ctrl0
    write_reg(Bus, otp_if, #{otp_ctrl => #{ldeload => 2#1}}), % Writes 0x8000 in OTP_CTRL
    timer:sleep(150), % User manual requires a wait of 150µs
    write_reg(Bus, pmsc, #{pmsc_ctrl0 => #{sysclks => 2#0}}), % Writes 0x0200 in pmsc_ctrl0
    write_reg(Bus, pmsc, #{pmsc_ctrl0 => #{res8 => 2#0}}). 

%% @private
%% If no frame is transmitted before AUTOACK, then the SFD isn't properly set
%% (cf. section 5.3.1.2 SFD initialisation)
setup_sfd(Bus) ->
    write_reg(Bus, sys_ctrl, #{txstrt => 2#1, trxoff => 2#1}).

%% @private
%% Transmit the data using UWB
%% @param Options is used to set options about the transmission like a transmission delay, etc.
-spec tx(grisp_spi:ref(), Data :: binary(), Options :: #tx_opts{}) -> ok.
tx(Bus, Data, #tx_opts{wait4resp = Wait4resp, w4r_tim = W4rTim, txdlys = TxDlys, tx_delay = TxDelay, ranging = Ranging}) -> 
    % Writing the data that will be sent (w/o CRC)
    DataLength = byte_size(Data) + 2, % DW1000 automatically adds the 2 bytes CRC 
    write_tx_data(Bus, Data),
    % Setting the options of the transmission
    case Wait4resp of
        ?ENABLED -> write_reg(Bus, ack_resp_t, #{w4r_tim => W4rTim});
        _ -> ok
    end,
    case TxDlys of
        ?ENABLED -> write_reg(Bus, dx_time, #{dx_time => TxDelay});
        _ -> ok
    end,
    write_reg(Bus, tx_fctrl, #{txboffs => 2#0, tr => Ranging, tflen => DataLength}),
    write_reg(Bus, sys_ctrl, #{txstrt => 2#1, wait4resp => Wait4resp, txdlys => TxDlys}). % start transmission and some options

%% @private
%% Transmit the data with a specified delay using UWB
delayed_tx(Bus, Data, Delay) ->
    write_reg(Bus, dx_time, #{dx_time => Delay}),
    DataLength = byte_size(Data) + 2, % DW1000 automatically adds the 2 bytes CRC 
    write_tx_data(Bus, Data),
    write_reg(Bus, tx_fctrl, #{txboffs => 2#0, tflen => DataLength}),
    write_reg(Bus, sys_ctrl, #{txstrt => 2#1, txdlys => 2#1}). % start transmission

%% @private
%% Get the received data (without the CRC bytes) stored in the rx_buffer
get_rx_data(Bus) ->
    #{rxflen := FrameLength} = read_reg(Bus, rx_finfo),
    Frame = read_rx_data(Bus, FrameLength-2), % Remove the CRC bytes
    {FrameLength, Frame}.

%% @private
%% @doc Reverse the byte order of the bitstring given in the argument 
%% @param Bin a bitstring
reverse(Bin) -> reverse(Bin, <<>>).
reverse(<<Bin:8>>, Acc) -> 
    <<Bin, Acc/binary>>;
reverse(<<Bin:8, Rest/bitstring>>, Acc) -> 
    reverse(Rest, <<Bin, Acc/binary>>).

% Source: https://stackoverflow.com/a/43310493
% reverse(Binary) ->
%     Size = bit_size(Binary),
%     <<X:Size/integer-little>> = Binary,
%     <<X:Size/integer-big>>.

%% @private
%% @doc Creates the header of the SPI transaction between the GRiSP and the pmod
%%
%%  It creates a header of 1 bytes. The header is used in a transaction that will affect
%%  the whole register file (read/write)
%%
%% @param Op an atom (either <i>read</i> or <i>write</i>
%% @param RegFileID an atom representing the register file
%% @returns a formated header of <b>1 byte</b> long as described in the user manual
header(Op, RegFileID) ->
    <<(rw(Op)):1, 2#0:1, (regFile(RegFileID)):6>>.

%% @private
%% @doc Creates the header of the SPI transaction between the GRiSP and the pmod
%%
%%  It creates a header of 2 bytes. The header is used in a transaction that will affect
%%  the whole sub-register (read/write)
%%  Careful: The sub-register needs to be mapped in the hrl file
%%
%% @param Op an atom (either <i>read</i> or <i>write</i>
%% @param RegFileID an atom representing the register file
%% @param SubRegister an atom representing the sub-register
%% @returns a formated header of <b>2 byte</b> long as described in the user manual
header(Op, RegFileID, SubRegister) -> 
    case subReg(SubRegister) < 127 of
        true -> header(Op, RegFileID, SubRegister, 2);
        _ -> header(Op, RegFileID, SubRegister, 3)
    end.

header(Op, RegFileID, SubRegister, 2) ->
    << (rw(Op)):1, 2#1:1, (regFile(RegFileID)):6,
        2#0:1, (subReg(SubRegister)):7 >>;
header(Op, RegFileID, SubRegister, 3) ->
    <<_:1, HighOrder:8, LowOrder:7>> = <<(subReg(SubRegister)):16>>,
    << (rw(Op)):1, 2#1:1, (regFile(RegFileID)):6,
       2#1:1, LowOrder:7,
       HighOrder:8>>.

%% @private
%% @doc Read the values stored in a register file
read_reg(Bus, lde_ctrl) -> read_reg(Bus, lde_if);
read_reg(Bus, lde_if) ->
    lists:foldl(fun(Elem, Acc) ->
                    Res = read_sub_reg(Bus, lde_if, Elem),                
                    maps:merge(Acc, Res)
                end, 
                #{}, 
                [lde_thresh, lde_cfg1, lde_ppindx, lde_ppampl, lde_rxantd, lde_cfg2, lde_repc]);    
read_reg(Bus, RegFileID) ->
    Header = header(read, RegFileID),
    [Resp] = grisp_spi:transfer(Bus, [{?SPI_MODE, Header, 1, regSize(RegFileID)}]),
    % debug_read(RegFileID, Resp),
    reg(decode, RegFileID, Resp).


read_sub_reg(Bus, RegFileID, SubRegister) ->
    Header = header(read, RegFileID, SubRegister),
    HeaderSize = byte_size(Header),
    % io:format("[HEADER] type ~w - ~w - ~w~n", [HeaderSize, Header, subRegSize(SubRegister)]),
    [Resp] = grisp_spi:transfer(Bus, [{?SPI_MODE, Header, HeaderSize, subRegSize(SubRegister)}]),
    reg(decode, SubRegister, Resp).


%% @doc get the received data 
%% @param Length is the total length of the data we are trying to read
read_rx_data(Bus, Length) ->
    Header = header(read, rx_buffer),
    [Resp] = grisp_spi:transfer(Bus, [{?SPI_MODE, Header, 1, Length}]),
    Resp.

% TODO: check that user isn't trying to write reserved bits by passing res, res1, ... in the map fields
%% @doc used to write the values in the map given in the Value argument
-spec write_reg(Bus::grisp_spi:ref(), RegFileID::regFileID(), Value::map()) -> ok.
% Write each sub-register one by one.
% If the user tries to write in a read-only sub-register, an error is thrown 
write_reg(Bus, RegFileID, Value) when ?IS_SRW(RegFileID) ->
    maps:map(
        fun(SubRegister, Val) ->
            CurrVal = maps:get(SubRegister, read_reg(Bus, RegFileID)), % ? can the read be done before ? Maybe but not assured that no values changes after a write in the register
            Body = case CurrVal of
                        V when is_map(V) -> reg(encode, SubRegister, maps:merge_with(fun(_Key, _Old, New) -> New end, CurrVal, Val));
                        _ -> reg(encode, SubRegister, #{SubRegister => Val})
                   end,
            Header = header(write, RegFileID, SubRegister),
            % debug_write(RegFileID, SubRegister, Body),
            _ = grisp_spi:transfer(Bus, [{?SPI_MODE, <<Header/binary, Body/binary>>, 2+subRegSize(SubRegister), 0}])
        end,
        Value),
    ok;
write_reg(Bus, RegFileID, Value) ->
    Header = header(write, RegFileID),
    CurrVal = read_reg(Bus, RegFileID),
    ValuesToWrite = maps:merge_with(fun(_Key, _Value1, Value2) -> Value2 end, CurrVal, Value),
    Body = reg(encode, RegFileID, ValuesToWrite),
    % debug_write(RegFileID, Body),
    _ = grisp_spi:transfer(Bus, [{?SPI_MODE, <<Header/binary, Body/binary>>, 1+regSize(RegFileID), 0}]),
    ok.

%% @doc write_tx_data/2 sends data (Value) in the register tx_buffer
%% @param Value is the data to be written. It must be a binary and have a size of maximum 1024 bits
write_tx_data(Bus, Value) when is_binary(Value), (bit_size(Value) < 1025) ->
    Header = header(write, tx_buffer),
    Length = byte_size(Value),
    % debug_write(tx_buffer, Body),
    _ = grisp_spi:transfer(Bus, [{?SPI_MODE, <<Header/binary, Value/binary>>, 1+Length, 0}]),
    ok.

%---- Register mapping --------------------------------------------------------

%% @doc Used to either decode the data returned by the pmod or to encode to data that will be sent to the pmod
%% 
%% The transmission on the MISO line is done byte by byte starting from the lowest rank byte to the highest rank
%% Example: dev_id value is 0xDECA0130 but 0x3001CADE is transmitted over the MISO line
-spec reg(Type, Register, Val) -> Ret when
      Type     :: encode | decode,
      Register :: regFileID(),
      Val      :: nonempty_binary() | register_values(),
      Ret      :: nonempty_binary() | register_values().
reg(encode, SubRegister, Value) when ?READ_ONLY_SUB_REG(SubRegister) -> error({writing_read_only_sub_register, SubRegister, Value});
reg(decode, dev_id, Resp) -> 
    <<
      RIDTAG:16, Model:8, Ver:4, Rev:4
    >> = reverse(Resp),
    #{
        ridtag => integer_to_list(RIDTAG, 16), model => Model, ver => Ver, rev => Rev
    };
reg(decode, eui, Resp) ->
    #{
        eui => reverse(Resp)
    };
reg(encode, eui, Val) ->
    #{
        eui:= EUI
    } = Val,
    reverse(
        EUI
    );
reg(decode, panadr, Resp) ->
    <<
        PanId:16, ShortAddr:16
    >> = reverse(Resp),
    #{
        pan_id => <<PanId:16>>, short_addr => <<ShortAddr:16>>
    };
reg(encode, panadr, Val) ->
    #{
        pan_id := PanId, short_addr := ShortAddr
    } = Val,
    reverse(<<
        PanId:16/bitstring, ShortAddr:16/bitstring
    >>);
reg(decode, sys_cfg, Resp) ->
    << 
        FFA4:1, FFAR:1, FFAM:1, FFAA:1, FFAD:1, FFAB:1, FFBC:1, FFEN:1, % bits 7-0
        FCS_INIT2F:1, DIS_RSDE:1, DIS_PHE:1, DIS_DRXB:1, DIS_FCE:1, SPI_EDGE:1, HIRQ_POL:1, FFA5:1, % bits 15-8
        _:1, RXM110K:1, _:3, DIS_STXP:1, PHR_MODE:2, % bits 23-16
        AACKPEND:1, AUTOACK:1, RXAUTR:1, RXWTOE:1, _:4 % bits 31-24
    >> = Resp,
    #{
        aackpend => AACKPEND, autoack => AUTOACK, rxautr => RXAUTR, rxwtoe => RXWTOE, 
        rxm110k => RXM110K, dis_stxp => DIS_STXP, phr_mode => PHR_MODE, 
        fcs_init2F => FCS_INIT2F, dis_rsde => DIS_RSDE, dis_phe => DIS_PHE, dis_drxb => DIS_DRXB, dis_fce => DIS_FCE, spi_edge => SPI_EDGE, hirq_pol => HIRQ_POL, ffa5 => FFA5,
        ffa4 => FFA4, ffar => FFAR, ffam => FFAM, ffaa => FFAA, ffad => FFAD, ffab => FFAB, ffbc => FFBC, ffen => FFEN
    };
reg(encode, sys_cfg, Val) ->
    #{
        aackpend := AACKPEND, autoack := AUTOACK, rxautr := RXAUTR, rxwtoe := RXWTOE, 
        rxm110k := RXM110K, dis_stxp := DIS_STXP, phr_mode := PHR_MODE, 
        fcs_init2F := FCS_INIT2F, dis_rsde := DIS_RSDE, dis_phe := DIS_PHE, dis_drxb := DIS_DRXB, dis_fce := DIS_FCE, spi_edge := SPI_EDGE, hirq_pol := HIRQ_POL, ffa5 := FFA5,
        ffa4 := FFA4, ffar := FFAR, ffam := FFAM, ffaa := FFAA, ffad := FFAD, ffab := FFAB, ffbc := FFBC, ffen := FFEN
    } = Val,
    <<
        FFA4:1, FFAR:1, FFAM:1, FFAA:1, FFAD:1, FFAB:1, FFBC:1, FFEN:1, % bits 7-0
        FCS_INIT2F:1, DIS_RSDE:1, DIS_PHE:1, DIS_DRXB:1, DIS_FCE:1, SPI_EDGE:1, HIRQ_POL:1, FFA5:1, % bits 15-8
        2#0:1, RXM110K:1, 2#0:3, DIS_STXP:1, PHR_MODE:2, % bits 23-16
        AACKPEND:1, AUTOACK:1, RXAUTR:1, RXWTOE:1, 2#0:4 % bits 31-24
    >>;
reg(decode, sys_time, Resp) -> 
    <<
        SysTime:40
    >> = reverse(Resp),
    #{
        sys_time => SysTime
    };
reg(decode, tx_fctrl, Resp) -> 
    <<
        IFSDELAY:8, TXBOFFS:10, PE:2, TXPSR:2, TXPRF:2, TR:1, TXBR:2, R:3, TFLE:3, TFLEN:7
    >> = reverse(Resp),
    #{
        ifsdelay => IFSDELAY, txboffs => TXBOFFS, pe => PE, txpsr => TXPSR, txprf => TXPRF, tr => TR, txbr => TXBR, r => R, tfle => TFLE, tflen => TFLEN
    };
reg(encode, tx_fctrl, Val) ->
    #{
        ifsdelay := IFSDELAY, txboffs := TXBOFFS, pe := PE, txpsr := TXPSR, txprf := TXPRF, tr := TR, txbr := TXBR, r := R, tfle := TFLE, tflen := TFLEN
    } = Val,
    reverse(<<
        IFSDELAY:8, TXBOFFS:10, PE:2, TXPSR:2, TXPRF:2, TR:1, TXBR:2, R:3, TFLE:3, TFLEN:7
    >>);
% TX_BUFFER is write only => no decode
reg(decode, dx_time, Resp) ->
    #{
        dx_time => reverse(Resp)
    };
reg(encode, dx_time, Val) ->
    #{
        dx_time := DX_TIME
    } = Val,
    reverse(<<
        DX_TIME:40
    >>);
reg(decode, rx_fwto, Resp) ->
    <<
        RXFWTO:16
    >> = reverse(Resp),
    #{
        rxfwto => RXFWTO
    };
reg(encode, rx_fwto, Val) ->
    #{
        rxfwto := RXFWTO
    } = Val,
    reverse(<<
        RXFWTO:16
    >>);
reg(decode, sys_ctrl, Resp) ->
    <<
        WAIT4RESP:1, TRXOFF:1, _:2, CANSFCS:1, TXDLYS:1, TXSTRT:1, SFCST:1, % bits 7-0
        _:6, RXDLYE:1, RXENAB:1, % bits 15-8
        _:8, % bits 23-16
        _:7, HRBPT:1 % bits 31-24
    >> = Resp,
    #{
        sfcst => SFCST, txstrt => TXSTRT, txdlys => TXDLYS, cansfcs => CANSFCS, trxoff => TRXOFF, wait4resp => WAIT4RESP, 
        rxenab => RXENAB, rxdlye => RXDLYE, 
        hrbpt => HRBPT
    };
reg(encode, sys_ctrl, Val) ->
    #{
        sfcst := SFCST, txstrt := TXSTRT, txdlys := TXDLYS, cansfcs := CANSFCS, trxoff := TRXOFF, wait4resp := WAIT4RESP, 
        rxenab := RXENAB, rxdlye := RXDLYE, 
        hrbpt := HRBPT
    } = Val,
    <<
        WAIT4RESP:1, TRXOFF:1, 2#0:2, CANSFCS:1, TXDLYS:1, TXSTRT:1, SFCST:1, % bits 7-0
        2#0:6, RXDLYE:1, RXENAB:1, % bits 15-8
        2#0:8, % bits 23-16
        2#0:7, HRBPT:1 % bits 31-24
    >>;
reg(decode, sys_mask, Resp) ->
    <<
        MTXFRS:1, MTXPHS:1, MTXPRS:1, MTXFRB:1, MAAT:1, MESYNCR:1, MCPLOCK:1, Reserved0:1, % bits 7-0
        MRXFCE:1, MRXFCG:1, MRXDFR:1, MRXPHE:1, MRXPHD:1, MLDEDON:1, MRXSFDD:1, MRXPRD:1, % bits 15-8
        MSLP2INIT:1, MGPIOIRQ:1, MRXPTO:1, MRXOVRR:1, Reserved1:1, MLDEERR:1, MRXRFTO:1, MRXRFSL:1, % bits 23-16
        Reserved2:2, MAFFREJ:1, MTXBERR:1, MHPDDWAR:1, MPLLHILO:1, MCPLLLL:1, MRFPLLLL:1 % bits 31-24
    >> = Resp,
    #{
        mtxfrs => MTXFRS, mtxphs => MTXPHS, mtxprs => MTXPRS, mtxfrb => MTXFRB, maat => MAAT, mesyncr => MESYNCR, mcplock => MCPLOCK, res0 => Reserved0, % bits 7-0
        mrxfce => MRXFCE, mrxfcg => MRXFCG, mrxdfr => MRXDFR, mrxphe => MRXPHE, mrxphd => MRXPHD, mldeon => MLDEDON, mrxsfdd => MRXSFDD, mrxprd => MRXPRD, % bits 15-8
        mslp2init => MSLP2INIT, mgpioirq => MGPIOIRQ, mrxpto => MRXPTO, mrxovrr => MRXOVRR, res1 => Reserved1, mldeerr => MLDEERR, mrxrfto => MRXRFTO, mrxrfsl => MRXRFSL, % bits 23-16
        res2 => Reserved2, maffrej => MAFFREJ, mtxberr => MTXBERR, mhpddwar => MHPDDWAR, mpllhilo => MPLLHILO, mcpllll => MCPLLLL, mrfpllll => MRFPLLLL % bits 31-24
    };
reg(encode, sys_mask, Val) ->
    #{
        mtxfrs := MTXFRS, mtxphs := MTXPHS, mtxprs := MTXPRS, mtxfrb := MTXFRB, maat := MAAT, mesyncr := MESYNCR, mcplock := MCPLOCK, res0 := Reserved0, % bits 7-0
        mrxfce := MRXFCE, mrxfcg := MRXFCG, mrxdfr := MRXDFR, mrxphe := MRXPHE, mrxphd := MRXPHD, mldeon := MLDEDON, mrxsfdd := MRXSFDD, mrxprd := MRXPRD, % bits 15-8
        mslp2init := MSLP2INIT, mgpioirq := MGPIOIRQ, mrxpto := MRXPTO, mrxovrr := MRXOVRR, res1 := Reserved1, mldeerr := MLDEERR, mrxrfto := MRXRFTO, mrxrfsl := MRXRFSL, % bits 23-16
        res2 := Reserved2, maffrej := MAFFREJ, mtxberr := MTXBERR, mhpddwar := MHPDDWAR, mpllhilo := MPLLHILO, mcpllll := MCPLLLL, mrfpllll := MRFPLLLL % bits 31-24
    } = Val,
    <<
        MTXFRS:1, MTXPHS:1, MTXPRS:1, MTXFRB:1, MAAT:1, MESYNCR:1, MCPLOCK:1, Reserved0:1, % bits 7-0
        MRXFCE:1, MRXFCG:1, MRXDFR:1, MRXPHE:1, MRXPHD:1, MLDEDON:1, MRXSFDD:1, MRXPRD:1, % bits 15-8
        MSLP2INIT:1, MGPIOIRQ:1, MRXPTO:1, MRXOVRR:1, Reserved1:1, MLDEERR:1, MRXRFTO:1, MRXRFSL:1, % bits 23-16
        Reserved2:2, MAFFREJ:1, MTXBERR:1, MHPDDWAR:1, MPLLHILO:1, MCPLLLL:1, MRFPLLLL:1 % bits 31-24
    >>;
reg(decode, sys_status, Resp) ->
    <<
        TXFRS:1, TXPHS:1, TXPRS:1, TXFRB:1, AAT:1, ESYNCR:1, CPLOCK:1, IRQS:1, % bits 7-0
        RXFCE:1, RXFCG:1, RXDFR:1, RXPHE:1, RXPHD:1, LDEDONE:1, RXSFDD:1, RXPRD:1, % bits 15-8
        SPL2INIT:1, GPIOIRQ:1, RXPTO:1, RXOVRR:1, Reserved0:1, LDEERR:1, RXRFTO:1, RXRFSL:1, % bits 23-16
        ICRBP:1, HSRBP:1, AFFREJ:1, TXBERR:1, HPDWARN:1, RXSFDTO:1, CLCKPLL_LL:1, RFPLL_LL:1, % bits 31-24
        Reserved1:5, TXPUTE:1, RXPREJ:1, RXRSCS:1 % bits 39-32
    >> = Resp,
    #{
        txfrs => TXFRS, txphs => TXPHS, txprs => TXPRS, txfrb => TXFRB, aat => AAT, esyncr => ESYNCR, cplock => CPLOCK, irqs => IRQS, % bits 7-0
        rxfce => RXFCE, rxfcg => RXFCG, rxdfr => RXDFR, rxphe => RXPHE, rxphd => RXPHD, ldedone => LDEDONE, rxsfdd => RXSFDD, rxprd => RXPRD, % bits 15-8
        splt2init => SPL2INIT, gpioirq => GPIOIRQ, rxpto => RXPTO, rxovrr => RXOVRR, res0 => Reserved0, ldeerr => LDEERR, rxrfto => RXRFTO, rxrfsl => RXRFSL, % bits 23-16
        icrbp => ICRBP, hsrbp => HSRBP, affrej => AFFREJ, txberr => TXBERR, hdpwarn => HPDWARN, rxsfdto => RXSFDTO, clkpll_ll => CLCKPLL_LL, rfpll_ll => RFPLL_LL, % bits 31-24
        res1 => Reserved1, txpute => TXPUTE, rxprej => RXPREJ, rxrscs => RXRSCS
    };
reg(encode, sys_status, Val) ->
    #{
        txfrs := TXFRS, txphs := TXPHS, txprs := TXPRS, txfrb := TXFRB, aat := AAT, esyncr := ESYNCR, cplock := CPLOCK, irqs := IRQS, % bits 7-0
        rxfce := RXFCE, rxfcg := RXFCG, rxdfr := RXDFR, rxphe := RXPHE, rxphd := RXPHD, ldedone := LDEDONE, rxsfdd := RXSFDD, rxprd := RXPRD, % bits 15-8
        splt2init := SPL2INIT, gpioirq := GPIOIRQ, rxpto := RXPTO, rxovrr := RXOVRR, res0 := Reserved0, ldeerr := LDEERR, rxrfto := RXRFTO, rxrfsl := RXRFSL, % bits 23-16
        icrbp := ICRBP, hsrbp := HSRBP, affrej := AFFREJ, txberr := TXBERR, hdpwarn := HPDWARN, rxsfdto := RXSFDTO, clkpll_ll := CLCKPLL_LL, rfpll_ll := RFPLL_LL, % bits 31-24
        res1 := Reserved1, txpute := TXPUTE, rxprej := RXPREJ, rxrscs := RXRSCS
    } = Val,
    <<
        TXFRS:1, TXPHS:1, TXPRS:1, TXFRB:1, AAT:1, ESYNCR:1, CPLOCK:1, IRQS:1, % bits 7-0
        RXFCE:1, RXFCG:1, RXDFR:1, RXPHE:1, RXPHD:1, LDEDONE:1, RXSFDD:1, RXPRD:1, % bits 15-8
        SPL2INIT:1, GPIOIRQ:1, RXPTO:1, RXOVRR:1, Reserved0:1, LDEERR:1, RXRFTO:1, RXRFSL:1, % bits 23-16
        ICRBP:1, HSRBP:1, AFFREJ:1, TXBERR:1, HPDWARN:1, RXSFDTO:1, CLCKPLL_LL:1, RFPLL_LL:1, % bits 31-24
        Reserved1:5, TXPUTE:1, RXPREJ:1, RXRSCS:1 % bits 39-32
    >>;
reg(decode, rx_finfo, Resp) ->
    <<
        RXPACC:12, RXPSR:2, RXPRFR:2, RNG:1, RXBR:2, RXNSPL:2, _:1, RXFLE:3, RXFLEN:7
    >> = reverse(Resp),
    #{
        rxpacc => RXPACC, rxpsr => RXPSR, rxprfr => RXPRFR, rng => RNG, rxbr => RXBR, rxnspl => RXNSPL, rxfle => RXFLE, rxflen => RXFLEN
    };
reg(decode, rx_buffer, Resp) ->
    #{ rx_buffer => reverse(Resp)};
reg(decode, rx_fqual, Resp) -> 
    <<
        CIR_PWR:16, PP_AMPL3:16, FP_AMPL2:16, STD_NOISE:16
    >> = Resp,
    #{
        cir_pwr => CIR_PWR, pp_ampl3 => PP_AMPL3, fp_ampl2 => FP_AMPL2, std_noise => STD_NOISE
    };
reg(decode, rx_ttcki, Resp) ->
    <<
      RXTTCKI:32
    >> = reverse(Resp),
    #{
        rxttcki => RXTTCKI 
    };
reg(decode, rx_ttcko, Resp) ->
    <<
        _:1, RCPHASE:7, RSMPDEL:8, _:5, RXTOFS:19 
    >> = reverse(Resp),
    #{
        rcphase => RCPHASE, rsmpdel => RSMPDEL, rxtofs => RXTOFS
    };
reg(decode, rx_time, Resp) ->
    <<
        RX_RAWST:40, FP_AMPL1:16, FP_INDEX:16, RX_STAMP:40
    >> = reverse(Resp),
    #{ 
        rx_rawst => RX_RAWST, fp_ampl1 => FP_AMPL1, fp_index => FP_INDEX, rx_stamp => RX_STAMP
    };
reg(decode, tx_time, Resp) -> 
    <<
        TX_RAWST:40, TX_STAMP:40
    >> = reverse(Resp),
    #{
        tx_rawst => TX_RAWST, tx_stamp => TX_STAMP
    };
reg(decode, tx_antd, Resp) ->
    #{
        tx_antd => reverse(Resp)
    };
reg(encode, tx_antd, Val) ->
    #{
        tx_antd := TX_ANTD
    } = Val,
    reverse(<<
        TX_ANTD:16
    >>);
reg(decode, sys_state, Resp) ->
    <<
        _:8, _:4,  PMSC_STATE:4, _:3, RX_STATE:5, _:4, TX_STATE:4
    >> = reverse(Resp),
    #{
        pmsc_state => PMSC_STATE, rx_state => RX_STATE, tx_state => TX_STATE
    };
reg(decode, ack_resp_t, Resp) ->
    <<
        ACK_TIME:8, _:4, W4R_TIME:20
    >> = reverse(Resp),
    #{
        ack_tim => ACK_TIME, w4r_tim => W4R_TIME
    };
reg(encode, ack_resp_t, Val) ->
    #{
        ack_tim := ACK_TIME, w4r_tim := W4R_TIME
    } = Val,
    reverse(<<
        ACK_TIME:8, 2#0:4, W4R_TIME:20
    >>);
reg(decode, rx_sniff, Resp) ->
    <<
        Reserved0:16, SNIFF_OFFT:8, Reserved1:4, SNIFF_ONT:4
    >> = reverse(Resp),
    #{
        res0 => Reserved0,
        sniff_offt => SNIFF_OFFT,
        sniff_ont => SNIFF_ONT,
        res1 => Reserved1
    };
reg(encode, rx_sniff, Val) ->
    #{
        res0 := Reserved0,
        sniff_offt := SNIFF_OFFT,
        sniff_ont := SNIFF_ONT,
        res1 := Reserved1
    } = Val,
    reverse(<<
        Reserved0:16, SNIFF_OFFT:8, Reserved1:4, SNIFF_ONT:4
    >>);
% Smart transmit power control (cf. user manual p 104)
reg(decode, tx_power, Resp) ->
    <<
        BOOSTP125:8, BOOSTP250:8, BOOSTP500:8, BOOSTNORM:8
    >> = reverse(Resp),
    #{
        boostp125 => BOOSTP125, boostp250 => BOOSTP250, boostp500 => BOOSTP500, boostnorm => BOOSTNORM
    };
reg(encode, tx_power, Val) ->
    % Leave the possibility to the user to write the value as one
    case Val of
        #{ tx_power := ValToEncode } -> reverse(<<ValToEncode:32>>);
        #{ boostp125 := BOOSTP125, boostp250 := BOOSTP250, boostp500 := BOOSTP500, boostnorm := BOOSTNORM } ->reverse(<<BOOSTP125:8, BOOSTP250:8, BOOSTP500:8, BOOSTNORM:8>>)
    end;
reg(decode, chan_ctrl, Resp) ->
    <<
        RX_PCODE:5, TX_PCODE:5, RNSSFD:1, TNSSFD:1, RXPRF:2, DWSFD:1, Reserved0:9, RX_CHAN:4, TX_CHAN:4 
    >> = reverse(Resp),
    #{
        rx_pcode => RX_PCODE, tx_pcode => TX_PCODE, rnssfd => RNSSFD, tnssfd => TNSSFD, rxprf => RXPRF, dwsfd => DWSFD, res0 => Reserved0, rx_chan => RX_CHAN, tx_chan => TX_CHAN
    };
reg(encode, chan_ctrl, Val) ->
    #{
        rx_pcode := RX_PCODE, tx_pcode := TX_PCODE, rnssfd := RNSSFD, tnssfd := TNSSFD, rxprf := RXPRF, dwsfd := DWSFD, res0 := Reserved0, rx_chan := RX_CHAN, tx_chan := TX_CHAN
    } = Val,
    reverse(<<
        RX_PCODE:5, TX_PCODE:5, RNSSFD:1, TNSSFD:1, RXPRF:2, DWSFD:1, Reserved0:9, RX_CHAN:4, TX_CHAN:4 
    >>);
reg(encode, usr_sfd, Value) ->
    #{
      usr_sfd := USR_SFD
     } = Value,
    reverse(<<
        USR_SFD:(8*41)
    >>);
reg(decode, usr_sfd, Resp) ->
    <<
        USR_SFD:(8*41)
    >> = reverse(Resp),
    #{
      usr_sfd => USR_SFD
     };
% AGC_CTRL is a complex register with reserved bits that can't be written
reg(encode, agc_ctrl1, Val) ->
    #{
        res := Reserved, dis_am := DIS_AM
    } = Val,
    reverse(<<
        Reserved:15, DIS_AM:1
    >>);
reg(encode, agc_tune1, Val) -> 
    #{
      agc_tune1 := AGC_TUNE1
     } = Val,
    reverse(<<
        AGC_TUNE1:16
    >>);
reg(encode, agc_tune2, Val) ->
    #{
        agc_tune2 := AGC_TUNE2
     } = Val,
    reverse(<<
        AGC_TUNE2:32
    >>);
reg(encode, agc_tune3, Val) ->
    #{
        agc_tune3 := AGC_TUNE3
     } = Val,
    reverse(<<
        AGC_TUNE3:16
    >>);
reg(decode, agc_ctrl, Resp) ->
    <<
        _:4, EDV2:9, EDG1:5, _:6, % AGC_STAT1 (RP => don't save reserved bits)
        _:80, % Reserved 4
        AGC_TUNE3:16, % AGC_TUNE3
        _:16, % Reserved 3
        AGC_TUNE2:32, % AGC_TUNE2
        _:48, % Reserved 2
        AGC_TUNE1:16, % AGC_TUNE1
        Reserved0:15, DIS_AM:1, % AGC_CTRL1 (RW => save reserved bits)
        _:16 % Reserved 1
    >> = reverse(Resp),
    #{
        agc_ctrl1 => #{res => Reserved0, dis_am => DIS_AM}, 
        agc_tune1 => AGC_TUNE1, 
        agc_tune2 => AGC_TUNE2, 
        agc_tune3 => AGC_TUNE3, 
        agc_stat1 => #{edv2 => EDV2, edg1 => EDG1}
    };
reg(encode, ec_ctrl, Val) ->
    #{
       res := Reserved, ostrm := OSTRM, wait := WAIT, pllldt := PLLLDT, osrsm := OSRSM, ostsm := OSTSM  
     } = Val,
    reverse(<<
        Reserved:20, OSTRM:1, WAIT:8, PLLLDT:1, OSRSM:1, OSTSM:1 % EC_CTRL
    >>);
reg(decode, ext_sync, Resp) ->
    <<
        _:26, OFFSET_EXT:6, % EC_GLOP        
        RX_TS_EST:32, % EC_RXTC
        Reserved:20, OSTRM:1, WAIT:8, PLLLDT:1, OSRSM:1, OSTSM:1 % EC_CTRL
    >> = reverse(Resp),
    #{
        ec_ctrl => #{res => Reserved, ostrm => OSTRM, wait => WAIT, pllldt => PLLLDT, osrsm => OSRSM, ostsm => OSTSM},
        rx_ts_est => RX_TS_EST,
        ec_golp => #{offset_ext => OFFSET_EXT}
    };
% "The host system doesn't need to access the ACC_MEM in normal operation, however it may be of interest [...] for diagnostic purpose" (from DW1000 user manual)
reg(decode, acc_mem, Resp) -> 
    #{
        acc_mem => reverse(Resp)
    };
reg(encode, gpio_mode, Val) ->
    #{
      msgp8 := MSGP8, msgp7 := MSGP7, msgp6 := MSGP6, msgp5 := MSGP5, msgp4 := MSGP4, msgp3 := MSGP3, msgp2 := MSGP2, msgp1 := MSGP1, msgp0 := MSGP0
     } = Val,
    reverse(<<
        2#0:8, MSGP8:2, MSGP7:2, MSGP6:2, MSGP5:2, MSGP4:2, MSGP3:2, MSGP2:2, MSGP1:2, MSGP0:2, 2#0:6 % GPIO_MODE
    >>);
reg(encode, gpio_dir, Val) ->
    #{
        gdm8 := GDM8, gdm7 := GDM7, gdm6 := GDM6, gdm5 := GDM5, gdm4 := GDM4, gdm3 := GDM3, gdm2 := GDM2, gdm1 := GDM1, gdm0 := GDM0,
        gdp8 := GDP8, gdp7 := GDP7, gdp6 := GDP6, gdp5 := GDP5, gdp4 := GDP4, gdp3 := GDP3, gdp2 := GDP2, gdp1 := GDP1, gdp0 := GDP0
     } = Val,
    reverse(<<
        2#0:11, GDM8:1, 2#0:3, GDP8:1, GDM7:1, GDM6:1, GDM5:1, GDM4:1, GDP7:1, GDP6:1, GDP5:1, GDP4:1, GDM3:1, GDM2:1, GDM1:1, GDM0:1, GDP3:1, GDP2:1, GDP1:1, GDP0:1 % GPIO2_DIR
    >>);
reg(encode, gpio_dout, Val) ->
    #{
        gom8 := GOM8, gom7 := GOM7, gom6 := GOM6, gom5 := GOM5, gom4 := GOM4, gom3 := GOM3, gom2 := GOM2, gom1 := GOM1, gom0 := GOM0,
        gop8 := GOP8, gop7 := GOP7, gop6 := GOP6, gop5 := GOP5, gop4 := GOP4, gop3 := GOP3, gop2 := GOP2, gop1 := GOP1, gop0 := GOP0
     } = Val,
    reverse(<<
        2#0:11, GOM8:1, 2#0:3, GOP8:1, GOM7:1, GOM6:1, GOM5:1, GOM4:1, GOP7:1, GOP6:1, GOP5:1, GOP4:1, GOM3:1, GOM2:1, GOM1:1, GOM0:1, GOP3:1, GOP2:1, GOP1:1, GOP0:1 % GPIO_DOUT
    >>);
reg(encode, gpio_irqe, Val) ->
    #{
        girqe8 := GIRQE8, girqe7 := GIRQE7, girqe6 := GIRQE6, girqe5 := GIRQE5, girqe4 := GIRQE4, girqe3 := GIRQE3, girqe2 := GIRQE2, girqe1 := GIRQE1, girqe0 := GIRQE0
     } = Val,
    reverse(<<
        2#0:23, GIRQE8:1, GIRQE7:1, GIRQE6:1, GIRQE5:1, GIRQE4:1, GIRQE3:1, GIRQE2:1, GIRQE1:1, GIRQE0:1 % GPIO_IRQE
    >>);
reg(encode, gpio_isen, Val) ->
    #{
        gisen8 := GISEN8, gisen7 := GISEN7, gisen6 := GISEN6, gisen5 := GISEN5, gisen4 := GISEN4, gisen3 := GISEN3, gisen2 := GISEN2, gisen1 := GISEN1, gisen0 := GISEN0
     } = Val,
    reverse(<<
        2#0:23, GISEN8:1, GISEN7:1, GISEN6:1, GISEN5:1, GISEN4:1, GISEN3:1, GISEN2:1, GISEN1:1, GISEN0:1 % GPIO_ISEN
    >>);
reg(encode, gpio_imod, Val) ->
    #{
        gimod8 := GIMOD8, gimod7 := GIMOD7, gimod6 := GIMOD6, gimod5 := GIMOD5, gimod4 := GIMOD4, gimod3 := GIMOD3, gimod2 := GIMOD2, gimod1 := GIMOD1, gimod0 := GIMOD0
     } = Val,
    reverse(<<
        2#0:23, GIMOD8:1, GIMOD7:1, GIMOD6:1, GIMOD5:1, GIMOD4:1, GIMOD3:1, GIMOD2:1, GIMOD1:1, GIMOD0:1 % GPIO_IMOD
    >>);
reg(encode, gpio_ibes, Val) ->
    #{
        gibes8 := GIBES8, gibes7 := GIBES7, gibes6 := GIBES6, gibes5 := GIBES5, gibes4 := GIBES4, gibes3 := GIBES3, gibes2 := GIBES2, gibes1 := GIBES1, gibes0 := GIBES0
     } = Val,
    reverse(<<
        2#0:23, GIBES8:1, GIBES7:1, GIBES6:1, GIBES5:1, GIBES4:1, GIBES3:1, GIBES2:1, GIBES1:1, GIBES0:1 % GPIO_IBES
    >>);
reg(encode, gpio_iclr, Val) ->
    #{
        giclr8 := GICLR8, giclr7 := GICLR7, giclr6 := GICLR6, giclr5 := GICLR5, giclr4 := GICLR4, giclr3 := GICLR3, giclr2 := GICLR2, giclr1 := GICLR1, giclr0 := GICLR0
     } = Val,
    reverse(<<
        2#0:23, GICLR8:1, GICLR7:1, GICLR6:1, GICLR5:1, GICLR4:1, GICLR3:1, GICLR2:1, GICLR1:1, GICLR0:1 % GPIO_ICLR
    >>);
reg(encode, gpio_idbe, Val) ->
    #{
        gidbe8 := GIDBE8, gidbe7 := GIDBE7, gidbe6 := GIDBE6, gidbe5 := GIDBE5, gidbe4 := GIDBE4, gidbe3 := GIDBE3, gidbe2 := GIDBE2, gidbe1 := GIDBE1, gidbe0 := GIDBE0
     } = Val,
    reverse(<<
        2#0:23, GIDBE8:1, GIDBE7:1, GIDBE6:1, GIDBE5:1, GIDBE4:1, GIDBE3:1, GIDBE2:1, GIDBE1:1, GIDBE0:1 % GPIO_IDBE
    >>);
reg(encode, gpio_raw, Val) ->
    #{
        grawp8 := GRAWP8, grawp7 := GRAWP7, grawp6 := GRAWP6, grawp5 := GRAWP5, grawp4 := GRAWP4, grawp3 := GRAWP3, grawp2 := GRAWP2, grawp1 := GRAWP1, grawp0 := GRAWP0
     } = Val,
    reverse(<<
        2#0:23, GRAWP8:1, GRAWP7:1, GRAWP6:1, GRAWP5:1, GRAWP4:1, GRAWP3:1, GRAWP2:1, GRAWP1:1, GRAWP0:1 % GPIO_RAW        
    >>);
reg(decode, gpio_ctrl, Resp) -> 
    <<
        _:23, GRAWP8:1, GRAWP7:1, GRAWP6:1, GRAWP5:1, GRAWP4:1, GRAWP3:1, GRAWP2:1, GRAWP1:1, GRAWP0:1, % GPIO_RAW        
        _:23, GIDBE8:1, GIDBE7:1, GIDBE6:1, GIDBE5:1, GIDBE4:1, GIDBE3:1, GIDBE2:1, GIDBE1:1, GIDBE0:1, % GPIO_IDBE
        _:23, GICLR8:1, GICLR7:1, GICLR6:1, GICLR5:1, GICLR4:1, GICLR3:1, GICLR2:1, GICLR1:1, GICLR0:1, % GPIO_ICLR
        _:23, GIBES8:1, GIBES7:1, GIBES6:1, GIBES5:1, GIBES4:1, GIBES3:1, GIBES2:1, GIBES1:1, GIBES0:1, % GPIO_IBES
        _:23, GIMOD8:1, GIMOD7:1, GIMOD6:1, GIMOD5:1, GIMOD4:1, GIMOD3:1, GIMOD2:1, GIMOD1:1, GIMOD0:1, % GPIO_IMOD
        _:23, GISEN8:1, GISEN7:1, GISEN6:1, GISEN5:1, GISEN4:1, GISEN3:1, GISEN2:1, GISEN1:1, GISEN0:1, % GPIO_ISEN
        _:23, GIRQE8:1, GIRQE7:1, GIRQE6:1, GIRQE5:1, GIRQE4:1, GIRQE3:1, GIRQE2:1, GIRQE1:1, GIRQE0:1, % GPIO_IRQE
        _:11, GOM8:1, _:3, GOP8:1, GOM7:1, GOM6:1, GOM5:1, GOM4:1, GOP7:1, GOP6:1, GOP5:1, GOP4:1, GOM3:1, GOM2:1, GOM1:1, GOM0:1, GOP3:1, GOP2:1, GOP1:1, GOP0:1, % GPIO_DOUT
        _:11, GDM8:1, _:3, GDP8:1, GDM7:1, GDM6:1, GDM5:1, GDM4:1, GDP7:1, GDP6:1, GDP5:1, GDP4:1, GDM3:1, GDM2:1, GDM1:1, GDM0:1, GDP3:1, GDP2:1, GDP1:1, GDP0:1, % GPIO_DIR
        _:32, % Reserved
        _:8, MSGP8:2, MSGP7:2, MSGP6:2, MSGP5:2, MSGP4:2, MSGP3:2, MSGP2:2, MSGP1:2, MSGP0:2, _:6 % GPIO_MODE
    >> = reverse(Resp),
    #{
        gpio_mode => #{msgp8 => MSGP8, msgp7 => MSGP7, msgp6 => MSGP6, msgp5 => MSGP5, msgp4 => MSGP4, msgp3 => MSGP3, msgp2 => MSGP2, msgp1 => MSGP1, msgp0 => MSGP0},
        gpio_dir => #{gdm8 => GDM8, gdm7 => GDM7, gdm6 => GDM6, gdm5 => GDM5, gdm4 => GDM4, gdm3 => GDM3, gdm2 => GDM2, gdm1 => GDM1, gdm0 => GDM0,
                      gdp8 => GDP8, gdp7 => GDP7, gdp6 => GDP6, gdp5 => GDP5, gdp4 => GDP4, gdp3 => GDP3, gdp2 => GDP2, gdp1 => GDP1, gdp0 => GDP0},
        gpio_dout => #{gom8 => GOM8, gom7 => GOM7, gom6 => GOM6, gom5 => GOM5, gom4 => GOM4, gom3 => GOM3, gom2 => GOM2, gom1 => GOM1, gom0 => GOM0,
                       gop8 => GOP8, gop7 => GOP7, gop6 => GOP6, gop5 => GOP5, gop4 => GOP4, gop3 => GOP3, gop2 => GOP2, gop1 => GOP1, gop0 => GOP0},
        gpio_irqe => #{girqe8 => GIRQE8, girqe7 => GIRQE7, girqe6 => GIRQE6, girqe5 => GIRQE5, girqe4 => GIRQE4, girqe3 => GIRQE3, girqe2 => GIRQE2, girqe1 => GIRQE1, girqe0 => GIRQE0},
        gpio_isen => #{gisen8 => GISEN8, gisen7 => GISEN7, gisen6 => GISEN6, gisen5 => GISEN5, gisen4 => GISEN4, gisen3 => GISEN3, gisen2 => GISEN2, gisen1 => GISEN1, gisen0 => GISEN0},
        gpio_imod => #{gimod8 => GIMOD8, gimod7 => GIMOD7, gimod6 => GIMOD6, gimod5 => GIMOD5, gimod4 => GIMOD4, gimod3 => GIMOD3, gimod2 => GIMOD2, gimod1 => GIMOD1, gimod0 => GIMOD0},
        gpio_ibes => #{gibes8 => GIBES8, gibes7 => GIBES7, gibes6 => GIBES6, gibes5 => GIBES5, gibes4 => GIBES4, gibes3 => GIBES3, gibes2 => GIBES2, gibes1 => GIBES1, gibes0 => GIBES0},
        gpio_iclr => #{giclr8 => GICLR8, giclr7 => GICLR7, giclr6 => GICLR6, giclr5 => GICLR5, giclr4 => GICLR4, giclr3 => GICLR3, giclr2 => GICLR2, giclr1 => GICLR1, giclr0 => GICLR0},
        gpio_idbe => #{gidbe8 => GIDBE8, gidbe7 => GIDBE7, gidbe6 => GIDBE6, gidbe5 => GIDBE5, gidbe4 => GIDBE4, gidbe3 => GIDBE3, gidbe2 => GIDBE2, gidbe1 => GIDBE1, gidbe0 => GIDBE0},
        gpio_raw => #{grawp8 => GRAWP8, grawp7 => GRAWP7, grawp6 => GRAWP6, grawp5 => GRAWP5, grawp4 => GRAWP4, grawp3 => GRAWP3, grawp2 => GRAWP2, grawp1 => GRAWP1, grawp0 => GRAWP0}
    };
reg(encode, drx_tune0b, Val) ->
    #{
        drx_tune0b := DRX_TUNE0b
     } = Val,
    reverse(<<
        DRX_TUNE0b:16
    >>);
reg(encode, drx_tune1a, Val) ->
    #{
        drx_tune1a := DRX_TUNE1a
     } = Val,
    reverse(<<
        DRX_TUNE1a:16
    >>);
reg(encode, drx_tune1b, Val) ->
    #{
        drx_tune1b := DRX_TUNE1b
     } = Val,
    reverse(<<
        DRX_TUNE1b:16
    >>);
reg(encode, drx_tune2, Val) ->
    #{
        drx_tune2 := DRX_TUNE2
     } = Val,
    reverse(<<
        DRX_TUNE2:32
    >>);
reg(encode, drx_sfdtoc, Val) ->
    #{
        drx_sfdtoc := DRX_SFDTOC
     } = Val,
    reverse(<<
        DRX_SFDTOC:16
    >>);
reg(encode, drx_pretoc, Val) ->
    #{
        drx_pretoc := DRX_PRETOC
     } = Val,
    reverse(<<
        DRX_PRETOC:16
    >>);
reg(encode, drx_tune4h, Val) ->
    #{
        drx_tune4h := DRX_TUNE4H
     } = Val,
    reverse(<<
        DRX_TUNE4H:16
    >>);
reg(decode, drx_conf, Resp) ->
    <<
        RXPACC_NOSAT:8, % present in the user manual but not in the driver code in C
        % _:8, % Placeholder for the remaining 8 bits
        DRX_CAR_INT:24,
        DRX_TUNE4H:16,
        DRX_PRETOC:16,
        _:16,
        DRX_SFDTOC:16,
        _:160,
        DRX_TUNE2:32,
        DRX_TUNE1b:16,
        DRX_TUNE1a:16,
        DRX_TUNE0b:16,
        _:16
    >> = reverse(Resp),
    #{
        drx_tune0b => DRX_TUNE0b,
        drx_tune1a => DRX_TUNE1a,
        drx_tune1b => DRX_TUNE1b,
        drx_tune2 => DRX_TUNE2,
        drx_tune4h => DRX_TUNE4H,
        drx_car_int => DRX_CAR_INT,
        drx_sfdtoc => DRX_SFDTOC,
        drx_pretoc => DRX_PRETOC,
        rxpacc_nosat => RXPACC_NOSAT
    };
reg(encode, rf_conf, Val) ->
    #{
        txrxsw := TXRXSW, ldofen := LDOFEN, pllfen := PLLFEN, txfen := TXFEN
     } = Val,
    reverse(<<
        2#0:9, TXRXSW:2, LDOFEN:5, PLLFEN:3, TXFEN:5, 2#0:8 % RF_CONF
    >>);
reg(encode, rf_rxctrlh, Val) ->
    #{
        rf_rxctrlh := RF_RXCTRLH
     } = Val,
    reverse(<<
        RF_RXCTRLH:8 % RF_RXCTRLH
    >>);
% user manual gives fields but encoding should be done as one following table 38
reg(encode, rf_txctrl, Val) ->
    #{
      rf_txctrl := RF_TXCTRL
     } = Val,
    reverse(<<
        RF_TXCTRL:32
    >>);
reg(encode, ldotune, Val) ->
    #{
      ldotune := LDOTUNE
     } = Val,
    reverse(<<
        LDOTUNE:40
    >>);
reg(decode, rf_conf, Resp) ->
    <<  
        _:40, % Placeholder for the remaining 40 bits
        LDOTUNE:40, % LDOTUNE
        _:28, RFPLLLOCK:1, CPLLHIGH:1, CPLLLOW:1, CPLLLOCK:1, % RF_STATUS
        _:128, _:96, % Reserved 2 - On user manual 16 bytes but offset gives 28 bytes (16 bytes (128 bits) + 12 bytes (96 bits))
        RF_TXCTRL:32, % cf. encode function: Reserved:20, TXMQ:3, TXMTUNE:4, _:5 - RF_TXCTRL
        RF_RXCTRLH:8, % RF_RXCTRLH
        _:56, % Reserved 1
        _:9, TXRXSW:2, LDOFEN:5, PLLFEN:3, TXFEN:5, _:8 % RF_CONF
    >> = reverse(Resp),
    #{
        ldotune => LDOTUNE,
        rf_status => #{rfplllock => RFPLLLOCK, cplllow => CPLLLOW, cpllhigh => CPLLHIGH, cplllock => CPLLLOCK},
        rf_txctrl => RF_TXCTRL,
        rf_rxctrlh => RF_RXCTRLH,
        rf_conf => #{txrxsw => TXRXSW, ldofen => LDOFEN, pllfen => PLLFEN, txfen => TXFEN}
    };
reg(encode, tc_sarc, Val) ->
    #{
        sar_ctrl := SAR_CTRL
     } = Val,
    reverse(<<
      2#0:15, SAR_CTRL:1
    >>);
reg(encode, tc_pg_ctrl, Val) ->
    #{
        pg_tmeas := PG_TMEAS, res := Reserved, pg_start := PG_START
     } = Val,
    reverse(<<
        2#0:2, PG_TMEAS:4, Reserved:1, PG_START:1
    >>);
reg(encode, tc_pgdelay, Val) ->
    #{
        tc_pgdelay := TC_PGDELAY
     } = Val,
    reverse(<<
        TC_PGDELAY:8
    >>);
reg(encode, tc_pgtest, Val) ->
    #{
        tc_pgtest := TC_PGTEST
     } = Val,
    reverse(<<
        TC_PGTEST:8
    >>);
reg(decode, tx_cal, Resp) -> 
    <<
        TC_PGTEST:8, % TC_PGTEST
        TC_PGDELAY:8, % TC_PGDELAY
        _:4, DELAY_CNT:12, % TC_PG_STATUS
        _:2, PG_TMEAS:4, Reserved0:1, PG_START:1, % TC_PG_CTRL
        SAR_WTEMP:8, SAR_WVBAT:8, % TC_SARW
        _:8, SAR_LTEMP:8, SAR_LVBAT:8, % TC_SARL
        _:8, % Place holder to fill the gap between the offsets
        _:15, SAR_CTRL:1 % TC_SARC
    >> = reverse(Resp),
    #{
        tc_pgtest => TC_PGTEST,
        tc_pgdelay => TC_PGDELAY,
        tc_pg_status => #{delay_cnt => DELAY_CNT},
        tc_pg_ctrl => #{pg_tmeas => PG_TMEAS, res => Reserved0, pg_start => PG_START},
        tc_sarw => #{sar_wtemp => SAR_WTEMP, sar_wvbat => SAR_WVBAT},
        tc_sarl => #{sar_ltemp => SAR_LTEMP, sar_lvbat => SAR_LVBAT},
        tc_sarc => #{sar_ctrl => SAR_CTRL}
    };
reg(encode, fs_pllcfg, Val) -> 
    #{
        fs_pllcfg := FS_PLLCFG
     } = Val,
    reverse(<<
        FS_PLLCFG:32
    >>);
reg(encode, fs_plltune, Val) ->
    #{
        fs_plltune := FS_PLLTUNE
     } = Val,
    reverse(<<
        FS_PLLTUNE:8
    >>);
reg(encode, fs_xtalt, Val) ->
    #{
      res := Reserved, xtalt := XTALT
     } = Val,
    reverse(<<
        Reserved:3, XTALT:5
    >>);
reg(decode, fs_ctrl, Resp) -> 
    <<
        _:48, % Reserved 3
        Reserved:3, XTALT:5, % FS_XTALT
        _:16, % Reserved 2
        FS_PLLTUNE:8, % FS_PLLTUNE
        FS_PLLCFG:32, % FS_PLLCFG
        _:56 % Reserved 1
    >> = reverse(Resp),
    #{
        fs_xtalt => #{res => Reserved, xtalt => XTALT},
        fs_plltune => FS_PLLTUNE, 
        fs_pllcfg => FS_PLLCFG
    };
reg(encode, aon_wcfg, Val) ->
    #{
        onw_lld := ONW_LLD, onw_llde := ONW_LLDE, pres_slee := PRES_SLEE, own_l64 := OWN_L64, own_ldc := OWN_LDC, own_leui := OWN_LEUI, own_rx := OWN_RX, own_rad := OWN_RAD
     } = Val,
    reverse(<<
        2#0:3, ONW_LLD:1, ONW_LLDE:1, 2#0:2, PRES_SLEE:1, OWN_L64:1, OWN_LDC:1, 2#0:2, OWN_LEUI:1, 2#0:1, OWN_RX:1, OWN_RAD:1 % AON_WCFG
    >>);
reg(encode, aon_ctrl, Val) ->
    #{
        dca_enab := DCA_ENAB, dca_read := DCA_READ, upl_cfg := UPL_CFG, save := SAVE, restore := RESTORE
     } = Val,
    reverse(<<
        DCA_ENAB:1, 2#0:3, DCA_READ:1, UPL_CFG:1, SAVE:1, RESTORE:1 % AON_CTRL
    >>);
reg(encode, aon_rdat, Val) ->
    #{
        aon_rdat := AON_RDAT
     } = Val,
    reverse(<<
        AON_RDAT:8 % AON_RDAT
    >>);
reg(encode, aon_addr, Val) -> 
    #{
        aon_addr := AON_ADDR
     } = Val,
    reverse(<<
        AON_ADDR:8 % AON_ADDR
    >>);
reg(encode, aon_cfg0, Val) ->
    #{
        sleep_tim := SLEEP_TIM, lpclkdiva := LPCLKDIVA, lpdiv_en := LPDIV_EN, wake_cnt := WAKE_CNT, wake_spi := WAKE_SPI, wake_pin := WAKE_PIN, sleep_en := SLEEP_EN
     } = Val,
    reverse(<<
        SLEEP_TIM:16, LPCLKDIVA:11, LPDIV_EN:1, WAKE_CNT:1, WAKE_SPI:1, WAKE_PIN:1, SLEEP_EN:1 % AON_CFG0
    >>);
reg(encode, aon_cfg1, Val) ->
    #{
        res := Reserved, lposc_c := LPOSC_C, smxx := SMXX, sleep_ce := SLEEP_CE
     } = Val,
    reverse(<<
        Reserved:13, LPOSC_C:1, SMXX:1, SLEEP_CE:1 % AON_CFG1
    >>);
reg(decode, aon, Resp) ->
    <<
        Reserved:13, LPOSC_C:1, SMXX:1, SLEEP_CE:1, % AON_CFG1
        SLEEP_TIM:16, LPCLKDIVA:11, LPDIV_EN:1, WAKE_CNT:1, WAKE_SPI:1, WAKE_PIN:1, SLEEP_EN:1, % AON_CFG0
        _:8, % Reserved 1
        AON_ADDR:8, % AON_ADDR
        AON_RDAT:8, % AON_RDAT
        DCA_ENAB:1, _:3, DCA_READ:1, UPL_CFG:1, SAVE:1, RESTORE:1, % AON_CTRL
        _:3, ONW_LLD:1, ONW_LLDE:1, _:2, PRES_SLEE:1, OWN_L64:1, OWN_LDC:1, _:2, OWN_LEUI:1, _:1, OWN_RX:1, OWN_RAD:1 % AON_WCFG
    >> = reverse(Resp),
    #{
        aon_cfg1 => #{res => Reserved, lposc_c => LPOSC_C, smxx => SMXX, sleep_ce => SLEEP_CE},
        aon_cfg0 => #{sleep_tim => SLEEP_TIM, lpclkdiva => LPCLKDIVA, lpdiv_en => LPDIV_EN, wake_cnt => WAKE_CNT, wake_spi => WAKE_SPI, wake_pin => WAKE_PIN, sleep_en => SLEEP_EN},
        aon_addr => AON_ADDR,
        aon_rdat => AON_RDAT,
        aon_ctrl => #{dca_enab => DCA_ENAB, dca_read => DCA_READ, upl_cfg => UPL_CFG, save => SAVE, restore => RESTORE},
        aon_wcfg => #{onw_lld => ONW_LLD, onw_llde => ONW_LLDE, pres_slee => PRES_SLEE, own_l64 => OWN_L64, own_ldc => OWN_LDC, own_leui => OWN_LEUI, own_rx => OWN_RX, own_rad => OWN_RAD}
    };
reg(encode, otp_wdat, Val) ->
    #{
        otp_wdat := OTP_WDAT
     } = Val,
    reverse(<<
        OTP_WDAT:32 % OTP_WDAT
    >>);
reg(encode, otp_addr, Val) ->
    #{
        otpaddr := OTP_ADDR, res := Reserved
     } = Val,
    reverse(<<
        Reserved:5, OTP_ADDR:11 % OTP_ADDR
    >>);
reg(encode, otp_ctrl, Val) ->
    #{
        ldeload := LDELOAD, res1 := Reserved1, otpmr := OTPMR, otpprog := OTPPROG, res2 := Reserved2, otpmrwr := OTPMRWR, res3 := Reserved3, otpread := OTPREAD, otp_rden := OTPRDEN
     } = Val,
    reverse(<<
        LDELOAD:1, Reserved1:4, OTPMR:4, OTPPROG:1, Reserved2:2, OTPMRWR:1, Reserved3:1, OTPREAD:1, OTPRDEN:1 % OTP_CTRL
    >>);
reg(encode, otp_stat, Val) ->
    #{
        res := Reserved, otp_vpok := OTP_VPOK, otpprgd := OTPPRGD
     } = Val,
    reverse(<<
        Reserved:14, OTP_VPOK:1, OTPPRGD:1 % OTP_STAT
    >>);
reg(encode, otp_rdat, Val) ->
    #{
        otp_rdat := OTP_RDAT
     } = Val,
    reverse(<<
        OTP_RDAT:32 % OTP_RDAT
    >>);
reg(encode, opt_srdat, Val) ->
    #{
        otp_srdat := OTP_SRDAT
     } = Val,
    reverse(<<
        OTP_SRDAT:32 % OTP_SRDAT
    >>);
reg(encode, otp_sf, Val) ->
    #{
        res1 := Reserved1, ops_sel := OPS_SEL, res2 := Reserved2, ldo_kick := LDO_KICK, ops_kick := OPS_KICK
     } = Val,
    reverse(<<
        Reserved1:2, OPS_SEL:1, Reserved2:3, LDO_KICK:1, OPS_KICK:1 % OTP_SF
    >>);
reg(decode, otp_if, Resp) ->
    <<
        Reserved5:2, OPS_SEL:1, Reserved6:3, LDO_KICK:1, OPS_KICK:1, % OTP_SF
        OTP_SRDAT:32, % OTP_SRDAT
        OTP_RDAT:32, % OTP_RDAT
        Reserved4:14, OTP_VPOK:1, OTPPRGD:1, % OTP_STAT
        LDELOAD:1, Reserved1:4, OTPMR:4, OTPPROG:1, Reserved2:2, OTPMRWR:1, Reserved3:1, OTPREAD:1, OTPRDEN:1, % OTP_CTRL
        Reserved0:5, OTP_ADDR:11, % OTP_ADDR
        OTP_WDAT:32 % OTP_WDAT
    >> = reverse(Resp),
    #{
        otp_sf => #{res1 => Reserved5, ops_sel => OPS_SEL, res2 => Reserved6, ldo_kick => LDO_KICK, ops_kick => OPS_KICK},
        otp_srdat => OTP_SRDAT,
        otp_rdat => OTP_RDAT,
        otp_stat => #{res => Reserved4, otp_vpok => OTP_VPOK, otpprgd => OTPPRGD},
        otp_ctrl => #{ldeload => LDELOAD, res1 => Reserved1, otpmr => OTPMR, otpprog => OTPPROG, res2 => Reserved2, otpmrwr => OTPMRWR, res3 => Reserved3, otpread => OTPREAD, otp_rden => OTPRDEN},
        otp_addr => #{otpaddr => OTP_ADDR, res => Reserved0},
        otp_wdat => OTP_WDAT
    };
reg(decode, lde_thresh, Resp) ->
    <<
      LDE_THRESH:16
    >> = reverse(Resp),
    #{
      lde_thresh => LDE_THRESH
    };
reg(encode, lde_cfg1, Val) ->
    #{
      pmult := PMULT, ntm := NTM
     } = Val,
    reverse(<<
        PMULT:3, NTM:5
    >>);
reg(decode, lde_cfg1, Resp) ->
    <<
      PMULT:3, NTM:5
    >> = reverse(Resp),
    #{
      lde_cfg1 => #{pmult => PMULT, ntm => NTM}
    };
reg(decode, lde_ppindx, Resp) ->
    <<
      LDE_PPINDX:16
    >> = reverse(Resp),
    #{
      lde_ppindx => LDE_PPINDX
    };
reg(decode, lde_ppampl, Resp) ->
    <<
      LDE_PPAMPL:16
    >> = reverse(Resp),
    #{
      lde_ppampl => LDE_PPAMPL
    };
reg(encode, lde_rxantd, Val) ->
    #{
      lde_rxantd := LDE_RXANTD
     } = Val,
    reverse(<<
        LDE_RXANTD:16 
    >>);
reg(decode, lde_rxantd, Resp) ->
    <<
      LDE_RXANTD:16
    >> = reverse(Resp),
    #{
      lde_rxantd => LDE_RXANTD
    };
reg(encode, lde_cfg2, Val) ->
    #{
        lde_cfg2 := LDE_CFG2
     } = Val,
    reverse(<<
        LDE_CFG2:16
    >>);
reg(decode, lde_cfg2, Resp) ->
    <<
      LDE_CFG2:16
    >> = reverse(Resp),
    #{
      lde_cfg2 => LDE_CFG2
    };
reg(encode, lde_repc, Val) ->
    #{
        lde_repc := LDE_REPC
     } = Val,
    reverse(<<
        LDE_REPC:16
    >>);
reg(decode, lde_repc, Resp) ->
    <<
      LDE_REPC:16
    >> = reverse(Resp),
    #{
      lde_repc => LDE_REPC
    };
reg(encode, evc_ctrl, Val) ->
    #{
        evc_clr := EVC_CLR, evc_en := EVC_EN
     } = Val,
    reverse(<<
        2#0:30, EVC_CLR:1, EVC_EN:1 % EVC_CTRL
    >>);
reg(encode, diag_tmc, Val) ->
    #{
        tx_pstm := TX_PSTM
     } = Val,
    reverse(<<
        2#0:11, TX_PSTM:1, 2#0:4 % DIAG_TMC
    >>);
reg(decode, dig_diag, Resp) -> 
    <<
        _:11, TX_PSTM:1, _:4, % DIAG_TMC
        _:64, % Reserved 1
        _:4, EVC_TPW:12, % EVC_TPW
        _:4, EVC_HPW:12, % EVC_HPW
        _:4, EVC_TXFS:12, % EVC_TXFS
        _:4, EVC_FWTO:12, % EVC_FWTO
        _:4, EVC_PTO:12, % EVC_PTO
        _:4, EVC_STO:12, % EVC_STO
        _:4, ECV_OVR:12, % EVC_OVR
        _:4, EVC_FFR:12, % EVC_FFR
        _:4, EVC_FCE:12, % EVC_FCE
        _:4, EVC_FCG:12, % EVC_FCG
        _:4, EVC_RSE:12, % EVC_RSE
        _:4, EVC_PHE:12, % EVC_PHE
        _:30, EVC_CLR:1, EVC_EN:1 % EVC_CTRL
    >> = reverse(Resp),
    #{
        diag_tmc => #{tx_pstm => TX_PSTM},
        evc_tpw => EVC_TPW,
        evc_hpw => EVC_HPW,
        evc_txfs => EVC_TXFS,
        evc_fwto => EVC_FWTO,
        evc_pto => EVC_PTO,
        evc_sto => EVC_STO,
        evc_ovr => ECV_OVR,
        evc_ffr => EVC_FFR,
        evc_fce => EVC_FCE,
        evc_fcg => EVC_FCG,
        evc_rse => EVC_RSE,
        evc_phe => EVC_PHE,
        evc_ctrl => #{evc_clr => EVC_CLR, evc_en => EVC_EN}
    };
reg(encode, pmsc_ctrl0, Val) ->
    #{
        softreset := SOFTRESET, pll2_seq_en := PLL2_SEQ_EN, khzclken := KHZCLKEN, gpdrn := GPDRN, gpdce := GPDCE, 
        gprn := GPRN, gpce := GPCE, amce := AMCE, adcce := ADCCE, otp := OTP, res8 := Res8, res7 := Res7, face := FACE, txclks := TXCLKS, rxclks := RXCLKS, sysclks := SYSCLKS % Here we need res8 for the initial config of the DW1000. We need to write it
     } = Val,
    reverse(<<
        SOFTRESET:4, 2#000:3, PLL2_SEQ_EN:1, KHZCLKEN:1, 2#011:3, GPDRN:1, GPDCE:1, GPRN:1, GPCE:1, AMCE:1, 2#0000:4, ADCCE:1, OTP:1, Res8:1, Res7:1, FACE:1, TXCLKS:2, RXCLKS:2, SYSCLKS:2 % PMSC_CTRL0
    >>);
reg(encode, pmsc_ctrl1, Val) ->
    #{
        khzclkdiv := KHZCLKDIV, lderune := LDERUNE, pllsyn := PLLSYN, snozr := SNOZR, snoze := SNOZE, arxslp := ARXSLP, atxslp := ATXSLP, pktseq := PKTSEQ, arx2init := ARX2INIT
     } = Val,
    reverse(<<
        KHZCLKDIV:6, 2#01000000:8, LDERUNE:1, 2#0:1, PLLSYN:1, SNOZR:1, SNOZE:1, ARXSLP:1, ATXSLP:1, PKTSEQ:8, 2#0:1, ARX2INIT:1, 2#0:1 % PMSC_CTRL1
    >>);
reg(encode, pmsc_snozt, Val) ->
    #{
        snoz_tim := SNOZ_TIM
     } = Val,
    reverse(<<
        SNOZ_TIM:8 % PMSC_SNOZT
    >>);
reg(encode, pmsc_txfseq, Val) ->
    #{
        txfineseq := TXFINESEQ
     } = Val,
    reverse(<<
        TXFINESEQ:16 % PMSC_TXFINESEQ
    >>);
reg(encode, pmsc_ledc, Val) ->
    #{
        res31 := RES31, blnknow := BLNKNOW, res15 := RES15, blnken := BLNKEN, blink_tim := BLINK_TIM
     } = Val,
    reverse(<<
        RES31:12, BLNKNOW:4, RES15:7, BLNKEN:1, BLINK_TIM:8 % PMSC_LEDC
    >>);
% mapping pmsc ctrl0 from: https://forum.qorvo.com/t/pmsc-ctrl0-bits8-15/746/3
reg(decode, pmsc, Resp) ->
    % User manual says: reserved bits should be preserved at their reset value => can hardcode their values ? Safe to do that ?
    <<
        Res31:12, BLNKNOW:4, Res15:7, BLNKEN:1, BLINK_TIM:8, % PMSC_LEDC
        TXFINESEQ:16, % PMSC_TXFINESEQ
        _:(25*8), % Reserved 2
        SNOZ_TIM:8, % PMSC_SNOZT
        _:32, % Reserved 1
        KHZCLKDIV:6, _:8, LDERUNE:1, _:1, PLLSYN:1, SNOZR:1, SNOZE:1, ARXSLP:1, ATXSLP:1, PKTSEQ:8, _:1, ARX2INIT:1, _:1, % PMSC_CTRL1
        SOFTRESET:4, _:3, PLL2_SEQ_EN:1, KHZCLKEN:1, _:3, GPDRN:1, GPDCE:1, GPRN:1, GPCE:1, AMCE:1, _:4, ADCCE:1, OTP:1, Res8:1, Res7:1, FACE:1, TXCLKS:2, RXCLKS:2, SYSCLKS:2 % PMSC_CTRL0
    >> = reverse(Resp),
    #{
        pmsc_ledc => #{res31 => Res31, blnknow => BLNKNOW, res15 => Res15, blnken => BLNKEN, blink_tim => BLINK_TIM},
        pmsc_txfseq => #{txfineseq => TXFINESEQ},
        pmsc_snozt => #{snoz_tim => SNOZ_TIM},
        pmsc_ctrl1 => #{khzclkdiv => KHZCLKDIV, lderune => LDERUNE, pllsyn => PLLSYN, snozr => SNOZR, snoze => SNOZE, arxslp => ARXSLP, atxslp => ATXSLP, pktseq => PKTSEQ, arx2init => ARX2INIT},
        pmsc_ctrl0 => #{softreset => SOFTRESET, pll2_seq_en => PLL2_SEQ_EN, khzclken => KHZCLKEN, gpdrn => GPDRN, gpdce => GPDCE, gprn => GPRN, gpce => GPCE, amce => AMCE, adcce => ADCCE, otp => OTP, res8 => Res8, res7 => Res7, face => FACE, txclks => TXCLKS, rxclks => RXCLKS, sysclks => SYSCLKS}
    };
reg(decode, RegFile, Resp) -> error({unknown_regfile_to_decode, RegFile, Resp});
reg(encode, RegFile, Resp) -> error({unknown_regfile_to_encode, RegFile, Resp}).

rw(read) -> 0;
rw(write) -> 1.

% Mapping of the different register IDs to their hexadecimal value
regFile(dev_id) -> 16#00;
regFile(eui) -> 16#01;
% 0x02 is reserved
regFile(panadr) -> 16#03;
regFile(sys_cfg) -> 16#04;
% 0x05 is reserved
regFile(sys_time) -> 16#06;
% 0x07 is reserved
regFile(tx_fctrl) -> 16#08;
regFile(tx_buffer) -> 16#09;
regFile(dx_time) -> 16#0A;
% 0x0B is reserved
regFile(rx_fwto) -> 16#0C;
regFile(sys_ctrl) -> 16#0D;
regFile(sys_mask) -> 16#0E;
regFile(sys_status) -> 16#0F;
regFile(rx_finfo) -> 16#10;
regFile(rx_buffer) -> 16#11;
regFile(rx_fqual) -> 16#12;
regFile(rx_ttcki) -> 16#13;
regFile(rx_ttcko) -> 16#14;
regFile(rx_time) -> 16#15;
% 0x16 is reserved
regFile(tx_time) -> 16#17;
regFile(tx_antd) -> 16#18;
regFile(sys_state) -> 16#19;
regFile(ack_resp_t) -> 16#1A;
% 0x1B is reserved
% 0x1C is reserved
regFile(rx_sniff) -> 16#1D;
regFile(tx_power) -> 16#1E;
regFile(chan_ctrl) -> 16#1F;
% 0x20 is reserved
regFile(usr_sfd) -> 16#21;
% 0x22 is reserved
regFile(agc_ctrl) -> 16#23;
regFile(ext_sync) -> 16#24;
regFile(acc_mem) -> 16#25;
regFile(gpio_ctrl) -> 16#26;
regFile(drx_conf) -> 16#27;
regFile(rf_conf) -> 16#28;
% 0x29 is reserved
regFile(tx_cal) -> 16#2A;
regFile(fs_ctrl) -> 16#2B;
regFile(aon) -> 16#2C;
regFile(otp_if) -> 16#2D;
regFile(lde_ctrl) -> regFile(lde_if); % No size ?
regFile(lde_if) -> 16#2E;
regFile(dig_diag) -> 16#2F;
% 0x30 - 0x35 are reserved
regFile(pmsc) -> 16#36;
% 0x37 - 0x3F are reserved
regFile(RegId) -> error({wrong_register_ID, RegId}).

% Only the writtable subregisters in SRW register files are present here
% AGC_CTRL
subReg(agc_ctrl1) -> 16#02;
subReg(agc_tune1) -> 16#04;
subReg(agc_tune2) -> 16#0C;
subReg(agc_tune3) -> 16#12;
subReg(agc_stat1) -> 16#1E;
subReg(ec_ctrl) -> 16#00;
subReg(gpio_mode) -> 16#00;
subReg(gpio_dir) -> 16#08;
subReg(gpio_dout) -> 16#0C;
subReg(gpio_irqe) -> 16#10;
subReg(gpio_isen) -> 16#14;
subReg(gpio_imode) -> 16#18;
subReg(gpio_ibes) -> 16#1C;
subReg(gpio_iclr) -> 16#20;
subReg(gpio_idbe) -> 16#24;
subReg(gpio_raw) -> 16#28;
subReg(drx_tune0b) -> 16#02;
subReg(drx_tune1a) -> 16#04;
subReg(drx_tune1b) -> 16#06;
subReg(drx_tune2) -> 16#08;
subReg(drx_sfdtoc) -> 16#20;
subReg(drx_pretoc) -> 16#24;
subReg(drx_tune4h) -> 16#26;
subReg(rf_conf) -> 16#00;
subReg(rf_rxctrlh) -> 16#0B;
subReg(rf_txctrl) -> 16#0C;
subReg(ldotune) -> 16#30;
subReg(tc_sarc) -> 16#00;
subReg(tc_pg_ctrl) -> 16#08;
subReg(tc_pgdelay) -> 16#0B;
subReg(tc_pgtest) -> 16#0C;
subReg(fs_pllcfg) -> 16#07;
subReg(fs_plltune) -> 16#0B;
subReg(fs_xtalt) -> 16#0E;
subReg(aon_wcfg) -> 16#00;
subReg(aon_ctrl) -> 16#02;
subReg(aon_rdat) -> 16#03;
subReg(aon_addr) -> 16#04;
subReg(aon_cfg0) -> 16#06;
subReg(aon_cfg1) -> 16#0A;
subReg(otp_wdat) -> 16#00;
subReg(otp_addr) -> 16#04;
subReg(otp_ctrl) -> 16#06;
subReg(otp_stat) -> 16#08;
subReg(otp_rdat) -> 16#0A;
subReg(otp_srdat) -> 16#0E;
subReg(otp_sf) -> 16#12;
subReg(lde_thresh) -> 16#00;
subReg(lde_cfg1) -> 16#806;
subReg(lde_ppindx) -> 16#1000;
subReg(lde_ppampl) -> 16#1002;
subReg(lde_rxantd) -> 16#1804;
subReg(lde_cfg2) -> 16#1806;
subReg(lde_repc) -> 16#2804;
subReg(evc_ctrl) -> 16#00;
subReg(diag_tmc) -> 16#24;
subReg(pmsc_ctrl0) -> 16#00;
subReg(pmsc_ctrl1) -> 16#04;
subReg(pmsc_snozt) -> 16#0C;
subReg(pmsc_txfseq) -> 16#26;
subReg(pmsc_ledc) -> 16#28.


% Mapping of the size in bytes of the different register IDs
regSize(dev_id) -> 4;
regSize(eui) -> 8;
regSize(panadr) -> 4;
regSize(sys_cfg) -> 4;
regSize(sys_time) -> 5;
regSize(tx_fctrl) -> 5;
regSize(tx_buffer) -> 1024;
regSize(dx_time) -> 5;
regSize(rx_fwto) -> 2; % user manual gives 2 bytes and bits 16-31 are reserved
regSize(sys_ctrl) -> 4;
regSize(sys_mask) -> 4;
regSize(sys_status) -> 5;
regSize(rx_finfo) -> 4;
regSize(rx_buffer) -> 1024;
regSize(rx_fqual) -> 8;
regSize(rx_ttcki) -> 4;
regSize(rx_ttcko) -> 5;
regSize(rx_time) -> 14;
regSize(tx_time) -> 10;
regSize(tx_antd) -> 2;
regSize(sys_state) -> 4;
regSize(ack_resp_t) -> 4;
regSize(rx_sniff) -> 4;
regSize(tx_power) -> 4;
regSize(chan_ctrl) -> 4;
regSize(usr_sfd) -> 41;
regSize(agc_ctrl) -> 33;
regSize(ext_sync) -> 12;
regSize(acc_mem) -> 4064;
regSize(gpio_ctrl) -> 44;
regSize(drx_conf) -> 44; % user manual gives 44 bytes but sum of register length gives 45 bytes
regSize(rf_conf) -> 58; % user manual gives 58 but sum of all its register gives 53 => Placeholder for the remaining 8 bytes
regSize(tx_cal) -> 13; % user manual gives 52 bytes but sum of all sub regs gives 13 bytes
regSize(fs_ctrl) -> 21;
regSize(aon) -> 12;
regSize(otp_if) -> 19; % user manual gives 18 bytes in regs table but sum of all sub regs is 19 bytes
regSize(lde_ctrl) -> undefined; % No size ?
regSize(lde_if) -> undefined; % No size ?
regSize(dig_diag) -> 38; % user manual gives 41 bytes but sum of all sub regs gives 38 bytes
regSize(pmsc) -> 44. % user manual gives 48 bytes but sum of all sub regs gives 41 bytes

%% Gives the size in bytes
subRegSize(agc_ctrl1) -> 2;
subRegSize(agc_tune1) -> 2;
subRegSize(agc_tune2) -> 4;
subRegSize(agc_tune3) -> 2;
subRegSize(agc_stat1) -> 3;
subRegSize(ec_ctrl) -> 4;
subRegSize(gpio_mode) -> 4;
subRegSize(gpio_dir) -> 4;
subRegSize(gpio_dout) -> 4;
subRegSize(gpio_irqe) -> 4;
subRegSize(gpio_isen) -> 4;
subRegSize(gpio_imode) -> 4;
subRegSize(gpio_ibes) -> 4;
subRegSize(gpio_iclr) -> 4;
subRegSize(gpio_idbe) -> 4;
subRegSize(gpio_raw) -> 4;
subRegSize(drx_tune0b) -> 2;
subRegSize(drx_tune1a) -> 2;
subRegSize(drx_tune1b) -> 2;
subRegSize(drx_tune2) -> 4;
subRegSize(drx_sfdtoc) -> 2;
subRegSize(drx_pretoc) -> 2;
subRegSize(drx_tune4h) -> 2;
subRegSize(rf_conf) -> 4;
subRegSize(rf_rxctrlh) -> 1;
subRegSize(rf_txctrl) -> 4; % ! table in user manual gives 3 but details gives 4
subRegSize(ldotune) -> 5;
subRegSize(tc_sarc) -> 2;
subRegSize(tc_pg_ctrl) -> 1;
subRegSize(tc_pgdelay) -> 1;
subRegSize(tc_pgtest) -> 1;
subRegSize(fs_pllcfg) -> 4;
subRegSize(fs_plltune) -> 1;
subRegSize(fs_xtalt) -> 1;
subRegSize(aon_wcfg) -> 2;
subRegSize(aon_ctrl) -> 1;
subRegSize(aon_rdat) -> 1;
subRegSize(aon_addr) -> 1;
subRegSize(aon_cfg0) -> 4;
subRegSize(aon_cfg1) -> 2;
subRegSize(otp_wdat) -> 4;
subRegSize(otp_addr) -> 2;
subRegSize(otp_ctrl) -> 2;
subRegSize(otp_stat) -> 2;
subRegSize(otp_rdat) -> 4;
subRegSize(otp_srdat) -> 4;
subRegSize(otp_sf) -> 1;
subRegSize(lde_thresh) -> 2;
subRegSize(lde_cfg1) -> 1;
subRegSize(lde_ppindx) -> 2;
subRegSize(lde_ppampl) -> 2;
subRegSize(lde_rxantd) -> 2;
subRegSize(lde_cfg2) -> 2;
subRegSize(lde_repc) -> 2;
subRegSize(evc_ctrl) -> 4;
subRegSize(diag_tmc) -> 2;
subRegSize(pmsc_ctrl0) -> 4;
subRegSize(pmsc_ctrl1) -> 4;
subRegSize(pmsc_snozt) -> 1;
subRegSize(pmsc_txfseq) -> 2;
subRegSize(pmsc_ledc) -> 4;
subRegSize(_) -> error({error}).

%--- Debug ---------------------------------------------------------------------

debug_read(Reg, Value) ->
    io:format("[PmodUWB] read [16#~2.16.0B - ~w] --> ~s -> ~s~n",
        [regFile(Reg), Reg, debug_bitstring(Value), debug_bitstring_hex(Value)]
    ).

debug_write(Reg, Value) ->
    io:format("[PmodUWB] write [16#~2.16.0B - ~w] --> ~s -> ~s~n",
        [regFile(Reg), Reg, debug_bitstring(Value), debug_bitstring_hex(Value)]
    ).
debug_write(Reg, SubReg, Value) ->
    io:format("[PmodUWB] write [16#~2.16.0B - ~w - 16#~2.16.0B - ~w] --> ~s -> ~s~n",
        [regFile(Reg), Reg, subReg(SubReg), SubReg, debug_bitstring(Value), debug_bitstring_hex(Value)]
    ).

debug_bitstring(Bitstring) ->
    lists:flatten([io_lib:format("2#~8.2.0B ", [X]) || <<X>> <= Bitstring]).

debug_bitstring_hex(Bitstring) ->
    lists:flatten([io_lib:format("16#~2.16.0B ", [X]) || <<X>> <= Bitstring]).