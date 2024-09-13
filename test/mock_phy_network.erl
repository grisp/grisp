-module(mock_phy_network).
-behaviour(gen_statem).

-include("mac_frame.hrl").


-export([start_link/2]).
-export([start/2]).
-export([stop_link/0]).

-export([transmit/2]).
-export([reception/0]).
-export([reception_async/0]).
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
-export([get_rx_metadata/0]).

%%% gen_statem callbacks
-export([init/1]).
-export([callback_mode/0]).
-export([idle/3, rx_on/3, idle_rx/3, idle_to/3]).
-export([terminate/2]).

%--- Records -------------------------------------------------------------------



%--- API -----------------------------------------------------------------------

start_link(_Connector, Params) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Params, []).

start(_Connector, Params) ->
    gen_statem:start({local, ?MODULE}, ?MODULE, Params, []).

stop_link() ->
    gen_statem:stop(?MODULE).

transmit(Frame, Options) ->
    gen_statem:call(?MODULE, {transmit, Frame, Options}).

reception() ->
    case {read(drx_conf), read(rx_fwto)} of
        {#{drx_pretoc := 0}, #{rxfwto := RXFWTO}} ->
            rx_(round(RXFWTO/1000), rxrfto);
        {#{drx_pretoc := PRETOC}, _} ->
            rx_(round(PRETOC/1000), rxpto)
    end.

reception_async() ->
    case reception() of
        {error, _} = Err ->
            ct:log("Error? : ~p", [Err]),
            Err;
        Frame ->
            ct:log("Frame: ~p", [Frame]),
            Metadata = get_rx_metadata(),
            ieee802154_events:rx_event(Frame, Metadata)
    end.

rx_(Timeout, TimeoutError) ->
    case gen_statem:call(?MODULE, {enable_rx, Timeout}, infinity) of
        timeout ->
            {error, TimeoutError};
        affrej ->
            {error, affrej};
        Ret ->
            ct:log("ret: ~p", [Ret]),
            Ret
    end.

reception(_RxOpts) ->
    reception().

disable_rx() ->
    gen_statem:call(?MODULE, {disable_rx}).

set_frame_timeout(Timeout) when is_float(Timeout) ->
    set_frame_timeout(trunc(Timeout));
set_frame_timeout(Timeout) when is_integer(Timeout) ->
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
    gen_statem:call(?MODULE, {read, Reg}).

write(Reg, Value) ->
    gen_statem:call(?MODULE, {write, Reg, Value}).

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
    gen_server:call(?MODULE, {get_conf}).

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
    RSL = signal_power(),
    RSL + Delta.

%--- Internal: gen server callbacks --------------------------------------------

init(#{network := NetworkNode}) ->
    {network_loop, NetworkNode} ! {register, node()},
    ets:new(callback_table, [public, named_table]),
    {ok, idle, #{regs => pmod_uwb_registers:default(),
                 network => NetworkNode,
                 conf => #phy_cfg{}}}.

callback_mode() ->
    [state_functions, state_enter].

idle(enter, _OldState, Data) ->
    {keep_state, Data};
idle({call, From}, {transmit, Frame, Options}, Data) ->
    #{network := NetworkNode, regs := Regs} = Data,
    NewRegs = tx(Frame, Options, NetworkNode, Regs),
    case Options#tx_opts.wait4resp of
        ?ENABLED ->
            {next_state, rx_on, Data#{regs := NewRegs}, {reply, From, ok}};
        ?DISABLED ->
            {keep_state, Data#{regs := NewRegs}, {reply, From, ok}}
    end;
idle({call, From}, {enable_rx, Timeout}, Data) ->
    {next_state, rx_on, Data#{timeout => Timeout, waiting => From}};
idle({call, From}, {disable_rx}, Data) ->
    {keep_state, Data, {reply, From, ok}};
idle(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

rx_on(enter, _OldState, #{regs := Regs, timeout := Timeout} = Data) ->
    NewRegs = enable_rx(Regs),
    TimerRef = erlang:start_timer(Timeout, ?MODULE, rx_timeout),
    {keep_state, Data#{regs => NewRegs, timer => TimerRef}};
rx_on({call, From}, {enable_rx, _Timeout}, Data) ->
    {keep_state, Data#{waiting => From}}; % Happens when W4R is enabled
rx_on({call, From}, {disable_rx}, #{regs := Regs, timer := TimerRef} = Data) ->
    erlang:cancel_timer(TimerRef),
    NewRegs = pmod_uwb_registers:update_reg(Regs, sys_ctrl, #{rxenab => ?DISABLED}),
    {next_state, idle, Data#{regs := NewRegs}, {reply, From, ok}};
rx_on(info, {frame, Frame}, #{timer := TimerRef, regs := Regs, network := NetworkNode} = Data) ->
    erlang:cancel_timer(TimerRef),
    ct:log("Received frame: ~p", [Frame]),
    NewRegs = handle_rx(Frame, NetworkNode, Regs),
    case Data#{regs := NewRegs} of
        #{waiting := From, regs:= #{sys_status := #{affrej := 0}}} ->
            #{rx_buffer := RawFrame} = pmod_uwb_registers:get_value(NewRegs, rx_buffer),
            Reply = {byte_size(RawFrame), RawFrame},
            NewData = maps:remove(waiting, Data),
            {next_state, idle, NewData#{regs => NewRegs}, {reply, From, Reply}};
        #{waiting := From, regs:= #{sys_status := #{affrej := 1}}} ->
            NewData = maps:remove(waiting, Data),
            {next_state, idle, NewData#{regs => NewRegs}, {reply, From, affrej}};
        _ ->
            {next_state, idle_rx, Data#{regs => NewRegs}}
    end;
rx_on(info, {timeout, _, rx_timeout}, #{regs := Regs} = Data) ->
    NewRegs = handle_timeout(Regs),
    case Data of
        #{waiting := From} ->
            NewData = maps:remove(waiting, Data),
            {next_state, idle, NewData#{regs => NewRegs}, {reply, From, timeout}};
        _ ->
            {next_state, idle_to, Data#{regs => NewRegs}}
    end;
rx_on(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

idle_rx(enter, _OldState, Data) ->
    {keep_state, Data};
idle_rx({call, From}, {enable_rx, _Timeout}, #{regs := Regs} = Data) ->
    case pmod_uwb_registers:get_value(Regs, sys_status) of
        #{affrej := 1} ->
            {next_state, idle, Data, {reply, From, affrej}};
        _ ->
            #{rx_buffer := RawFrame} = pmod_uwb_registers:get_value(Regs, rx_buffer),
            Reply = {byte_size(RawFrame), RawFrame},
            NewData = maps:remove(waiting, Data),
            {next_state, idle, NewData#{regs => Regs}, {reply, From, Reply}}
    end;
idle_rx(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

idle_to(enter, _OldState, Data) ->
    {keep_state, Data};
idle_to({call, From}, {enable_rx, _Timeout}, Data) ->
    {next_state, idle, Data, {reply, From, timeout}}.

handle_event({call, From}, {read, Reg}, #{regs := Regs} = Data) ->
    Val = pmod_uwb_registers:get_value(Regs, Reg),
    {keep_state, Data, {reply, From, Val}};
handle_event({call, From}, {write, Reg, Value}, #{regs := Regs} = Data) ->
    NewRegs = pmod_uwb_registers:update_reg(Regs, Reg, Value),
    {keep_state, Data#{regs => NewRegs}, {reply, From, ok}};
handle_event({call, From}, {get_conf}, #{conf := Conf} = Data) ->
    {keep_state, Data, {reply, From, Conf}};
handle_event(info, Event, Data) ->
    ct:log("Event skipped: ~p", [Event]),
    {keep_state, Data};
handle_event(EventType, EventContent, _Data) ->
    error({unknown_event, EventType, EventContent}).

terminate(Reason, _) ->
    io:format("Terminate: ~w", [Reason]).

%--- Internal -------------------------------------------------------------------

tx(Frame, Options, NetworkNode, Regs) ->
    Rng = Options#tx_opts.ranging,
    PhyFrame = {Rng, Frame},
    ct:log("Tx frame ~p", [Frame]),
    {network_loop, NetworkNode} ! {tx, node(), PhyFrame},
    pmod_uwb_registers:update_reg(Regs, tx_fctrl, #{tr => Rng}).

enable_rx(Regs) ->
    NewRegs = pmod_uwb_registers:update_reg(Regs, sys_ctrl, #{rxenab => 1}),
    pmod_uwb_registers:update_reg(NewRegs, sys_status, #{rxfcg => 0, affrej => 0}).

handle_rx({_, <<_:5/bitstring, ?FTYPE_ACK:3, _/bitstring>>=RawFrame}, _, Regs) ->
    ct:log("Received Ack"),
    NewRegs1 = pmod_uwb_registers:update_reg(Regs, sys_cfg, #{rxenab => ?DISABLED}),
    pmod_uwb_registers:update_reg(NewRegs1, rx_buffer, #{rx_buffer => RawFrame});
handle_rx(Frame, NetworkNode, #{sys_cfg := #{ffen := ?ENABLED}} = Regs) ->
    {Rng, RawFrame} = Frame,
    #{short_addr := ShortAddress} = pmod_uwb_registers:get_value(Regs, panadr),
    #{eui := ExtAddress} = pmod_uwb_registers:get_value(Regs, eui),
    case check_address(RawFrame, ShortAddress, ExtAddress) of
        ok ->
            AckRegs = ack_reply(RawFrame, NetworkNode, Regs),
            NewRegs = pmod_uwb_registers:update_reg(AckRegs, rx_finfo, #{rng => Rng}),
            NewRegs1 = pmod_uwb_registers:update_reg(NewRegs, sys_cfg, #{rxenab => ?DISABLED}),
            pmod_uwb_registers:update_reg(NewRegs1, rx_buffer, #{rx_buffer => RawFrame});
        _ ->
            NewRegs = pmod_uwb_registers:update_reg(Regs, sys_cfg, #{rxenab => ?DISABLED}),
            pmod_uwb_registers:update_reg(NewRegs, sys_status, #{affrej => 1})
    end;
handle_rx(Frame, _, Regs) ->
    {Rng, RawFrame} = Frame,
    NewRegs = pmod_uwb_registers:update_reg(Regs, rx_finfo, #{rng => Rng}),
    NewRegs1 = pmod_uwb_registers:update_reg(NewRegs, sys_cfg, #{rxenab => ?DISABLED}),
    pmod_uwb_registers:update_reg(NewRegs1, rx_buffer, #{rx_buffer => RawFrame}).

handle_timeout(Regs) ->
    pmod_uwb_registers:update_reg(Regs, sys_status, #{rxenab => 0}).

check_address(Frame, ShortAddress, ExtAddress) -> % This will need to check the PAN and accept broadcast address at some point
    {_, MacHeader, _} = mac_frame:decode(Frame),
    case MacHeader#mac_header.dest_addr of
        ShortAddress -> ok;
        ExtAddress -> ok;
        _ -> continue 
    end.

ack_reply(_, _, #{sys_cfg := #{autoack := 0}} = Regs) ->
    Regs;
ack_reply(Frame, NetworkNode, Regs) ->
    <<_:2, _ACKREQ:1, _/bitstring>> = Frame,
    % io:format("Ack req: ~w ~n ~w", [ACKREQ, Frame]),
    case Frame of
        <<?FTYPE_ACK:2, _/bitstring>> ->
            ok;
        <<_:2, ?ENABLED:1, _:13, Seqnum:8, _/bitstring>> ->
            % io:format("Ack requested~n"),
            Ack = mac_frame:encode_ack(?DISABLED, Seqnum),
            tx(Ack, #tx_opts{}, NetworkNode, Regs);
        _ ->
            % io:format("No Ack requested~n"),
            Regs
    end.