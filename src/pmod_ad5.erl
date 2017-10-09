-module(pmod_ad5).
-compile([export_all]).

-behavior(gen_server).

-include("grisp.hrl").
-include("pmod_ad5.hrl").

% API
-export([start_link/1]).
-export([single/2]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(SPI_MODE, #{cpol => high, cpha => trailing}).
-define(WAIT_READY_TIMEOUT, 1000).
-define(WAIT_READY_POLL, 10).

%--- Records -------------------------------------------------------------------

-record(state, {
    slot
}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot) -> gen_server:start_link(?MODULE, Slot, []).

single(Config, Mode) ->
    BinConfig = bin_config(Config), 
    %% since the last bit of a measurement can't be read reliably we
    %% allways append the status and ignore its last bit
    BinMode = bin_mode(Mode#{md => 1, dat_sta => 1}),
    case count_channels(Config) of
	1 -> ok;
	C -> error({only_one_channel_in_single, C})
    end,
    call({single, BinConfig, BinMode, maps:get(enpar,Mode,0)}).

bin_config(Config) -> calc_flags(Config, ?CONFIG_BITS).
bin_mode(Mode) -> calc_flags(Mode, ?MODE_BITS).
    
%--- Callbacks -----------------------------------------------------------------

% @private
init(Slot) ->
    State = try
		configure_pins(Slot),
		reset_communication(Slot),
		timer:sleep(500),
		verify_device(Slot),
		#state{slot = Slot}
	    catch
		Class:Reason ->
		    restore_pins(Slot),
		    erlang:raise(Class, Reason, erlang:get_stacktrace())
	    end,
    grisp_devices:register(Slot, ?MODULE),
    {ok, State}.

% @private
handle_call({single, BinConfig, BinMode, Check_parity}, _From, 
	    #state{slot=Slot}=State) ->
    write(State#state.slot, ?CONFIGURATION, BinConfig),
    Reply = try
		get_values(State#state.slot, BinMode, 4, 1)
	    of 
		[<<_:24, _Rdy:1, _Err:1, 1:1, _Parity:1, _:4>>] -> 
		    {error, no_reference_voltage};
		[<<0:24, _Rdy:1, 1:1, _Noref:1, _Parity:1, _:4>>] -> 
		    {error, underrange};
		[<<16#ffffff:24, _Rdy:1, 1:1, _Noref:1, _Parity:1, _:4>>] -> 
		    {error, overrange};
		[<<Val:24, _Rdy:1, 0:1, 0:1, Parity:1, _:4>>] 
		  when Check_parity =:= 1 -> 
		    check_parity(Val, Parity);
		[<<Val:24, _Rdy:1, 0:1, 0:1, _Parity:1, _:4>>] -> 
		    {ok, Val}
	    catch
		{error, _} = Error -> Error
	    end,
    {reply, Reply, State}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, State) -> restore_pins(State#state.slot).

%--- Internal ------------------------------------------------------------------

call(Call) ->
    Dev = grisp_devices:default(?MODULE),
    gen_server:call(Dev#device.pid, Call).

configure_pins(Slot) ->
    % Disable automatic chip select (gets configured to output_1)
    grisp_gpio:configure_slot(Slot, disable_cs).

restore_pins(Slot) ->
    grisp_gpio:configure_slot(Slot, enable_cs).


verify_device(Slot) ->
    case read(Slot, ?ID, 1) of
        <<_:4, ?DEVID:4>> -> ok;
        Other             -> error({device_mismatch, {id, Other}})
    end.

check_parity(X, Parity) ->
    Y1 = X bxor (X bsr 1),
    Y2 = Y1 bxor (Y1 bsr 2),
    Y3 = Y2 bxor (Y2 bsr 4),
    Y4 = Y3 bxor (Y3 bsr 8),
    case (Y4 bxor (Y4 bsr 16)) band 1 of
	Parity -> {ok, X};
	_ -> {error, parity_check}
    end.

get_values(Slot, BinMode, ValSize, N) ->
    try
        grisp_gpio:clear(chip_select(Slot)),
	write_no_cs(Slot, ?MODE, BinMode),
	get_values1(Slot, BinMode, ValSize, N)
    after
        grisp_gpio:set(chip_select(Slot))
    end.

get_values1(_Slot, _M, _ValSize, 0) ->
    [];
get_values1(Slot, BinMode, ValSize, N) when N > 0 ->
    % TODO: Return timeout error to caller instead of crashing gen_server
    ok = wait_ready(Slot, ?WAIT_READY_TIMEOUT),
    [read_no_cs(Slot, ?DATA, ValSize) 
     | get_values(Slot, BinMode, ValSize, N - 1)].

count_channels(Config) ->
    lists:sum(maps:values(maps:with(?CONFIG_CHANNELS, Config))).

% Common

wait_ready(_Slot, Timeout) when Timeout =< 0 ->
    throw({error, timeout});
wait_ready(Slot, Timeout) ->
    case read_no_cs(Slot, ?STATUS, 1) of
        <<?RDY_WAIT:1, _:7>> ->
            timer:sleep(?WAIT_READY_POLL),
            wait_ready(Slot, Timeout - ?WAIT_READY_POLL);
        <<?RDY_READY:1, _:7>> ->
            ok
    end.

%% currently not needed but laters
%% value_size(#{dat_sta := ?DAT_STA_ENABLE})  -> 4;
%% value_size(_)                              -> 3.

calc_flags(Flags, Bits) ->
    << <<(flag(F, Flags, Default)):Size>> || {F, Size, Default} <- Bits >>.

flag(0, _Flags, _Default)  -> 0;
flag(Flag, Flags, Default) -> maps:get(Flag, Flags, Default).

reset_communication(Slot) -> 
    grisp_gpio:clear(chip_select(Slot)),
    <<>> = grisp_spi:send_recv(Slot, ?SPI_MODE, <<16#ffffffffffff:48>>, 6, 0),
    grisp_gpio:set(chip_select(Slot)).


read(Slot, Reg, Size) -> 
    grisp_gpio:clear(chip_select(Slot)),
    Res = read_no_cs(Slot, Reg, Size),
    grisp_gpio:set(chip_select(Slot)),
    Res.

read_no_cs(Slot, Reg, Size) -> 
    send_recv(Slot, ?RW_READ, Reg, <<>>, Size).

write(Slot, Reg, Data) -> 
    grisp_gpio:clear(chip_select(Slot)),
    Res = write_no_cs(Slot, Reg, Data),
    grisp_gpio:set(chip_select(Slot)),
    Res.

write_no_cs(Slot, Reg, Data) -> 
    send_recv(Slot, ?RW_WRITE, Reg, Data, 0).

send_recv(Slot, RW, Reg, Data, Size) ->
    Req = req(RW, Reg, Data),
    grisp_spi:send_recv(Slot, ?SPI_MODE, Req, byte_size(Data) + 1, Size).

req(RW, Reg, Data) ->
    <<?WRITE_ENABLE:1, RW:1, Reg:3, ?CONT_READ_DISABLE:1, 0:2, Data/binary>>.

chip_select(spi1) ->
    ss1;
chip_select(spi2) ->
    ss2.
