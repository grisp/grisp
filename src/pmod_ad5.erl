-module(pmod_ad5).

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
    BinMode = bin_mode(Mode#{md => 1}),
    ChanCount = count_channels(Config),
    ValSize = value_size(Mode),
    case call({single, BinConfig, BinMode, ValSize, ChanCount}) of
        {error, Reason} -> error(Reason);
        Result          -> Result
    end.

bin_config(Config) -> calc_flags(Config, ?CONFIG_BITS).
bin_mode(Mode) -> calc_flags(Mode, ?MODE_BITS).
    
%--- Callbacks -----------------------------------------------------------------

% @private
init(Slot) ->
    State = try
		configure_pins(Slot),
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
handle_call({single, BinConfig, BinMode, ValSize, ChanCount}, _From, 
	    #state{slot=Slot}=State) ->
    reset_communication(Slot),
    write(State#state.slot, ?CONFIGURATION, BinConfig),
    Reply = try
        get_values(State#state.slot, BinMode, ValSize, ChanCount+1)
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

value_size(#{dat_sta := ?DAT_STA_ENABLE})  -> 4;
value_size(_)                              -> 3.

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
