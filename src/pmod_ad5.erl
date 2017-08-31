-module(pmod_ad5).
-compile([export_all]).

-behavior(gen_server).

-include("grisp.hrl").
-include("pmod_ad5.hrl").

% API
-export([start_link/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(SPI_MODE, #{cpol => high, cpha => trailing}).
-define(wait_ready_timeout, 1000).
-define(wait_ready_poll, 10).

%--- Records -------------------------------------------------------------------

-record(state, {
    slot
}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot) -> gen_server:start_link(?MODULE, Slot, []).


%--- Callbacks -----------------------------------------------------------------

% @private
init(Slot) ->
    verify_device(Slot),
    grisp_devices:register(Slot, ?MODULE),
    {ok, #state{slot = Slot}}.

% @private
handle_call(tbd, _From, State) ->
    {reply, not_implemented, State}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

verify_device(Slot) ->
    <<_:4, 2:4>> = read(Slot, 2#100, 1).


single(Slot, Config, Mode) ->
    {C, Cf} = config_flags(Config),
    {M, Mf} = mode_flags(Mode#{md => 1}),
    N = count_channels(Cf),
    Val_size = val_size(Mf),
    write_config(Slot, C),
    get_values(Slot, M, Val_size, N).

get_values(_Slot, _M, _Val_size, 0) ->
    [];
get_values(Slot, M, Val_size, N) when N > 0 ->
    write_mode(Slot, M),
    ok = wait_ready(Slot, ?wait_ready_timeout),
    [ read(Slot, 3, Val_size) | get_values(Slot, M, Val_size, N-1) ].
    

wait_ready(_Slot, Timeout) when Timeout =< 0 ->
    {error, timeout};
wait_ready(Slot, Timeout) ->
    case read(Slot, 0, 1) of
	<<1:1, _:7>> ->
	    timer:sleep(?wait_ready_poll),
	    wait_ready(Slot, Timeout-?wait_ready_poll);
	<<0:1, _:7>> ->
	    ok
    end.
    
    
count_channels(Config) ->
    lists:sum(maps:values(maps:with([short, temp, ch7, ch6, ch5, 
				     ch4, ch3, ch2, ch1, ch0], Config))).

val_size(#{dat_sta := 0}) -> 3;
val_size(#{dat_sta := 1}) -> 4.
    
write_mode(Slot, <<_:24>>=Flags) ->
    <<>> = write(Slot, 1, Flags);
write_mode(Slot, Flags) ->
    {F, _} = mode_flags(Flags),
    write_mode(Slot, F).

mode_flags(Flags) ->
    Default = #{0 => 0, 1 => 1,
		md => 0, dat_sta => 0, clk => 2, avg => 0, 
		sinc3 => 0, enpar => 0, clk_div => 0, single => 0, 
		rej60 => 0, fs => 16#60},
    Bits = [{md, 3}, dat_sta, {clk, 2}, {avg, 2}, 
	    sinc3, 0, enpar, clk_div, single, rej60, {fs, 10}],
    calc_flags(Flags, Default, Bits).

write_config(Slot, <<_:24>>=Flags) ->
    <<>> = write(Slot, 2, Flags);
write_config(Slot, Flags) ->
    {F, _} = config_flags(Flags),
    write_config(Slot, F).

config_flags(Flags) ->
    Default = #{0 => 0, 1 => 1, 
		chop => 0, refsel => 0, pseudo => 0,
		short => 0, temp => 0, 
		ch7 => 0, ch6 => 0, ch5 => 0, ch4 => 0, 
		ch3 => 0, ch2 => 0, ch1 => 0, ch0 => 0, 
		burn => 0, refdet => 0, buf => 1, unb => 0, 
		gain => 7},
    Bits = [chop, 0, 0, refsel, 0, pseudo, short, temp, 
	    ch7, ch6, ch5, ch4, ch3, ch2, ch1, ch0, 
	    burn, refdet, 0, buf, unb, {gain, 3}],
    calc_flags(Flags, Default, Bits).

calc_flags(Flags, Default, Bits) ->
    F = maps:merge(Default, Flags),
    {<< <<(flag_chunk(B, F))/bits>> || B <- Bits>>, F}.

flag_chunk({B, S}, F) ->
    <<(maps:get(B, F)):S>>;
flag_chunk(B, F) ->
    <<(maps:get(B, F)):1>>.

read(Slot, Reg, Size) ->
    grisp_spi:send_recv(Slot, ?SPI_MODE, <<0:1, 1:1, Reg:3, 0:1, 0:2>>, 
			1, Size).

write(Slot, Reg, Data) ->
    grisp_spi:send_recv(Slot, ?SPI_MODE, 
			<<0:1, 0:1, Reg:3, 0:1, 0:2, Data/binary>>, 
			byte_size(Data)+1, 0).
    

