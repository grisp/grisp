-module(grisp_onewire).

%% Chip: DS2482 - 1-Wire Master
%% specification can be found at maximintegrated.com

-include("grisp_i2c.hrl").

% API
-export([reset/0, write_config/1, detect/0, bus_reset/0,
	 write_byte/1, write_triplet/1, read_byte/0, search/0]).

-define(ds2482_i2c_adr, 16#18).
-define(cmd_drst, 16#f0).
-define(cmd_wcfg, 16#d2).
-define(cmd_1wrs, 16#b4).
-define(cmd_1wwb, 16#a5).
-define(cmd_1wrb, 16#96).
-define(cmd_1wt,  16#78).
-define(cmd_srp,  16#e1).

-define(poll_interval, 10).
-define(poll_timeout, 1000).

%--- API -----------------------------------------------------------------------


%% Spec: | S | AD,0 ‖ A ‖ DRST ‖ A ‖ Sr | AD,1 ‖ A | <byte> ‖ A\ | P |
reset() ->
    <<Status:8>> = grisp_i2c:msgs([?ds2482_i2c_adr, 
				   {write, <<?cmd_drst>>}, 
				   {read, 1, ?I2C_M_NO_RD_ACK}]),
    case Status band 16#f7 of
	16#10 -> ok;
	Any -> error({invalid_status, Any})
    end.

%% Spec: | S | AD,0 ‖ A ‖ WCFG ‖ A ‖ <byte> ‖ A ‖ Sr | AD,1 ‖ A | <byte> ‖ A\ | P |
write_config(Conf) when is_list(Conf) ->
    write_config(lists:foldl(fun(X, A) -> A bor map_config(X) end, 0, Conf));
write_config(Conf) when is_integer(Conf) ->
    Val = (bnot(Conf) bsl 4) bor Conf,
    case grisp_i2c:msgs([?ds2482_i2c_adr, 
			 {write, <<?cmd_wcfg, Val>>},
			 {read, 1, ?I2C_M_NO_RD_ACK}]) of
	<<Conf:8>> ->
	    ok;
	Any -> error({read_back_config, Any, Val})
    end.

map_config(apu) ->
    1;
map_config(spu) ->
    4;
map_config(overdrive) ->
    8.

detect() ->
    reset(),
    write_config([apu]).

bus_reset() ->
    grisp_i2c:msgs([?ds2482_i2c_adr, {write, <<?cmd_1wrs>>}]),
    timer:sleep(1),
    check_status(grisp_i2c:msgs([?ds2482_i2c_adr, 
				 {read, 1, ?I2C_M_NO_RD_ACK}])).

check_status(<<_:5, Sd:1, Ppd:1, 0:1>>) ->
    case {Sd, Ppd} of
	{0, 0} -> nothing_present;
	{0, 1} -> presence_detected;
	{1, _} -> short_detected
    end;
check_status(<<_:7, 1:1>>) ->
    error(bus_reset_busy).

write_byte(Byte) ->
    grisp_i2c:msgs([?ds2482_i2c_adr, {write, <<?cmd_1wwb, Byte>>}]),
    timer:sleep(1).

read_byte() ->
    grisp_i2c:msgs([?ds2482_i2c_adr, 
		    {write, <<?cmd_1wrb>>}]),
    timer:sleep(1),
    grisp_i2c:msgs([?ds2482_i2c_adr, 
		    {write, <<?cmd_srp, 16#e1>>},
		    {read, 1, ?I2C_M_NO_RD_ACK}]).

write_triplet(Dir) ->
    grisp_i2c:msgs([?ds2482_i2c_adr, {write, <<?cmd_1wt, Dir>>}]),
    timer:sleep(1),
    <<D:1, T:1, S:1, _:5>> = grisp_i2c:msgs([?ds2482_i2c_adr, 
					     {read, 1, ?I2C_M_NO_RD_ACK}]),
    {D, T, S}.
	
search() ->    
    bus_reset(),
    write_byte(16#f0),
    L = lists:reverse([ element(1, write_triplet(0)) || _ <- lists:seq(1,64) ]),
    lists:reverse([ Y || <<Y:8>> <= << <<X:1>> || X <- L >> ]).


    

%%% For this to work we need a way to not sending a stop after a msg chain

%% bus_reset_busy_polling() ->
%%     grisp_i2c:msgs([?ds2482_i2c_adr, {write, <<?cmd_1wrs>>}]),
%%     poll_busy(?poll_timeout).

%% poll_busy(Timeout) when Timeout > 0 ->
%%     case grisp_i2c:msgs([?ds2482_i2c_adr, {read, 1, ?I2C_M_NOSTART}]) of
%% 	<<_:7, 1:1>> ->
%% 	    timer:sleep(?poll_interval),
%% 	    poll_busy(Timeout - ?poll_interval);
%% 	<<_:7, 0:1>> -> 
%% check_status(grisp_i2c:msgs([?ds2482_i2c_adr, 
%% 				 {read, 1, 
%% 				  ?I2C_M_NOSTART bor ?I2C_M_NO_RD_ACK}]))
%%     end.
		  
	    
