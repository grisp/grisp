-module(onewire_ds2408).

%% Device: DS2408 - 1-Wire Slave
%% specification can be found at maximintegrated.com


-export([write_channel/2, read_channel/2]).

write_channel(Sel, <<Byte:8>>) ->
    init_select(Sel),
    grisp_onewire:write_byte(16#5a),
    grisp_onewire:write_byte(Byte),
    grisp_onewire:write_byte(bnot Byte),
    case grisp_onewire:read_byte() of
	<<16#aa>> ->
	    grisp_onewire:read_byte();
	<<16#ff>> ->
	    {error, write_channel_fail}
    end.


read_channel(Sel, Count) ->
    init_select(Sel),
    grisp_onewire:write_byte(16#f5),
    << (grisp_onewire:read_byte()) || _ <- lists:seq(1,Count) >>.

init_select(skip_rom) ->
    presence_detected = grisp_onewire:bus_reset(),
    grisp_onewire:write_byte(16#cc);
init_select(Id) when is_list(Id) ->
    presence_detected = grisp_onewire:bus_reset(),
    grisp_onewire:write_byte(16#55),
    [ grisp_onewire:write_byte(X) || X <- Id ].

    
    
    
