-module(onewire_ds2408).
% Device: DS2408 - 1-Wire Slave
% https://datasheets.maximintegrated.com/en/ds/DS2408.pdf

% API
-export([write_channel/2, read_channel/2]).

%--- API -----------------------------------------------------------------------

write_channel(Sel, <<Byte:8>>) ->
    grisp_onewire:transaction(fun() ->
        init_select(Sel),
        grisp_onewire:write_byte(16#5a),
        grisp_onewire:write_byte(Byte),
        grisp_onewire:write_byte(bnot Byte),
        case grisp_onewire:read_byte() of
            <<16#aa>> ->
                grisp_onewire:read_byte();
            <<16#ff>> ->
                {error, write_channel_fail}
        end
    end).

read_channel(Sel, Count) ->
    grisp_onewire:transaction(fun() ->
        init_select(Sel),
        grisp_onewire:write_byte(16#f5),
        << (grisp_onewire:read_byte()) || _ <- lists:seq(1,Count) >>
    end).

%--- Internal ------------------------------------------------------------------

init_select(skip_rom) ->
    presence_detected = grisp_onewire:bus_reset(),
    grisp_onewire:write_byte(16#cc);
init_select(Id) when is_list(Id) ->
    presence_detected = grisp_onewire:bus_reset(),
    grisp_onewire:write_byte(16#55),
    [ grisp_onewire:write_byte(X) || X <- Id ].
