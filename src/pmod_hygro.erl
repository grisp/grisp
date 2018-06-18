-module(pmod_hygro).

% API
-export([temp/0, humid/0, measurements/0]).

-define(DEVICE_ADR,    16#40).
-define(TEMP_REGISTER, 16#00).
-define(DELAY_TIME, 15).

%--- API -----------------------------------------------------------------------

temp() ->
    {ok, <<T:14/unsigned-big, _:2>>} = device_request(2),
    Temp = evaluate_temp(T),
    [{temp, Temp}].

humid() ->
    {ok, <<_:14, _:2, H:14/unsigned-big, _:2>>} = device_request(4),
    Humid = evaluate_humid(H),
    [{humid, Humid}].

measurements() ->
    {ok, <<T:14/unsigned-big, _:2, H:14/unsigned-big, _:2>>} = device_request(4),
    Temp = evaluate_temp(T),
    Humid = evaluate_humid(H),
    [{temp, Temp}, {humid, Humid}].

%--- Internal ------------------------------------------------------------------

device_request(BytesToRead) ->
    Response = grisp_i2c:msgs([?DEVICE_ADR, {write, <<?TEMP_REGISTER>>},
                               {sleep, ?DELAY_TIME},
                               {read, BytesToRead, ?TEMP_REGISTER}]),
    {ok, Response}.

evaluate_temp(T) ->
    (T / 16384) * 165 - 40.

evaluate_humid(H) ->
    (H / 16384) * 100.
