-module(grisp_bitmap).

% API
-export([set_bits/3]).
-export([get_bits/3]).
-export([set_bytes/3]).
-export([get_bytes/3]).

%--- API -----------------------------------------------------------------------

set_bits(Bin, Start, Value) when bit_size(Bin) >= Start + bit_size(Value) ->
    Len = bit_size(Value),
    <<Prefix:Start/bitstring, _:Len/bitstring, Postfix/bitstring>> = Bin,
    <<Prefix/bitstring, Value/bitstring, Postfix/bitstring>>.

get_bits(Bin, Start, Len) ->
    <<_:Start/bitstring, Bytes:Len/bitstring, _/bitstring>> = Bin,
    Bytes.

set_bytes(Bin, Start, Value) when is_binary(Value) ->
    set_bits(Bin, Start * 8, Value).

get_bytes(Bin, Start, Len) ->
    get_bits(Bin, Start * 8, Len * 8).
