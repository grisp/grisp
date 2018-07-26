-module(grisp_bitmap).

% API
-export([set_bits/3]).
-export([get_bits/3]).
-export([set_bytes/3]).
-export([get_bytes/3]).
-export([pp/1]).
-export([pp/2]).

%--- API -----------------------------------------------------------------------

set_bits(Bin, Start, Value) when bit_size(Bin) >= Start + bit_size(Value) ->
    Len = bit_size(Value),
    <<Prefix:Start/bitstring, _:Len/bitstring, Postfix/bitstring>> = Bin,
    <<Prefix/bitstring, Value/bitstring, Postfix/bitstring>>.

get_bits(Bin, Start, Len) when bit_size(Bin) >= Start + Len ->
    <<_:Start/bitstring, Bytes:Len/bitstring, _/bitstring>> = Bin,
    Bytes.

set_bytes(Bin, Start, Value) when byte_size(Bin) >= Start + byte_size(Value) ->
    set_bits(Bin, Start * 8, Value).

get_bytes(Bin, Start, Len) when byte_size(Bin) >= Start + Len ->
    get_bits(Bin, Start * 8, Len * 8).

pp(Bin) -> pp(Bin, #{display => hex}).

pp(Bin, Display) when is_atom(Display) ->
    pp(Bin, #{display => Display});
pp(<<B:8/bitstring, Rest/bitstring>>, Opts) ->
    print_byte(B, Opts),
    pp(Rest, Opts);
pp(<<>>, _Opts) ->
    io:format("~n").

%--- Internal ------------------------------------------------------------------

print_byte(<<Byte>>, #{display := D}) when D == hex; D == hexadecimal ->
    io:format("~2.16.0B ", [Byte]);
print_byte(<<Byte>>, #{display := D}) when D == bin; D == binary ->
    io:format("~8.2.0B ", [Byte]);
print_byte(<<N1:4, N2:4>>, #{display := D}) when D == nib; D == nibble ->
    io:format("~4.2.0B ~4.2.0B  ", [N1, N2]);
print_byte(<<Byte>>, #{display := D}) when D == dec; D == decimal->
    io:format("~3.10.0B ", [Byte]);
print_byte(_Byte, Opts) ->
    case maps:find(display, Opts) of
        {ok, Value} -> error({invalid_option, display, Value});
        error       -> error({missing_option, display})
    end.
