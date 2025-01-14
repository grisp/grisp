-module(grisp_bitmap).
-moduledoc """
Bitstring editing and printing.
""".


% API
-export([set_bits/3]).
-export([get_bits/3]).
-export([set_bytes/3]).
-export([get_bytes/3]).
-export([pp/1]).
-export([pp/2]).

%--- Types ---------------------------------------------------------------------

-type coding() :: bin | binary | dec | decimal | hex | hexadecimal
                  | nib | nibble.

%--- API -----------------------------------------------------------------------

-doc """
Replace a part of a bitsting.

### Example
```
5> grisp_bitmap:pp(grisp_bitmap:set_bits(<<2#00000000>>, 6, <<2#1:1>>), bin).
00000010
ok
```
""".
-spec set_bits(bitstring(),non_neg_integer(),bitstring()) -> bitstring().
set_bits(Bin, Start, Value) when bit_size(Bin) >= Start + bit_size(Value) ->
    Len = bit_size(Value),
    <<Prefix:Start/bitstring, _:Len/bitstring, Postfix/bitstring>> = Bin,
    <<Prefix/bitstring, Value/bitstring, Postfix/bitstring>>.

-doc """
Get a part of a bitstring.

### Example
```
 1> grisp_bitmap:get_bits(<<1,2,3,4,5>>, 6, 2).
 <<1:2>>
```
""".
-spec get_bits(bitstring(),non_neg_integer(),non_neg_integer()) -> bitstring().
get_bits(Bin, Start, Len) when bit_size(Bin) >= Start + Len ->
    <<_:Start/bitstring, Bytes:Len/bitstring, _/bitstring>> = Bin,
    Bytes.

-doc #{equiv => set_bits(Bin, Start * 8, Value)}.
-spec set_bytes(binary(),non_neg_integer(),bitstring()) -> binary().
set_bytes(Bin, Start, Value) when byte_size(Bin) >= Start + byte_size(Value) ->
    set_bits(Bin, Start * 8, Value).

-doc #{equiv => get_bits(Bin, Start * 8, Len * 8)}.
-spec get_bytes(binary(),non_neg_integer(),non_neg_integer()) -> binary().
get_bytes(Bin, Start, Len) when byte_size(Bin) >= Start + Len ->
    get_bits(Bin, Start * 8, Len * 8).

-doc """
Print binary as hexadecimal numbers.

### Example
```
 2> grisp_bitmap:pp(<<16#f2, 17>>).
 F2 11
 ok
```
""".
-spec pp(bitstring()) -> ok.
pp(Bin) -> pp(Bin, #{}).

-doc """
Print binary as numbers.

### Example
```
 3> grisp_bitmap:pp(<<16#f2, 17>>, nib).
 1111 0010  0001 0001
 ok
 4> grisp_bitmap:pp(<<16#f2, 17>>, #{display => bin}).
 1111 0010  0001 0001
 ok
```
""".
-spec pp(bitstring(), coding() | #{display => coding()}) -> ok.
pp(Bin, Display) when is_atom(Display) ->
    pp(Bin, #{display => Display});
pp(Bin, Opts) ->
    print_bitstring(Bin, normalize_opts(maps:merge(#{display => hex}, Opts))).

%--- Internal ------------------------------------------------------------------

normalize_opts(Opts) -> maps:map(fun normalize_opt/2, Opts).

normalize_opt(display, hexadecimal) -> hex;
normalize_opt(display, binary) -> bin;
normalize_opt(display, nibble) -> nib;
normalize_opt(display, decimal) -> dec;
normalize_opt(display, hex) -> hex;
normalize_opt(display, bin) -> bin;
normalize_opt(display, nib) -> nib;
normalize_opt(display, dec) -> dec;
normalize_opt(display, Other) -> error({invalid_option, display, Other});
normalize_opt(_Opt, Value) -> Value.

print_bitstring(<<B:8/bitstring, Rest/bitstring>>, Opts) ->
    print_bits(B, Opts),
    io:format(" "),
    print_bitstring(Rest, Opts);
print_bitstring(<<Rest/bitstring>>, Opts) ->
    print_bits(Rest, Opts),
    io:format("~n").

print_bits(Bits, Opts) ->
    Size = bit_size(Bits),
    print_bits(Size, Bits, Opts).

print_bits(Size, Bits, #{display := hex}) ->
    <<Int:Size>> = Bits,
    io:format("~*.16.0B", [Size div 4, Int]);
print_bits(Size, Bits, #{display := bin}) ->
    <<Int:Size>> = Bits,
    io:format("~*.2.0B", [Size, Int]);
print_bits(_Size, <<N1:4, N2:4>>, #{display := nib}) ->
    io:format("~4.2.0B ~4.2.0B ", [N1, N2]);
print_bits(_Size, <<Byte>>, #{display := dec}) ->
    io:format("~3.10.0B", [Byte]);
print_bits(0, <<>>, _Opts) ->
    ok;
print_bits(_Size, _Byte, _Opts) ->
    io:format("foo").
