% @doc Bitstring editing and printing.
% @end
-module(grisp_bitmap).

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

% @doc Replace a part of a bitsting.
%
% === Example ===
% ```
%  5> grisp_bitmap:set_bits(<<1,2,3,4,5>>, 6, <<2:2>>).
%  <<2,2,3,4,5>>
% '''
-spec set_bits(bitstring(),non_neg_integer(),bitstring()) -> bitstring().
set_bits(Bin, Start, Value) when bit_size(Bin) >= Start + bit_size(Value) ->
    Len = bit_size(Value),
    <<Prefix:Start/bitstring, _:Len/bitstring, Postfix/bitstring>> = Bin,
    <<Prefix/bitstring, Value/bitstring, Postfix/bitstring>>.

% @doc Get a part of a bitstring.
%
% === Example ===
% ```
%  1> grisp_bitmap:get_bits(<<1,2,3,4,5>>, 6, 2).
%  <<1:2>>
% '''
-spec get_bits(bitstring(),non_neg_integer(),non_neg_integer()) -> bitstring().
get_bits(Bin, Start, Len) when bit_size(Bin) >= Start + Len ->
    <<_:Start/bitstring, Bytes:Len/bitstring, _/bitstring>> = Bin,
    Bytes.

% @equiv set_bits(Bin, Start * 8, Value)
-spec set_bytes(bitstring(),non_neg_integer(),bitstring()) -> bitstring().
set_bytes(Bin, Start, Value) when byte_size(Bin) >= Start + byte_size(Value) ->
    set_bits(Bin, Start * 8, Value).

% @equiv get_bits(Bin, Start * 8, Len * 8)
-spec get_bytes(bitstring(),non_neg_integer(),non_neg_integer()) -> bitstring().
get_bytes(Bin, Start, Len) when byte_size(Bin) >= Start + Len ->
    get_bits(Bin, Start * 8, Len * 8).

% @doc Print bitstring as hexadecimal numbers.
%
% === Example ===
% ```
%  2> grisp_bitmap:pp(<<16#f2, 17>>).
%  F2 11
%  ok
% '''
-spec pp(bitstring()) -> 'ok'.
pp(Bin) -> pp(Bin, #{display => hex}).

% @doc Print bitstring as numbers.
%
% === Example ===
% ```
%  3> grisp_bitmap:pp(<<16#f2, 17>>, nib).
%  1111 0010  0001 0001
%  ok
%  4> grisp_bitmap:pp(<<16#f2, 17>>, #{display => bin}).
%  1111 0010  0001 0001
%  ok
% '''
-spec pp(bitstring(), coding() | #{display := coding()}) -> 'ok'.
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
