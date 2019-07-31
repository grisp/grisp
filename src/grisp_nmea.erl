-module(grisp_nmea).

%--- Exports -------------------------------------------------------------------

-export([parse/1]).


%--- API Functions -------------------------------------------------------------

parse(<<$$, Body/binary>>) ->
    case binary:split(Body, <<"*">>) of
        [Msg, HexCrc] -> nmea_parse_packet(Msg, HexCrc);
        _ -> {error, bad_format}
    end.


%--- Internal Functions --------------------------------------------------------

nmea_parse_packet(Body, Crc) ->
    try
        ExpectedCrc = nmea_crc(Body),
        case parse_hex(Crc) of
            ExpectedCrc ->
                case binary:split(Body, <<",">>) of
                    [Address, ValueData] ->
                        nmea_parse_address(Address, ValueData);
                    _ ->
                        {error, bad_format}
                end;
            _ ->
                {error, bad_crc}
        end
    catch
        bad_format ->
            {error, bad_format}
    end.

nmea_crc(Data) ->
    nmea_crc(Data, 0).

nmea_crc(<<>>, Acc) -> Acc;
nmea_crc(<<C:8, Rest/binary>>, Acc) ->
    nmea_crc(Rest, Acc bxor C).

nmea_split(Data, ExpectedSize) ->
    Fields = binary:split(Data, <<",">>, [global]),
    case length(Fields) =:= ExpectedSize of
        false -> erlang:throw(bad_format);
        true -> Fields
    end.

nmea_parse_address(<<T:2/binary, M/binary>>, ValueData) ->
    Talker = nmea_parse_talker_id(T),
    Type = nmea_parse_message_id(M),
    ParsedValues = nmea_parse_values(Talker, Type, ValueData),
    {ok, {Talker, Type, ParsedValues}}.

nmea_parse_values(_Talker, gga, Data) -> nmea_parse_gga(Data);
nmea_parse_values(_Talker, _Type, Data) -> Data.

nmea_parse_gga(Data) ->
    [T, Lat, NS, Long, EW, Q, _, _, Alt, AltU | _] = nmea_split(Data, 14),
    #{
        fixed => nmea_parse_gga_quality(Q),
        time => nmea_parse_time(T),
        lat => nmea_apply_direction(NS, nmea_parse_coordinate(Lat)),
        long => nmea_apply_direction(EW, nmea_parse_coordinate(Long)),
        alt => nmea_parse_altitude(Alt, AltU)
    }.

nmea_parse_time(<<>>) -> undefined;
nmea_parse_time(<<H:2/binary, M:2/binary, S:2/binary, $., X/binary>>) ->
    ((parse_dec(H, 2) * 60 + parse_dec(M, 2)) * 60 + parse_dec(S, 2)) * 1000 + parse_dec(X, 3);
nmea_parse_time(<<H:2/binary, M:2/binary, S:2/binary>>) ->
    ((parse_dec(H, 2) * 60 + parse_dec(M, 2)) * 60 + parse_dec(S, 2)) * 1000;
nmea_parse_time(_) ->
    erlang:throw(bad_format).

nmea_parse_coordinate(<<>>) -> undefined;
nmea_parse_coordinate(<<D:2/binary, M:2/binary, $., X/binary>>) ->
    ((parse_dec(D, 2) * 60 + parse_dec(M, 2)) * 1000000 + parse_dec(X, 6)) / (60 * 1000000);
nmea_parse_coordinate(<<D:3/binary, M:2/binary, $., X/binary>>) ->
    ((parse_dec(D, 3) * 60 + parse_dec(M, 2)) * 1000000 + parse_dec(X, 6)) / (60 * 1000000);
nmea_parse_coordinate(_) ->
    erlang:throw(bad_format).

nmea_parse_altitude(<<>>, _) -> undefined;
nmea_parse_altitude(Num, <<"M">>) ->
    parse_num(Num);
nmea_parse_altitude(_, _) ->
    erlang:throw(bad_format).

nmea_parse_gga_quality(<<>>)    -> false;
nmea_parse_gga_quality(<<"0">>) -> false;
nmea_parse_gga_quality(<<"1">>) -> true;
nmea_parse_gga_quality(<<"2">>) -> true;
nmea_parse_gga_quality(<<"4">>) -> true;
nmea_parse_gga_quality(<<"5">>) -> true;
nmea_parse_gga_quality(<<"6">>) -> false;
nmea_parse_gga_quality(_) ->
    erlang:throw(bad_format).

nmea_apply_direction(_, undefined) -> undefined;
nmea_apply_direction(<<"N">>, Coordinate) -> Coordinate;
nmea_apply_direction(<<"S">>, Coordinate) -> -Coordinate;
nmea_apply_direction(<<"E">>, Coordinate) -> Coordinate;
nmea_apply_direction(<<"W">>, Coordinate) -> -Coordinate;
nmea_apply_direction(_, _) ->
    erlang:throw(bad_format).

nmea_parse_talker_id(<<"GP">>) -> gps;
nmea_parse_talker_id(<<"GL">>) -> glonass;
nmea_parse_talker_id(<<"GA">>) -> galileo;
nmea_parse_talker_id(<<"GB">>) -> beidou;
nmea_parse_talker_id(<<"GN">>) -> any;
nmea_parse_talker_id(Other) when is_binary(Other) -> Other.

nmea_parse_message_id(<<"DTM">>) -> dtm;
nmea_parse_message_id(<<"GBQ">>) -> gbq;
nmea_parse_message_id(<<"GBS">>) -> gbs;
nmea_parse_message_id(<<"GGA">>) -> gga;
nmea_parse_message_id(<<"GLL">>) -> gll;
nmea_parse_message_id(<<"GLQ">>) -> glq;
nmea_parse_message_id(<<"GNQ">>) -> gnq;
nmea_parse_message_id(<<"GNS">>) -> gns;
nmea_parse_message_id(<<"GPQ">>) -> gpq;
nmea_parse_message_id(<<"GRS">>) -> grs;
nmea_parse_message_id(<<"GSA">>) -> gsa;
nmea_parse_message_id(<<"GST">>) -> gst;
nmea_parse_message_id(<<"GSV">>) -> gsv;
nmea_parse_message_id(<<"RMC">>) -> rmc;
nmea_parse_message_id(<<"TXT">>) -> txt;
nmea_parse_message_id(<<"VLW">>) -> vlw;
nmea_parse_message_id(<<"VTG">>) -> vtg;
nmea_parse_message_id(<<"ZDA">>) -> zda;
nmea_parse_message_id(Other) when is_binary(Other) -> Other.


%--- GENERIC PARSING FUNCTIONS -------------------------------------------------

parse_hex(<<>>) -> erlang:throw(bad_format);
parse_hex(Bin) -> parse_hex(Bin, 0).

parse_hex(<<>>, Acc) -> Acc;
parse_hex(<<V:8, Rest/binary>>, Acc) ->
    parse_hex(Rest, Acc * 16 + hex_to_int(V)).

parse_dec(<<>>, _Digits) -> erlang:throw(bad_format);
parse_dec(Bin, Digits) -> parse_dec(Bin, Digits, 0).

parse_dec(<<>>, 0, Acc) -> Acc;
parse_dec(<<>>, Digits, _Acc) when Digits < 0 ->
    erlang:throw(bad_format);
parse_dec(<<>>, Digits, Acc) ->
    parse_dec(<<>>, Digits - 1 , Acc * 10);
parse_dec(<<V:8, Rest/binary>>, Digits, Acc) ->
    parse_dec(Rest, Digits - 1, Acc * 10 + dec_to_int(V)).

parse_num(<<>>) -> erlang:throw(bad_format);
parse_num(<<"-", Rest/binary>>) -> -parse_num(Rest);
parse_num(Bin) -> parse_num(Bin, undefined, 0).

parse_num(<<>>, undefined, Acc) -> Acc;
parse_num(<<>>, 1, Acc) -> Acc;
parse_num(<<>>, Denom, Acc) -> Acc / Denom;
parse_num(<<".", Rest/binary>>, undefined, Acc) ->
    parse_num(Rest, 1, Acc);
parse_num(<<V:8, Rest/binary>>, undefined, Acc) ->
    parse_num(Rest, undefined, Acc * 10 + dec_to_int(V));
parse_num(<<V:8, Rest/binary>>, Denom, Acc) ->
    parse_num(Rest, Denom * 10, Acc * 10 + dec_to_int(V)).

hex_to_int($0) -> 0;
hex_to_int($1) -> 1;
hex_to_int($2) -> 2;
hex_to_int($3) -> 3;
hex_to_int($4) -> 4;
hex_to_int($5) -> 5;
hex_to_int($6) -> 6;
hex_to_int($7) -> 7;
hex_to_int($8) -> 8;
hex_to_int($9) -> 9;
hex_to_int($A) -> 10;
hex_to_int($B) -> 11;
hex_to_int($C) -> 12;
hex_to_int($D) -> 13;
hex_to_int($E) -> 14;
hex_to_int($F) -> 15;
hex_to_int(_) ->
    erlang:throw(bad_format).

dec_to_int($0) -> 0;
dec_to_int($1) -> 1;
dec_to_int($2) -> 2;
dec_to_int($3) -> 3;
dec_to_int($4) -> 4;
dec_to_int($5) -> 5;
dec_to_int($6) -> 6;
dec_to_int($7) -> 7;
dec_to_int($8) -> 8;
dec_to_int($9) -> 9;
dec_to_int(_) ->
    erlang:throw(bad_format).
