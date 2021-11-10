-module(grisp_nmea_tests).

%--- Includes ------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").


%--- Setup ---------------------------------------------------------------------

gpgga_parsing_test() ->
    ?assertMatch({ok, {gps, gga, #{time := 45319000, lat := 48.1173,
                                   long := 11.516666666666667, alt := 545.4,
                                   fixed := true}}},
        grisp_nmea:parse(<<"$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47">>)),
    ?assertMatch({ok, {gps, gga, #{time := 45319000, lat := 48.1173,
                                   long := 11.516666666666667, alt := 545.4,
                                   fixed := true}}},
        grisp_nmea:parse(<<"$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47\n">>)),
    ok.
