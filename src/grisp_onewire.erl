-module(grisp_onewire).

-include("grisp_i2c.hrl").

% API
-export([reset/0]).

%--- API -----------------------------------------------------------------------


reset() ->
    <<Status:8>> = grisp_i2c:msgs([16#18, 
				   {write, <<16#f0>>}, 
				   {read, 1, ?I2C_M_NO_RD_ACK}]),
    case Status band 16#f7 of
	16#10 -> ok;
	Any -> error({invalid_status, Any})
    end.
