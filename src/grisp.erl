-module(grisp).

% API
-export([device/1]).

%--- API -----------------------------------------------------------------------

device(Port) -> grisp_device:port(Port).
