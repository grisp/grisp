-module(grisp).

% API
-export([device/1]).

%--- API -----------------------------------------------------------------------

device(Slot) -> grisp_devices:slot(Slot).
