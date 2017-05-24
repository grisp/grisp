-module(grisp).

% API
-export([device/1]).

%--- API -----------------------------------------------------------------------

device(Slot) -> grisp_device:slot(Slot).
