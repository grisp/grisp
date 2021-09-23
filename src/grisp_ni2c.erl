% @doc GRiSP I2C API.
-module(grisp_ni2c).

% API
-export([register_bus/2]). % FIXME: REMOVE
-export([open/1]).
-export([detect/1]).
-export([read/4]).
-export([transfer/2]).

% Callbacks
-export([init/0]).

% Attributes

-on_load(init/0).

-include("grisp_nif.hrl").

%--- API -----------------------------------------------------------------------

open(Path) -> open_nif(null(Path)).

% @doc Detect I2C devices on a bus.
%
% === Example ===
% ```
% 1> [io_lib:format("0x~.16B", [Addr]) || Addr <- grisp_ni2c:detect(Bus1)].
% ["0x18","0x36","0x37","0x52","0x57","0x5A","0x5F"]
% '''
detect(Bus) ->
    lists:reverse(lists:foldl(fun(ChipAddr, Detected) ->
        case grisp_ni2c:transfer(Bus, [{ChipAddr, 0, <<>>}]) of
            {error, ioctl_failed, _} -> Detected;
            [ok] -> [ChipAddr|Detected]
        end
    end, [], lists:seq(1, 127))).

read(Bus, ChipAddr, DataAddr, Bytes) ->
    [ok, Resp] = grisp_ni2c:transfer(Bus, [{ChipAddr, 0, <<DataAddr>>}, {ChipAddr, 1, Bytes}]),
    Resp.

% @doc Transfer I2C messages on a bus.
transfer(Bus, Messages) -> transfer_nif(Bus, Messages).

%--- Callbacks -----------------------------------------------------------------

init() ->
    ok = erlang:load_nif(atom_to_list(?MODULE), 0),
    case grisp_hw:platform() of
        grisp2 ->
            ok = register_bus(<<"/dev/i2c-1">>, <<"i2c0">>),
            ok = register_bus(<<"/dev/i2c-2">>, <<"i2c1">>)
    end.

%--- Internal ------------------------------------------------------------------

register_bus(Path, Alias) -> register_bus_nif(null(Path), null(Alias)).

register_bus_nif(_Bus, _Alias) -> ?NIF_STUB.

open_nif(_Bus) -> ?NIF_STUB.

transfer_nif(_Bus, _Messages) -> ?NIF_STUB.

null(Bin) -> [Bin, 0].
