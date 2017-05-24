-module(grisp_spi_drv_emu).

% API
-export([open/0]).
-export([command/3]).

%--- API -----------------------------------------------------------------------

open() ->
    Devices = application:get_env(grisp, devices, []),
    spawn_link(fun() -> init(Devices) end).

command(Pid, Slot, Command) -> Pid ! {self(), {command, Slot, Command}}.

%--- Internal ------------------------------------------------------------------

init(DeviceConfig) ->
    Devices = [{Slot, init_emulator(Driver)} || {Slot, Driver} <- DeviceConfig],
    loop(Devices).

loop(Devices) ->
    receive
        {From, {command, Slot, Command}} ->
            {Emu, State} = proplists:get_value(Slot, Devices),
            {Data, NewState} = Emu:command(State, Command),
            From ! {self(), {data, Data}},
            loop(lists:keyreplace(Slot, 1, Devices, {Slot, {Emu, NewState}}))
    end.

init_emulator(Driver) ->
    Emu = list_to_atom(atom_to_list(Driver) ++ "_emu"),
    {Emu, Emu:init()}.
