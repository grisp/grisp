-module(grisp_spi_drv_emu).

% API
-export([open/0]).
-export([command/2]).

%--- API -----------------------------------------------------------------------

open() ->
    Devices = application:get_env(grisp, devices, []),
    spawn_link(fun() -> init(Devices) end).

command(Pid, Command) -> Pid ! {self(), {command, Command}}.

%--- Internal ------------------------------------------------------------------

init(DeviceConfig) ->
    Devices = [begin
        Emu = emulator(Driver),
        State = Emu:init(),
        SlaveSelect = case Port of
            spi1 -> 2;
            spi2 -> 3
        end,
        {SlaveSelect, {Emu, State}}
    end || {Port, Driver} <- DeviceConfig],
    loop(Devices).

loop(Devices) ->
    receive
        {From, {command, <<SlaveSelect, Command/binary>>}} ->
            {Emu, State} = proplists:get_value(SlaveSelect, Devices),
            {Data, NewState} = Emu:command(State, Command),
            From ! {self(), {data, Data}},
            loop(lists:keyreplace(SlaveSelect, 1, Devices, {SlaveSelect, {Emu, NewState}}))
    end.

emulator(Driver) -> list_to_atom(atom_to_list(Driver) ++ "_emu").
