-include("grisp.hrl").

-define(PORT_OPEN(NAME, SETTINGS),
    case ?EMULATION_BACKEND of
        disabled -> open_port(NAME, SETTINGS);
        {Platform, Module} ->
            Module:call(Platform, port, open,
                        [?MODULE, self(), NAME, SETTINGS])
    end
).

-define(PORT_COMMAND(PORT, COMMAND),
    case ?EMULATION_BACKEND of
        disabled -> port_command(PORT, COMMAND);
        {Platform, Module} ->
            Module:call(Platform, port, command,
                        [?MODULE, self(), PORT, COMMAND])
    end
).
