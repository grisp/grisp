-include("grisp.hrl").

-define(NIF_LOAD,
    case ?EMULATION_BACKEND of
        disabled ->
            ok = erlang:load_nif(atom_to_list(?MODULE), 0);
        {Platform, Module} ->
            Module:call(Platform, ?MODULE, ?FUNCTION_NAME, [])
    end
).

-define(NIF_STUB(Args),
    case ?EMULATION_BACKEND of
        disabled ->
            erlang:nif_error(nif_not_loaded);
        {Platform, Module} ->
            Module:call(Platform, ?MODULE, ?FUNCTION_NAME, Args)
    end
).
