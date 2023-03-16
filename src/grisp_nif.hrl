-define(EMULATION,
    case persistent_term:get(grisp_emulation, undefined) of
        undefined ->
            Configured = application:get_env(grisp, emulation, disabled),
            persistent_term:put(grisp_emulation, Configured),
            Configured;
        Current ->
           Current
    end
).

-define(NIF_LOAD,
    case ?EMULATION of
        disabled ->
            ok = erlang:load_nif(atom_to_list(?MODULE), 0);
        {Platform, Module} ->
            Module:call(Platform, ?MODULE, ?FUNCTION_NAME, [])
    end
).

-define(NIF_STUB(Args),
    case ?EMULATION of
        disabled ->
            erlang:nif_error(nif_not_loaded);
        {Platform, Module} ->
            Module:call(Platform, ?MODULE, ?FUNCTION_NAME, Args)
    end
).
