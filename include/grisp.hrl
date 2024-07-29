-ifndef(GRISP_HRL).
-define(GRISP_HRL, true).

-define(EMULATION_BACKEND,
    case persistent_term:get(grisp_emulation, undefined) of
        undefined ->
            Configured = application:get_env(grisp, emulation, disabled),
            persistent_term:put(grisp_emulation, Configured),
            Configured;
        Current ->
            Current
    end
).

-define(IS_EMULATED, (?EMULATION_BACKEND =/= disabled)).

-endif. % GRISP_HRL
