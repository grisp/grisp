-module(pmod_nav_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_nav_config/0]).

%--- Properties ----------------------------------------------------------------

prop_nav_config() ->
    ?SETUP(fun() ->
        {ok, Config} = file:consult("config/test.config"),
        _ = rebar_utils:reread_config(Config),
        fun() -> ok end
    end,
    ?FORALL({Comp, Opt, Value}, opt(), begin
        {ok, _} = application:ensure_all_started(grisp),
        ok == pmod_nav:config(Comp, #{Opt => Value})
    end)).

%--- Generators ----------------------------------------------------------------

comp() -> oneof(maps:to_list(pmod_nav:registers())).

reg() ->
    ?LET({Comp, Registers}, comp(),
        ?LET(
            {_Reg, {_Addr, _Type, _Size, Conv}},
            ?SUCHTHAT(
                {_Reg, {_Addr, Type, _Size, Conv}},
                oneof(maps:to_list(Registers)),
                is_list(Conv) andalso Type == read_write
            ),
            {Comp, ?SUCHTHAT(C, oneof(Conv), element(1, C) =/= 0)}
        )
    ).

opt() ->
    ?LET({Comp, {Opt, OptSize, Mapping}}, reg(), begin
        ?LET(Value, value(OptSize, Mapping), {Comp, Opt, Value})
    end).

value(_OptSize, Mapping) when is_map(Mapping) ->
    oneof(maps:keys(Mapping));
value(_OptSize, unsigned_little) ->
    nat();
value(_OptSize, {unsigned_little, Min, Max}) ->
    choose(Min, Max);
value(_OptSize, signed_little) ->
    int();
value(OptSize, raw) ->
    bitstring(OptSize).
