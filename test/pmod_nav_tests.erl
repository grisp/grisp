-module(pmod_nav_tests).

-include_lib("eunit/include/eunit.hrl").

%--- Setup ---------------------------------------------------------------------

nav_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        fun read_all_/0,
        fun read_out_/0,
        fun read_ctrl_/0,
        fun read_unknown_/0,
        fun config_/0,
        fun config_unknown_/0,
        fun config_raw_/0,
        fun config_invalid_value_/0
    ]}.

setup() ->
    error_logger:tty(false),
    application:set_env(grisp, drivers, [
        {spi, grisp_spi_drv_emu},
        {gpio, grisp_gpio_drv_emu},
        {i2c, grisp_i2c_drv_emu}
    ]),
    application:set_env(grisp, devices, [
        {spi1, pmod_nav}
    ]),
    {ok, Apps} = application:ensure_all_started(grisp),
    Apps.

teardown(Apps) ->
    [application:stop(A) || A <- lists:reverse(Apps)],
    error_logger:tty(true).

%--- Tests ---------------------------------------------------------------------

read_all_() ->
    [
        pmod_nav:read(Comp, [Reg])
        ||
        {Comp, {Reg, {_Addr, _Type, _Size, _Conv}}} <- all_regs()
    ].

read_out_() ->
    ?assert(lists:all(fun is_float/1,
        pmod_nav:read(acc, [out_x_xl, out_y_xl, out_z_xl])
    )).

read_ctrl_() ->
    ?assert(lists:all(fun is_map/1,
        pmod_nav:read(acc, [ctrl_reg5_xl, ctrl_reg6_xl])
    )).

read_unknown_() ->
    ?assertError({unknown_register, acc, foo}, pmod_nav:read(acc, [foo])).

config_() ->
    ?assertEqual(ok, pmod_nav:config(acc, #{
        fs_xl => {g, 16},
        dec => {samples, 4}
    })),
    ?assertMatch([#{dec := {samples, 4}}], pmod_nav:read(acc, [ctrl_reg5_xl])),
    ?assertMatch([#{fs_xl := {g, 16}}], pmod_nav:read(acc, [ctrl_reg6_xl])).

config_unknown_() ->
    ?assertError({unknown_option, foo}, pmod_nav:config(acc, #{foo => bar})).

config_raw_() ->
    ?assertEqual(ok, pmod_nav:config(acc, #{act_ths => 0})),
    ?assertEqual(ok, pmod_nav:config(acc, #{act_ths => <<0:7>>})).

config_invalid_value_() ->
    ?assertError({invalid_value, fs_xl, foobar},
         pmod_nav:config(acc, #{fs_xl => foobar})
    ),
    ?assertError({invalid_value, act_ths, foobar},
        pmod_nav:config(acc, #{act_ths => foobar})
    ).

%--- Internal ------------------------------------------------------------------

all_regs() ->
    [
        {Comp, Reg}
        ||
        {Comp, Regs} <- maps:to_list(pmod_nav:registers()),
        Reg          <- maps:to_list(Regs)
    ].
