-module(grisp_pwm_tests).

-include_lib("eunit/include/eunit.hrl").

%--- Setup ---------------------------------------------------------------------

pwm_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun open_and_close_same_/0,
        fun open_and_close_several_/0,
        fun setting_sample_/0,
        fun non_existing_pin_/0,
        fun open_twice_/0,
        fun conflicting_pins_/0,
        fun idempotent_close_/0
    ]}.

setup() ->
    grisp_pwm:start_link().

teardown(_) ->
    ok = gen_server:stop(grisp_pwm, normal, 100).

%--- Tests ---------------------------------------------------------------------

open_and_close_same_() ->
    OpenReply = grisp_pwm:open(gpio1_2, default, <<128:16>>),
    ?assertMatch(ok, OpenReply),
    CloseReply = grisp_pwm:close(gpio1_2),
    ?assertMatch(ok, CloseReply),
    OpenReply2 = grisp_pwm:open(gpio1_2, default, <<128:16>>),
    ?assertMatch(ok, OpenReply2).

open_and_close_several_() ->
    OpenReply = grisp_pwm:open(gpio1_2, default, <<128:16>>),
    ?assertMatch(ok, OpenReply),
    OpenReply2 = grisp_pwm:open(gpio1_4, default, <<128:16>>),
    ?assertMatch(ok, OpenReply2),
    CloseReply = grisp_pwm:close(gpio1_4),
    ?assertMatch(ok, CloseReply),
    CloseReply2 = grisp_pwm:close(gpio1_2),
    ?assertMatch(ok, CloseReply2).

setting_sample_() ->
    OpenReply = grisp_pwm:open(gpio1_2, default, 1.1),
    ?assertMatch({error, sample_out_of_range}, OpenReply),
    OpenReply2 = grisp_pwm:open(gpio1_2, default, <<128:16>>),
    ?assertMatch(ok, OpenReply2),
    ?assertMatch(ok, grisp_pwm:set_sample(gpio1_2, 0.0)),
    ?assertMatch(ok, grisp_pwm:set_sample(gpio1_2, 1.0)),
    ?assertMatch(ok, grisp_pwm:set_sample(gpio1_2, 0.5)),
    ?assertMatch(ok, grisp_pwm:set_sample(gpio1_2, <<0:16>>)),
    ?assertMatch(ok, grisp_pwm:set_sample(gpio1_2, <<512:16>>)),
    ?assertMatch(ok, grisp_pwm:set_sample(gpio1_2, <<1024:16>>)),
    ?assertMatch({error, sample_out_of_range}, grisp_pwm:set_sample(gpio1_2, -0.1)),
    ?assertMatch({error, sample_out_of_range}, grisp_pwm:set_sample(gpio1_2, 1.1)),
    ?assertMatch({error, sample_out_of_range}, grisp_pwm:set_sample(gpio1_2, <<1025:16>>)),
    CloseReply = grisp_pwm:close(gpio1_2),
    ?assertMatch(ok, CloseReply).

non_existing_pin_() ->
    OpenReply = grisp_pwm:open(foobar, default, <<128:16>>),
    ?assertMatch({error, unknown_pin}, OpenReply).

open_twice_() ->
    OpenReply = grisp_pwm:open(gpio1_2, default, <<128:16>>),
    ?assertMatch(ok, OpenReply),
    OpenReply2 = grisp_pwm:open(gpio1_2, default, <<128:16>>),
    ?assertMatch({error, already_open}, OpenReply2).

conflicting_pins_() ->
    OpenReply = grisp_pwm:open(gpio1_4, default, <<128:16>>),
    ?assertMatch(ok, OpenReply),
    OpenReply2 = grisp_pwm:open(gpio1_8, default, <<128:16>>),
    ?assertMatch({error, conflicting_pin, gpio1_4}, OpenReply2).

idempotent_close_() ->
    OpenReply = grisp_pwm:open(gpio1_2, default, <<128:16>>),
    ?assertMatch(ok, OpenReply),
    CloseReply = grisp_pwm:close(gpio1_2),
    ?assertMatch(ok, CloseReply),
    CloseReply2 = grisp_pwm:close(gpio1_2),
    ?assertMatch(ok, CloseReply2).
