%%% @author Thomas Arts
%%% @copyright (C) 2017, Quviq AB
%%% @doc Simple QuickCheck model using mocking.
%%%      We test that the command to turn on an LED on the Grisp board
%%%      has in the emulator the expected outcome.
%%%      By replacing the emulator by a mocked C function, one could test
%%%      the same for the actual device drivers.
%%%
%%%      Requires QuickCheck version 1.41.2 or above
%%%
%%% @end
%%% Created : 21 Jul 2017 by Thomas Arts

-module(led_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-compile(export_all).

-define(NR_LEDS, 2).

%% -- Generators -------------------------------------------------------------

color() ->
  elements([ {off,     {0, 0, 0}},
             {black,   {0, 0, 0}},
             {blue,    {0, 0, 1}},
             {green,   {0, 1, 0}},
             {red,     {1, 0, 0}},
             {aqua,    {0, 1, 1}},
             {magenta, {1, 0, 1}},
             {yellow,  {1, 1, 0}},
             {white,   {1, 1, 1}}]).

%% -- State ------------------------------------------------------------------
initial_state() ->
  undefined.

postcondition_common(S, Call, Res) ->
  eq(Res, return_value(S, Call)). %% Check all return values

%% -- Operations -------------------------------------------------------------

%% --- Operation: led_color ---
led_color_args(_S) ->
  [choose(1, ?NR_LEDS), color(), elements([by_value, by_name])].

led_color(Nr, {Name, _RGB}, by_name) ->
  grisp_led:color(Nr, Name),
  timer:sleep(10);
led_color(Nr, {_Name, RGB}, by_value) ->
  grisp_led:color(Nr, RGB),
  timer:sleep(10).

led_color_callouts(_S, [Nr, {_, {R, G, B}}, _]) ->
  ?PAR([
        ?CALLOUT(grisp_gpio_drv_emu, led, [pin(Nr, red),   onoff(R)], ok),
        ?CALLOUT(grisp_gpio_drv_emu, led, [pin(Nr, green), onoff(G)], ok),
        ?CALLOUT(grisp_gpio_drv_emu, led, [pin(Nr, blue),  onoff(B)], ok)]),
  ?RET(ok).

%% --- Operation: off ---
led_off_args(_S) ->
  [choose(1, ?NR_LEDS)].

led_off(Nr) ->
  grisp_led:off(Nr),
  timer:sleep(10).

led_off_callouts(_S, [Nr]) ->
  ?PAR([
        ?CALLOUT(grisp_gpio_drv_emu, led, [pin(Nr, red),   clear], ok),
        ?CALLOUT(grisp_gpio_drv_emu, led, [pin(Nr, green), clear], ok),
        ?CALLOUT(grisp_gpio_drv_emu, led, [pin(Nr, blue),  clear], ok)]),
  ?RET(ok).

pin(1, red)   -> led1_r;
pin(1, green) -> led1_g;
pin(1, blue)  -> led1_b;
pin(2, red)   -> led2_r;
pin(2, green) -> led2_g;
pin(2, blue)  -> led2_b.

onoff(1) -> set;
onoff(0) -> clear.

%% -- Property ---------------------------------------------------------------

weight(_S, _Cmd) -> 1.

prop_led() ->
  ?SETUP(fun() ->
             %% setup mocking here
             eqc_cover:start(),
             eqc_mocking:start_mocking(api_spec()),
             {ok, Config} = file:consult("config/test.config"),
             _ = rebar_utils:reread_config(Config),
             error_logger:tty(false),
             fun() ->
                 eqc_mocking:stop_mocking(),
                 eqc_cover:write_html(eqc_cover:stop(), [{out_dir, "cover"}]),
                 error_logger:tty(true)
             end %% Teardown function
         end,
  eqc:dont_print_counterexample(
  ?FORALL(Cmds, commands(?MODULE),
  begin
    application:stop(grisp),
    timer:sleep(5),
    {ok, _} = application:ensure_all_started(grisp),
    timer:sleep(10),
    {H, S, Res} = run_commands(Cmds),
    check_command_names(Cmds,
                        measure(length, commands_length(Cmds),
                                pretty_commands(?MODULE, Cmds, {H, S, Res},
                                                Res == ok)))
  end))).

%% -- API-spec ---------------------------------------------------------------
api_spec() ->
  #api_spec{ language = erlang,
             mocking = eqc_mocking,
             modules =
               [ #api_module{
                    name = grisp_gpio_drv_emu,
                    fallback = grisp_gpio_drv_emu,
                    functions =  [ #api_fun{ name = led, arity = 2} ]}
               ]}.
