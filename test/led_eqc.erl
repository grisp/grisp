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

% see ../rebar.config
-ifndef(NO_QC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-compile(export_all).

-define(NR_LEDS, 2).

%% -- Generators -------------------------------------------------------------

color() ->
  oneof([ {black,   {0, 0, 0}},
          {blue,    {0, 0, 1}},
          {green,   {0, 1, 0}},
          {aqua,    {0, 1, 1}},
          {red,     {1, 0, 0}},
          {magenta, {1, 0, 1}},
          {yellow,  {1, 1, 0}},
          {white,   {1, 1, 1}}]).

%% -- State ------------------------------------------------------------------
initial_state() ->
  undefined.

postcondition_common(S, Call, Res) ->
  eq(Res, return_value(S, Call)). %% Check all return values

%% -- Operations -------------------------------------------------------------

%% --- Operation: grisp_led ---
grisp_led_args(_S) ->
  [choose(1, ?NR_LEDS), color(), elements([by_value, by_name])].

grisp_led(Nr, {Name, _RGB}, by_name) ->
  grisp_led:color(Nr, Name),
  timer:sleep(10);
grisp_led(Nr, {_Name, RGB}, by_value) ->
  grisp_led:color(Nr, RGB),
  timer:sleep(10).

grisp_led_callouts(_S, [Nr, {_, {R, G, B}}, _]) ->
  ?PAR([
        ?CALLOUT(grisp_gpio_drv_emu, led, [Nr, red, onoff(R)], ok),
        ?CALLOUT(grisp_gpio_drv_emu, led, [Nr, green, onoff(G)], ok),
        ?CALLOUT(grisp_gpio_drv_emu, led, [Nr, blue, onoff(B)], ok)]),
  ?RET(ok).


onoff(1) -> on;
onoff(0) -> off.
  
%% borrowed code, ugly!



%% -- Property ---------------------------------------------------------------

weight(_S, _Cmd) -> 1.

prop_led() ->
  ?SETUP(fun() ->
             %% setup mocking here
             application:stop(grisp),
             eqc_cover:start(),
             ok = application:start(grisp),
             eqc_mocking:start_mocking(api_spec()),
             fun() -> 
                 eqc_mocking:stop_mocking(),
                 eqc_cover:write_html(eqc_cover:stop(), [{out_dir, "cover"}])
             end %% Teardown function
         end,
  ?FORALL(Cmds, commands(?MODULE),
  begin
    {H, S, Res} = run_commands(Cmds),
    check_command_names(Cmds,
                        measure(length, commands_length(Cmds),
                                pretty_commands(?MODULE, Cmds, {H, S, Res},
                                                Res == ok)))
  end)).

%% -- API-spec ---------------------------------------------------------------
api_spec() -> 
  #api_spec{ language = erlang, 
             mocking = eqc_mocking, 
             modules = 
               [ #api_module{ 
                    name = grisp_gpio_drv_emu,
                    fallback = grisp_gpio_drv_emu,
                    functions =  [ #api_fun{ name = led, arity = 3} ]} 
               ]}.
-endif.
