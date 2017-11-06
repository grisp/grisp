%%% @author Thomas Arts 
%%% @copyright (C) 2017, Thomas Arts
%%% @doc A specific component that should be clustered using with_crashes_eqc.erl
%%%      with led_eqc.erl. It performs fault injections in the gpio driver.
%%%
%%%      We test that any N crashes in T seconds can be tolerated.
%%%
%%% @end
%%% Created : 24 Jul 2017 by Thomas Arts

-module(let_it_crash_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-compile(export_all).

-define(IFF(X,Y), (not(X) orelse Y) andalso (not(Y) orelse X)).

%% How many milliseconds precision in "N crashes in T seconds"?
-define(MARGIN, 20).  

%% -- State ------------------------------------------------------------------
-record(state,{crashes = [],  %% [ time() ]
               max_crashes,
               period
              }).

initial_state() ->
  initial_state(2,1).

initial_state(N, Period) ->
  #state{max_crashes = N, period = Period * 1000}.


%% -- helpers ----------------------------------------------------------------

%% 0 equals not allowed, other value returned is moment of crash
crashing_allowed(Crashes, Period, Max) ->
  Now = dynamic_now(),
  InPeriod = [ Crash || Crash <- Crashes, Crash > (Now - (Period + ?MARGIN)) ],
%  MarginFromLast = 
%    InPeriod == [] orelse lists:last(InPeriod) + ?MARGIN < Now,
  if length(InPeriod) < Max -> Now;
     true -> 0 end.

%% --- Operation: crash ---
crash_args(S) ->
   [ {var, base_processes}, nat(), S#state.crashes, S#state.period, S#state.max_crashes ].

crash_pre(S, [_, _, Crashes, _, _]) ->
  S#state.crashes == Crashes.

crash_adapt(S, [Var, N, _Crashes, Period, Max]) ->
  [Var, N, S#state.crashes, Period, Max].

crash(Processes, N, Crashes, Period, Max) ->
  NewProcs = erlang:processes() -- Processes,
  case erlang:processes() -- Processes of
    [] ->
      0;  %% no new processes created, we cannot crash anything
    _ ->
      Proc = lists:nth((N rem length(NewProcs)) + 1, NewProcs),
      case crashing_allowed(Crashes, Period, Max) of
        0 ->
          %% Zero is always outside the period that we look at
          0;
        Now ->
          Info = erlang:process_info(Proc),
          exit(Proc, killed_by_quickcheck),
          let_it_crash:process(Info),
          Now
      end
  end.

%% Only for failure output to see which process has been killed
crash_callouts(_S, _Args) ->
  ?OPTIONAL(?CALLOUT(let_it_crash, process, [?WILDCARD], ok)).

crash_next(S, V, _Args) ->
  S#state{crashes = S#state.crashes ++ [ V ]}.

crash_features(S, _Args, Res) ->
  [{crashed, in_period(S#state.crashes, S#state.period)} || Res =/= 0].

%% --- Operation: sleep ---
sleep_pre(S) ->
  length(S#state.crashes) > 0.

sleep_args(S) ->
  [?LET(T, choose(10, S#state.period), S#state.period - T)].

sleep(Time) ->
  timer:sleep(Time).


%% -- Driver Crashing --------------------------------------------------------
driver_failure_args(S) ->
  ?LET(Args, led_eqc:led_color_args(undefined), Args ++ 
         [choose(1,3), S#state.crashes, S#state.period, S#state.max_crashes]).

driver_failure_pre(S, [_, _, _, _Failure, Crashes, _, _]) ->
  S#state.crashes == Crashes.

driver_failure_adapt(S, [Nr, Color, Kind, Failure, _Crashes, Period, Max]) ->
  [Nr, Color, Kind, Failure, S#state.crashes, Period, Max].

driver_failure(Nr, Color, Kind, _Failure, Crashes, Period, Max) ->
  case crashing_allowed(Crashes, Period, Max) of
    0 ->
      %% Zero is always outside the period that we look at
      0;
    Now ->
      led_eqc:led_color(Nr, Color, Kind),
      Now
  end.

driver_failure_callouts(_S, [Nr, _, _, Failure, _, _, _]) ->
  Ok = fun(X) when X==Failure -> not_ok;
          (_) -> ok
       end,
  ?PAR([
        ?OPTIONAL(
           ?CALLOUT(grisp_gpio_drv_emu, led, [led_eqc:pin(Nr, red),   ?WILDCARD], Ok(1))),
        ?OPTIONAL(
           ?CALLOUT(grisp_gpio_drv_emu, led, [led_eqc:pin(Nr, green), ?WILDCARD], Ok(2))),
        ?OPTIONAL(
           ?CALLOUT(grisp_gpio_drv_emu, led, [led_eqc:pin(Nr, blue),  ?WILDCARD], Ok(3)))]).

driver_failure_next(S, V, _Args) ->
  S#state{crashes = S#state.crashes ++ [ V ]}.

driver_failure_features(S,  _Args, Res) ->
  [ {driver_crash, in_period(S#state.crashes, S#state.period)} || Res =/= 0 ].


%% -- Exception raising operations -------------------------------------------
exceptions_args(S) ->
  [choose(0, 3), {brown, {2,1,0}}, elements([by_name, by_value]), 
   S#state.crashes, S#state.period, S#state.max_crashes].

exceptions_pre(S, [_, _, _, Crashes, _, _]) ->
  S#state.crashes == Crashes.

exceptions_adapt(S, [Nr, Color, NameVal, _Crashes, Period, Max]) ->
  [Nr, Color, NameVal, S#state.crashes, Period, Max].

exceptions(Nr, Color, NameVal, Crashes, Period, Max) ->
  case crashing_allowed(Crashes, Period, Max) of
    0 ->
      %% Zero is always outside the period that we look at
      0;
    Now ->
      led_eqc:led_color(Nr, Color, NameVal),
      Now
  end.

exceptions_next(S, V, _) ->
  S#state{crashes = S#state.crashes ++ [ V ]}.

exceptions_features(S, _Args, Res) ->
  [ {exceptions, in_period(S#state.crashes, S#state.period)} || Res =/= 0 ].


%% Now in milli seconds
dynamic_now() ->
  {Mega, S, Micro} = os:timestamp(),
  ((Mega * 1000 * 1000 + S) * 1000) + (Micro div 1000).

in_period(Crashes, Period) ->
  Now = dynamic_now(),
  [ Crash || Crash <- Crashes,
             Crash > Now - Period ]. 

%% -- Property ---------------------------------------------------------------

weight(_S, _Cmd) -> 1.

api_spec() -> 
  #api_spec{ language = erlang, 
             mocking = eqc_mocking, 
             modules = 
               [  #api_module{ 
                     name = let_it_crash, 
                     functions = 
                       [#api_fun{ name = process, arity = 1}] } ]}.


