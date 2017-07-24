%%% @author Thomas Arts 
%%% @copyright (C) 2017, Thomas Arts
%%% @doc A specific component that should be clustered using with_crashes_eqc.erl
%%%      with a general component specification that has a supervisor structure
%%%      to restart processes.
%%%
%%%      We test that any N crashes in T seconds can be tolerated.
%%%
%%% @end
%%% Created : 23 Jul 2017 by Thomas Arts

-module(let_it_crash_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-compile(export_all).

-define(IFF(X,Y), (not(X) orelse Y) andalso (not(Y) orelse X)).

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

%% 0 is not allowed, otherwise value returned is moment of crash
crashing_allowed(Crashes, Period, Max) ->
  Now = dynamic_now(),
  InPeriod = [ Crash || Crash <- Crashes, Crash > (Now - Period) ],
  if length(InPeriod) < Max -> Now; true -> 0 end.

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
          io:format("Crashing proc ~p: ~p from total ~p\n",[Proc, (N rem length(NewProcs)) + 1, length(NewProcs) ]),
          Info = erlang:process_info(Proc),
          timer:exit_after(7, Proc, killed_by_quickcheck),
                                                % exit(Proc, killed_by_quickcheck),
          let_it_crash:process(Info),
          Now
      end
  end.

crash_callouts(_S, _Args) ->
  ?OPTIONAL(?CALLOUT(let_it_crash, process, [?WILDCARD], ok)).

crash_next(S, V, _Args) ->
  S#state{crashes = S#state.crashes ++ [ V ]}.

crash_features(S, _Args, Res) ->
  [{crashed, length(S#state.crashes) + 1} || Res =/= 0].

%% --- Operation: sleep ---
sleep_pre(S) ->
  length(S#state.crashes) > 0.

sleep_args(S) ->
  [?LET(T, choose(10, S#state.period), S#state.period - T)].

sleep(Time) ->
  timer:sleep(Time).





%% -- Exception raising operations -------------------------------------------
%% led_fault_tolerant_args(S) ->
%%   [choose(1, 2), {brown, {2,1,0}} elements([byname, by_value]), 
%%    S#state.crashes, S#state.period, S#state.max_crashes].

led_fault_tolerant_pre(S, [_, _, _, Crashes, _, _]) ->
  S#state.crashes == Crashes.

led_fault_tolerant_adapt(S, [Nr, Color, NameVal, _Crashes, Period, Max]) ->
  [Nr, Color, NameVal, S#state.crashes, Period, Max].

led_fault_tolerant(Nr, Color, NameVal, Crashes, Period, Max) ->
  case crashing_allowed(Crashes, Period, Max) of
    0 ->
      %% Zero is always outside the period that we look at
      0;
    Now ->
      led_eqc:led_color(Nr, Color, NameVal),
      Now
  end.

led_fault_tolerant_next(S, V, _) ->
  S#state{crashes = S#state.crashes ++ [ V ]}.


%% Now in milli seconds
dynamic_now() ->
  {Mega, S, Micro} = os:timestamp(),
  ((Mega * 1000 * 1000 + S) * 1000) + (Micro div 1000)  .

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


