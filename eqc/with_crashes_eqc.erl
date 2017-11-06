%%% @author Thomas Arts 
%%% @copyright (C) 2017, Thomas Arts
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2017 by Thomas Arts 

-module(with_crashes_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_cluster.hrl").

-compile(export_all).

components() -> [ let_it_crash_eqc, led_eqc ].

weight(let_it_crash_eqc) -> 1;
weight(_) -> 5.

api_spec() -> eqc_cluster:api_spec(?MODULE).

prop_with_crashes() -> prop_with_crashes(1, 5).

prop_with_crashes(N, Period) ->
  ?SETUP(fun() ->
             %% setup mocking here
             eqc_mocking:start_mocking(api_spec(), components()),
             error_logger:tty(false),
             fun() ->  
                 eqc_mocking:stop_mocking(),
                 error_logger:tty(true),
                 ok 
             end %% Teardown function
         end,
  ?LET(Shrinking, parameter(shrinking, false),
  ?FORALL(Cmds, commands(?MODULE, [ {let_it_crash_eqc, 
                                     let_it_crash_eqc:initial_state(N,Period)} ]),
  ?ALWAYS(if Shrinking -> 10; true -> 1 end,
    begin
      application:stop(grisp),
      timer:sleep(10),
      Processes = erlang:processes(),
      {ok, _} = application:ensure_all_started(grisp),
      timer:sleep(10),  
      
      {H, S, Res} = run_commands(Cmds, [{base_processes, Processes}]),
      pretty_commands(?MODULE, Cmds, {H, S, Res},
                      measure(length, length(Cmds),
                              aggregate(call_features(H),
                                        aggregate(command_names(Cmds),
                                                  Res == ok))))
    end)))).
