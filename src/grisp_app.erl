% @private
-module(grisp_app).

-behavior(application).

% API
-export([start/2]).
-export([stop/1]).

%--- API -----------------------------------------------------------------------

start(_Type, _Args) ->
    {ok, Pid} = grisp_sup:start_link(),
    grisp_devices:setup(),
    {ok, Pid}.

stop(_State) -> ok.
