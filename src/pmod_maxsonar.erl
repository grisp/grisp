-module(pmod_maxsonar).

-behavior(gen_server).

% API
-export([start_link/2]).
-export([get/0]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- Records -------------------------------------------------------------------

-record(state, {port, last_val}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot, _Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Slot, []).

get() ->
    gen_server:call(?MODULE, get_value).

%--- Callbacks -----------------------------------------------------------------

% @private
init(Slot = uart) ->
    Port = open_port({spawn_driver, "grisp_termios_drv"}, [binary]),
    grisp_devices:register(Slot, ?MODULE),
    {ok, #state{port = Port}}.

% @private
handle_call(get_value, _From, #state{last_val = Val} = State) ->
    {reply, Val, State}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    case Data of
        <<_, _, D1, D2, D3, 10>> when $0 =< D1, D1 =< $9,
                                      $0 =< D2, D2 =< $9,
                                      $0 =< D3, D3 =< $9 ->
            Val = (D1 - $0) * 100 + (D2 - $0) * 10 + (D3 - $0),
            {noreply, State#state{last_val = Val}};
        _ ->
            {noreply, State}
    end.



% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.
