-module(pmod_maxsonar).
-include("grisp_docs.hrl").

?moduledoc("""
[Pmod MAXSONAR](https://store.digilentinc.com/pmodmaxsonar-maxbotix-ultrasonic-range-finder/)
module.

The Pmod MAXSONAR cyclically sends measurements via the UART interface.
This module converts and stores the latest measurement.

Start the driver with
```
1> grisp:add_device(uart, pmod_maxsonar).
```
""").

-behaviour(gen_server).

-include("grisp_port.hrl").

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

?doc(false).
start_link(Slot, _Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Slot, []).

?doc("""
Get the latest measured distance in inches.
""").
-spec get() -> integer().
get() ->
    gen_server:call(?MODULE, get_value).

%--- Callbacks -----------------------------------------------------------------

?doc(false).
init(Slot = uart) ->
    Port = ?PORT_OPEN({spawn_driver, "grisp_termios_drv"}, [binary]),
    grisp_devices:register(Slot, ?MODULE),
    {ok, #state{port = Port}}.

?doc(false).
handle_call(get_value, _From, #state{last_val = Val} = State) ->
    {reply, Val, State}.

?doc(false).
handle_cast(Request, _State) -> error({unknown_cast, Request}).

?doc(false).
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    case Data of
        % Format of response is 'Rxxx\n' where xxx is the decimal
        % representation of the measured range in inches (2.54cm)
        % (left-padded with zeros - so there are always three digits)
        <<$R, D1, D2, D3, $\n>> when $0 =< D1, D1 =< $9,
                                      $0 =< D2, D2 =< $9,
                                      $0 =< D3, D3 =< $9 ->
            % Val is given in inches
            Val = (D1 - $0) * 100 + (D2 - $0) * 10 + (D3 - $0),
            {noreply, State#state{last_val = Val}};
        % Sometimes for no obvious reason we receive
        % a different value from the sonar.
        % Instead of $R we get two garbage characters
        <<_, _, D1, D2, D3, $\n>> when $0 =< D1, D1 =< $9,
                                      $0 =< D2, D2 =< $9,
                                      $0 =< D3, D3 =< $9 ->
            % Val is given in inches
            Val = (D1 - $0) * 100 + (D2 - $0) * 10 + (D3 - $0),
            {noreply, State#state{last_val = Val}};
        _ ->
            {noreply, State}
    end.



?doc(false).
code_change(_OldVsn, State, _Extra) -> {ok, State}.

?doc(false).
terminate(_Reason, _State) -> ok.
