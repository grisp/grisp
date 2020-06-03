% @doc
% <a href="https://store.digilentinc.com/pmodmaxsonar-maxbotix-ultrasonic-range-finder/">
% Pmod MAXSONAR
% </a>
% module.
%
% The Pmod MAXSONAR cyclically sends measurements via the UART interface.
% This module converts and stores the latest measurement.
%
% Start the driver with
%  ```
%  1> grisp:add_device(uart, pmod_maxsonar).
%  '''
% @end
-module(pmod_maxsonar).

-behaviour(gen_server).

% API
-export([start_link/2]).
-export([get/0]).
-export([get_single/0]).

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

% @doc Get the latest measured distance in inches.
-spec get() -> integer().
get() ->
    gen_server:call(?MODULE, get_value).

-spec get_single() -> integer().
get_single() ->
  gen_server:call(?MODULE, get_single_value).

%--- Callbacks -----------------------------------------------------------------

% @private
init(Slot = uart) ->
    Port = open_port({spawn_driver, "grisp_termios_drv"}, [binary]),
    grisp_devices:register(Slot, ?MODULE),
    grisp_gpio:configure(uart_2_txd, output_0),
    {ok, #state{port = Port}}.

% @private
handle_call(get_value, _From, #state{last_val = Val} = State) ->
    {reply, Val, State};
handle_call(get_single_value, _From, #state{port = Port} = State) ->
    grisp_gpio:configure(uart_2_txd, output_1),
    grisp_gpio:configure(uart_2_txd, output_0),
    Val = receive
        {Port, {data, Data}} ->
            decode(Data, State)
    end,
    {reply, Val, State#state{last_val = Val}}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    {noreply, State#state{last_val = decode(Data, State)}}.

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

decode(Data, State) ->
    case Data of
      % Format of response is 'Rxxx\n' where xxx is the decimal
      % representation of the measured range in inches (2.54cm)
      % (left-padded with zeros - so there are always three digits)
      <<$R, D1, D2, D3, $\n>> when $0 =< D1, D1 =< $9,
        $0 =< D2, D2 =< $9,
        $0 =< D3, D3 =< $9 ->
        % Val is given in inches
        (D1 - $0) * 100 + (D2 - $0) * 10 + (D3 - $0);
      % Sometimes for no obvious reason we receive
      % a different value from the sonar.
      % Instead of $R we get two garbage characters
      <<_, _, D1, D2, D3, $\n>> when $0 =< D1, D1 =< $9,
        $0 =< D2, D2 =< $9,
        $0 =< D3, D3 =< $9 ->
        % Val is given in inches
        (D1 - $0) * 100 + (D2 - $0) * 10 + (D3 - $0);
      _ ->
        State#state.last_val
    end.