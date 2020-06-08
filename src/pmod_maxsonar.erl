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
-export([set_mode/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- Records -------------------------------------------------------------------

-record(state, {
  port,
  last_val,
  callers :: list(),
  mode :: disabled %% in this mode returns undefined when getting a value
  | single %% Triggering mode
  | continuous %% Free run mode
}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot, _Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Slot, []).

% @doc Get the latest measured distance in inches.
-spec get() -> integer().
get() ->
  gen_server:call(?MODULE, get_value).

-spec set_mode(disabled | continuous | single) -> any().
set_mode(Mode) ->
  gen_server:call(?MODULE, {set_mode, Mode}).

%--- Callbacks -----------------------------------------------------------------

% @private
init(Slot = uart) ->
  Port = open_port({spawn_driver, "grisp_termios_drv"}, [binary]),
  grisp_devices:register(Slot, ?MODULE),
  grisp_gpio:configure(uart_2_txd, output_1), %% by default continuous mode
  {ok, #state{port = Port, mode = continuous, callers = []}}.

% @private
handle_call(get_value, _From, #state{mode = disabled} = State) ->
  {reply, undefined, State};
handle_call(get_value, _From, #state{last_val = Val, mode = continuous} = State) ->
  {reply, Val, State};
handle_call(get_value, From, #state{mode = single, callers = Callers} = State) ->
  case length(Callers) of
    0 ->
      grisp_gpio:configure(uart_2_txd, output_1),
      grisp_gpio:configure(uart_2_txd, output_0);
    _ -> ok
  end,
  {noreply, State#state{callers = State#state.callers ++ [From]}};
handle_call({set_mode, disabled}, _From, State) ->
  grisp_gpio:configure(uart_2_txd, output_0),
  {reply, ok, State#state{mode = disabled}};
handle_call({set_mode, single}, _From, State) ->
  grisp_gpio:configure(uart_2_txd, output_0),
  {reply, ok, State#state{mode = single}};
handle_call({set_mode, continuous}, _From, State) ->
  grisp_gpio:configure(uart_2_txd, output_1),
  {reply, ok, State#state{mode = continuous}}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info({Port, {data, Data}}, #state{port = Port, mode = continuous} = State) ->
  {noreply, State#state{last_val = decode(Data, State)}};
handle_info({Port, {data, Data}}, #state{port = Port, mode = single, callers = Callers} = State) ->
  lists:map(fun(C) -> gen_server:reply(C, decode(Data, State)) end, Callers),
  {noreply, State#state{port = Port, callers = []}}.

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

% @private
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