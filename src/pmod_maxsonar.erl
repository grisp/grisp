
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

-record(state, {port, list_val}).

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
    {ok, #state{port = Port,list_val = []}}.

% @private
handle_call(get_value, _From, #state{list_val = Val} = State) ->
    {reply, Val, State}.

%handle_call({get_value,Req}, _From, #state{last_val = Val} = State) ->
%  Data = grisp_termios:send_recv(Req),
%  case Data of
%      <<_, _, D1, D2, D3, 10>> when $0 =< D1, D1 =< $9,
%                                    $0 =< D2, D2 =< $9,
%                                    $0 =< D3, D3 =< $9 ->
%          Val = (D1 - $0) * 100 + (D2 - $0) * 10 + (D3 - $0),
%          {reply,Data, State#state{last_val = Val}};
%      _ ->
%          {reply,Data, State}
%  end.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info({Port, {data, Data}}, #state{port = Port,list_val = Previous} = State) ->
 if
  byte_size(Data) > 5 -> NewList = Previous;
  length(Previous) > 5 -> grisp_led:color(1,green), NewList = [];
  byte_size(Data)==5 -> grisp_led:color(1,red),
                        <<_:8,Val1:8,Val2:8,Val3:8,Rest>> = Data,
                        List = [Val1,Val2,Val3],
                        {NewValue,_} = string:to_integer(List),
                        %  io:format("Value is: ~p ~n",[NewValue]),
                        if
                          NewValue < 50 -> NewList = lists:append([NewValue],Previous);
                        true -> NewList = []
                        end;
  true -> NewList = Previous

  end,
  {noreply, State#state{list_val = NewList}}.



% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.
