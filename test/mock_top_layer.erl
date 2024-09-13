-module(mock_top_layer).

-behaviour(gen_server).

-include("mac_frame.hrl").

-include_lib("common_test/include/ct.hrl").

%%% EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API functions
-export([start/0]).
-export([rx_frame/4]).
-export([dump/0]).
-export([stop/0]).

%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

%%% MACROS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-define(RCVR_ADDR, <<16#CAFEDECA00000003:64>>).
-define(MDL_ADDR, <<16#CAFEDECA00000002:64>>).

%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

rx_frame(Frame, _, _, _) ->
    gen_server:cast(?MODULE, {rx, Frame}).

dump() ->
    gen_server:call(?MODULE, {dump}).

stop() ->
    gen_server:stop(?MODULE).

%%% gen_server callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_) ->
    {ok, #{received => #{}}}.

handle_call({dump}, _From, #{received := Received} = State) ->
    Ret = lists:sort(fun({_, MH1, _}, {_, MH2, _}) ->
                             MH1#mac_header.seqnum < MH2#mac_header.seqnum
                     end, maps:values(Received)),
    {reply, {ok, Ret}, State#{received => []}};
handle_call(_, _, _) ->
  error(not_implemented).

handle_cast({rx, {FC, MH, Payload}=Frame}, #{received := Received} = State)
  when FC#frame_control.frame_type == ?FTYPE_DATA ->
    Seqnum = MH#mac_header.seqnum,
    case Received of
        #{Seqnum := _} ->
            {noreply, State};
        _ ->
            case MH#mac_header.dest_addr of
                ?MDL_ADDR ->
                    NewMH = MH#mac_header{seqnum = Seqnum + 10,
                                          src_addr = ?MDL_ADDR,
                                          dest_addr = ?RCVR_ADDR},
                    NewFrame = {FC, NewMH, Payload},
                    ieee802154:transmission(NewFrame);
                _ ->
                    ok
            end,
            {noreply, State#{received => maps:put(Seqnum, Frame, Received)}}
    end;
handle_cast({rx, _}, State) ->
    {noreply, State};
handle_cast(_, _) ->
  error(not_implemented).

handle_info(_, _) ->
  error(not_implemented).

terminate(_, _) ->
    ok.