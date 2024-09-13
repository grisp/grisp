-module(network_simulation).

-behaviour(application).

-include_lib("common_test/include/ct.hrl").


-include("mac_frame.hrl").

%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% application callbacks
-export([start/2]).
-export([stop/1]).

%%% application callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start(_, Args) -> {ok, pid()} when
      Args :: #{loss => boolean()}.
start(_, Args) ->
    Loss = maps:get(loss, Args, false),
    StartData = #{nodes => sets:new(), exchanges => #{}, loss => Loss},
    LoopPid = spawn(fun() -> loop(ready, StartData) end),
    register(network_loop, LoopPid),
    {ok, LoopPid}.

stop(_) ->
    network_loop ! {stop},
    unregister(network_loop).

%%%% Internal %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec loop(State, Data) -> ok when
      State :: ready | blocked,
      Data  :: #{nodes := NodeMap, loss := boolean()},
      NodeMap :: #{node() => sets:set()}.
loop(ready, #{nodes := Nodes} = Data) ->
    receive
        {ping, Pid, Node} ->
            {Pid, Node} ! pong, loop(ready, Data);
        {register, Name} ->
            NewNodes = sets:add_element(Name, Nodes),
            loop(ready, Data#{nodes => NewNodes});
        {tx, From, Frame} ->
            do_broadcast(Data, From, Frame);
        {stop} ->
            ok
    end;
loop(blocked, #{nodes := Nodes} = Data) ->
    receive 
        {register, _Name} ->
            loop(ready, Data);
        {tx, From, Frame} -> % Once a frame has been blocked => pass through
            broadcast(sets:to_list(Nodes), From, Frame),
            loop(blocked, Data);
        {stop} ->
            ok;
        OtherEvent ->
            error(wrong_event_in_blocked, OtherEvent)
    end.

do_broadcast(#{loss := false, nodes := Nodes} = Data, From, Frame) ->
    broadcast(sets:to_list(Nodes), From, Frame),
    loop(ready, Data);
do_broadcast(#{loss := true} = Data, From, Frame) ->
    {_, RawFrame} = Frame,
    #{nodes := Nodes, exchanges := Exchanges} = Data,
    {_, MH, _} = mac_frame:decode(RawFrame),
    #mac_header{dest_addr = DestAddr} = MH,
    Key = {From, DestAddr},
    case maps:get(Key, Exchanges, {0,0}) of
        {MemorySeen, MemorySeen} ->
            NewExch = maps:put(Key, {0, MemorySeen+1}, Exchanges),
            loop(blocked, Data#{exchanges => NewExch});
        {RndSeen, MemorySeen} ->
            broadcast(sets:to_list(Nodes), From, Frame),
            NewExch = maps:put(Key, {RndSeen+1, MemorySeen}, Exchanges),
            loop(ready, Data#{exchanges => NewExch})
    end.

broadcast([], _, _) -> 
    ok;
broadcast([From | T], From, Frame) -> 
    broadcast(T, From, Frame);
broadcast([Node | T], From, Frame) -> 
    {mock_phy_network, Node} ! {frame, Frame},
    broadcast(T, From, Frame).