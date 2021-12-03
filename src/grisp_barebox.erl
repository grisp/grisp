% @doc
% @end
-module(grisp_barebox).

-behaviour(gen_server).


%--- EXPORTS -------------------------------------------------------------------

% API
-export([start_link/1]).
-export([load/0]).
-export([get/1]).
-export([get_all/0]).
-export([set/2]).
-export([commit/0]).

% Behaviour gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- TYPES ---------------------------------------------------------------------

-type var_name() :: [atom()].
-type var_type() :: uint32.

%--- RECORDS -------------------------------------------------------------------

-record(var, {
    name :: var_name(),
    type :: var_type(),
    value :: non_neg_integer()
}).
-record(spec, {
    magic :: binary(),
    start :: non_neg_integer(),
    size :: non_neg_integer(),
    stride :: non_neg_integer(),
    vars :: [#var{}]
}).
-record(state, {
    store :: atom(),
    spec :: #spec{},
    vars :: [#var{}]
}).


%--- MACROS --------------------------------------------------------------------

-define(DIRECT_STORAGE_MAGIC, <<243,253,84,35>>).


%--- API FUNCTIONS -------------------------------------------------------------

% @private
start_link(Store)->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Store], []).

load() ->
    gen_server:call(?MODULE, load).

get(Name) ->
    gen_server:call(?MODULE, {get, Name}).

get_all() ->
    gen_server:call(?MODULE, get_all).

set(Name, Value) ->
    gen_server:call(?MODULE, {set, Name, Value}).

commit() ->
    gen_server:call(?MODULE, commit).


%--- BEHAVIOUR gen_server CALLBACK FUNCTIONS -----------------------------------

% @private
init([Store]) ->
    %TODO: This should be read from the FDT
    Spec = #spec{
        magic = <<129,72,215,175>>, % 0xafd74881
        start = 0,
        size = 192,
        stride = 64,
        vars = [
            #var{name = [bootstate, booted_system], type = uint32, value = 0},
            #var{name = [bootstate, update_system], type = uint32, value = 0},
            #var{name = [bootstate, update_boot_count], type = uint32, value = 0},
            #var{name = [bootstate, active_system], type = uint32, value = 0}
        ]
    },
    State = #state{store = Store, spec = Spec, vars = []},
    {ok, load_vars(State)}.

% @private
handle_call(load, _From, State) ->
    {reply, ok, load_vars(State)};
handle_call({get, Name}, _From, State) ->
    {reply, get_var(State, Name), State};
handle_call(get_all, _From, State) ->
    {reply, get_all_vars(State), State};
handle_call({set, Name, Value}, _From, State) ->
    case set_var(State, Name, Value) of
        {error, _Reason} = Error ->
            {reply, Error, State};
        {ok, State2} ->
            {reply, ok, State2}
    end;
handle_call(commit, _From, State) ->
    case store_vars(State) of
        {error, _Reason} = Error ->
            {reply, Error, State};
        {ok, State2} ->
            {reply, ok, State2}
    end;
handle_call(Call, _From, _State) ->
    error({unknown_call, Call}).

% @private
handle_cast(Request, _State) ->
    error({unknown_cast, Request}).

% @private
handle_info(Info, _State) ->
    error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% @private
terminate(_Reason, _State) ->
    ok.

%--- INTERNAL FUNCTIONS --------------------------------------------------------

vars_to_str(Vars) ->
    L = [io_lib:format("~s=~p",
            [lists:flatten(lists:join(".", [atom_to_list(A) || A <- N])), V])
         || #var{name = N, value = V} <- Vars],
    list_to_binary(lists:join(", ", L)).

load_vars(#state{store = Store, spec = Spec} = State) ->
    #spec{start = Start, size = Size} = Spec,
    Vars2 = load_var_block(Store, Spec, Start, Start + Size, 0),
    State#state{vars = Vars2}.

get_var(#state{vars = Vars}, Name) ->
    case lists:keyfind(Name, #var.name, Vars) of
        false -> undefined;
        #var{value = V} -> V
    end.

get_all_vars(#state{vars = Vars}) ->
    lists:foldl(fun add_var/2, #{}, Vars).

add_var(#var{name = N, value = V}, Map) ->
    add_var(Map, N, V).

add_var(Map, [N], V) ->
    Map#{N => V};
add_var(Map, [N | R], V) ->
    Map#{N => add_var(maps:get(N, Map, #{}), R, V)}.

set_var(#state{vars = Vars} = State, Name, Value) ->
    case lists:keyfind(Name, #var.name, Vars) of
        false -> {error, unknown_variable};
        #var{type = Type} = Var ->
            case update_value(Type, Value) of
                {error, _Reason} = Error -> Error;
                {ok, Value2} ->
                    Var2 = Var#var{value = Value2},
                    Vars2 = lists:keyreplace(Name, #var.name, Vars, Var2),
                    {ok, State#state{vars = Vars2}}
            end
    end.

update_value(uint32, Value) when Value >= 0, Value =< 16#FFFFFFFF ->
    {ok, Value};
update_value(uint32, Value) ->
    {error, {invalid_value, uint32, Value}};
update_value(Type, Value) ->
    {error, {unsupported_type, Type, Value}}.

store_vars(#state{store = Store, spec = Spec, vars = Vars} = State) ->
    #spec{start = Start, size = Size} = Spec,
    Block = format_block(Spec, Vars),
    case store_blocks(Store, Spec, Start, Start + Size, 0, Block) of
        {error, _Reason} = Error -> Error;
        ok -> {ok, State}
    end.

store_blocks(_Store, _Spec, _Start, _EndAddr, 3, _Block) ->
    ok;
store_blocks(Store, Spec, Start, EndAddr, Count, Block) ->
    BlockSize = byte_size(Block),
    DataSize = min(BlockSize, EndAddr - Start),
    <<Data:DataSize/binary, _/binary>> = Block,
    logger:info("Writing barebox variable block ~w at ~w", [Count + 1, Start]),
    try {DataSize, grisp_eeprom:write(Store, Start, Data)} of
        {BlockSize, ok} ->
            store_blocks(Store, Spec, Start + Spec#spec.stride, EndAddr,
                         Count + 1, Block);
        {_TrucatedSize, ok} ->
            ok
    catch
        error:Reason -> {error, Reason}
    end.

format_block(#spec{magic = BlockMagic}, Vars) ->
    Data = format_vars(Vars),
    DataSize = byte_size(Data),
    DataCrc = erlang:crc32(Data),
    Header = <<BlockMagic:4/binary, 0:16, DataSize:16/little-integer,
               DataCrc:32/little-integer>>,
    HeaderSize = byte_size(Header),
    HeaderCrc = erlang:crc32(Header),
    BlockSize = HeaderSize + 4 + DataSize,
    <<?DIRECT_STORAGE_MAGIC/binary, BlockSize:32/little-integer,
      Header/binary, HeaderCrc:32/little-integer, Data/binary>>.

format_vars(Vars) ->
    format_vars(Vars, []).

format_vars([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
format_vars([#var{type = uint32, value = Value} | Rest], Acc) ->
    format_vars(Rest, [<<Value:32/little-integer>> | Acc]).

load_var_block(_Store, Spec, _Addr, _EndAddr, 3) ->
    load_var_block_not_found(Spec);
load_var_block(Store, Spec, Addr, EndAddr, Count) ->
    case read_var_block(Store, Spec, Addr, EndAddr, Count) of
        {error, _Reason} = Error -> Error;
        not_found ->
            load_var_block(Store, Spec, Addr + Spec#spec.stride,
                           EndAddr, Count + 1);
        Vars ->
            logger:info("Barebox variables from block ~w: ~s",
                        [Count + 1, vars_to_str(Vars)]),
            check_var_block(Store, Spec, Addr + Spec#spec.stride,
                            EndAddr, Vars, Count + 1),
            Vars
    end.

check_var_block(_Store, _Spec, _Addr, _EndAddr, _ExpVars, 3) ->
    ok;
check_var_block(Store, Spec, Addr, EndAddr, ExpVars, Count) ->
    case read_var_block(Store, Spec, Addr, EndAddr, Count) of
        {error, _Reason} = Error -> Error;
        not_found ->
            check_var_block(Store, Spec, Addr + Spec#spec.stride,
                            EndAddr, ExpVars, Count + 1);
        ExpVars ->
            check_var_block(Store, Spec, Addr + Spec#spec.stride,
                            EndAddr, ExpVars, Count + 1);
        OtherVars ->
            logger:warning("Barebox variable block ~w mismatched: ~s",
                           [Count + 1, vars_to_str(OtherVars)]),
            check_var_block(Store, Spec, Addr + Spec#spec.stride,
                            EndAddr, ExpVars, Count + 1)
    end.

read_var_block(Store, Spec, Addr, EndAddr, Count)
  when (Addr + 8) =< EndAddr ->
    try grisp_eeprom:read(Store, Addr, 8) of
        <<Magic:4/binary, _BlockSize:32/little-integer>>
          when Magic =/= ?DIRECT_STORAGE_MAGIC ->
            logger:warning("Barebox variable block ~w with unsupported magic: ~w",
                           [Count + 1, Magic]),
            not_found;
        <<_:4/binary, BlockSize:32/little-integer>>
          when (Addr + 8 + BlockSize) > EndAddr ->
            logger:warning("Barebox variable block ~w is outside of the partition",
                           [Count + 1]),
            not_found;
        <<_:4/binary, BlockSize:32/little-integer>> ->
            try grisp_eeprom:read(Store, Addr + 8, BlockSize) of
                Block ->
                    case parse_var_block(Spec, Block) of
                        {error, Msg} ->
                            logger:warning("Barebox variable block ~w is invalid: ~s",
                                           [Count + 1, Msg]),
                            not_found;
                        {ok, Vars2} ->
                            Vars2
                    end
            catch
                error:Reason -> {error, Reason}
            end
    catch
        error:Reason -> {error, Reason}
    end;
read_var_block(_Store, _Spec, _Addr, _EndAddr, Count) ->
    logger:warning("Barebox variable block ~w is outside of the partition",
                   [Count + 1]),
    not_found.

load_var_block_not_found(#spec{vars = Vars}) ->
    logger:warning("Couldn't find any valid barebox variable set, using default values: ~s",
                   [vars_to_str(Vars)]),
    Vars.

parse_var_block(#spec{vars = Vars} = Spec, Block) ->
    case validate_var_block(Spec, Block) of
        {error, _Msg} = Error -> Error;
        {ok, Data} -> parse_vars(Vars, Data)
    end.

validate_var_block(#spec{magic = SpecMagic}, Block) ->
    <<Header:12/binary, HeaderCrc:32/little-integer, Rest/binary>> = Block,
    case erlang:crc32(Header) of
        HeaderCrc ->
            <<BlockMagic:4/binary, _:16, DataSize:16/little-integer,
              DataCrc:32/little-integer>> = Header,
            if SpecMagic =/= BlockMagic ->
                    {error, "unknown block magic"};
                true ->
                    if DataSize > byte_size(Rest) ->
                            {error, "invalid block size"};
                        true ->
                            <<Data:DataSize/binary, _/binary>> = Rest,
                            case erlang:crc32(Data) of
                                DataCrc ->
                                    {ok, Data};
                                _BadCrc ->
                                    {error, "invalid data CRC"}
                            end
                    end
            end;
        _BadCrc ->
            {error, "invalid header CRC"}
    end.

parse_vars(VarSpec, Data) ->
    parse_vars(VarSpec, Data, []).

parse_vars([], <<>>, Acc) ->
    {ok, lists:reverse(Acc)};
parse_vars([], _ExtraData, _Acc) ->
    {error, "unexpected extra variable data"};
parse_vars([#var{type = uint32} = Var | VarSpec],
           <<V:32/little-integer, Rest/binary>>, Acc) ->
    parse_vars(VarSpec, Rest, [Var#var{value = V} | Acc]);
parse_vars([#var{type = T} | _], _Data, _Acc) ->
    {error, lists:flatten(io_lib:format("unsupported variable type ~w", [T]))}.
