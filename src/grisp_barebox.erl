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
        magic = <<166,134,59,136>>,
        start = 0,
        size = 256,
        stride = 64,
        vars = [
            #var{name = [last_chosen], type = uint32, value = 0},
            #var{name = [system0, remaining_attempts], type = uint32, value = 3},
            #var{name = [system0, priority], type = uint32, value = 21},
            #var{name = [system0, ok], type = uint32, value = 0},
            #var{name = [system1, remaining_attempts], type = uint32, value = 3},
            #var{name = [system1, priority], type = uint32, value = 20},
            #var{name = [system1, ok], type = uint32, value = 0}
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

set_var(_State, _Name, _Value) ->
    {error, not_implemented}.

store_vars(_State) ->
    {error, not_implemented}.

load_var_block(_Store, Spec, _Addr, _EndAddr, 3) ->
    load_var_block_not_found(Spec);
load_var_block(Store, Spec, Addr, EndAddr, Count)
  when (Addr + 8) =< EndAddr ->
    case grisp_eeprom:read(Store, Addr, 8) of
        <<Magic:4/binary, _BlockSize:32/little-integer>>
          when Magic =/= ?DIRECT_STORAGE_MAGIC ->
            logger:warning("Barebox variable block ~w with unsupported magic: ~w",
                           [Count + 1, Magic]),
            load_var_block(Store, Spec, Addr + Spec#spec.stride,
                           EndAddr, Count + 1);
        <<_:4/binary, BlockSize:32/little-integer>>
          when (Addr + 8 + BlockSize) > EndAddr ->
            logger:warning("Barebox variable block ~w is outside of the partition",
                           [Count + 1]),
            load_var_block_not_found(Spec);
        <<_:4/binary, BlockSize:32/little-integer>> ->
            Block = grisp_eeprom:read(Store, Addr + 8, BlockSize),
            case parse_var_block(Spec, Block) of
                {error, Msg} ->
                    logger:warning("Barebox variable block ~w is invalid: ~s",
                                   [Count + 1, Msg]),
                    load_var_block(Store, Spec, Addr + Spec#spec.stride,
                                   EndAddr, Count + 1);
                {ok, Vars2} ->
                    logger:info("Barebox variables from block ~w: ~s",
                                [Count + 1, vars_to_str(Vars2)]),
                    Vars2
            end
    end;
load_var_block(_Store, Spec, _Addr, _EndAddr, _Count) ->
    load_var_block_not_found(Spec).

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
