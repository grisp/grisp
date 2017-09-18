-module(pmod_nav).

-behavior(gen_server).

% API
-export([start_link/1]).
-export([config/2]).
-export([read/2]).
-export([read/3]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("grisp.hrl").
-include("pmod_nav.hrl").

-define(SPI_MODE, #{cpol => high, cpha => trailing}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot) -> gen_server:start_link(?MODULE, Slot, []).

config(Comp, Options) -> call({config, Comp, Options}).

read(Comp, Registers) -> read(Comp, Registers, #{}).

read(Comp, Registers, Opts) -> call({read, Comp, Registers, Opts}).

%--- Callbacks -----------------------------------------------------------------

% @private
init(Slot = spi1) ->
    State = try
        S = #{
            slot => Slot,
            % FIXME: Change to #{regs => , rev => , cache => }
            acc => {{registers(acc), reverse_opts(registers(acc))}, #{}},
            mag => {{registers(mag), reverse_opts(registers(mag))}, #{}},
            alt => {{registers(alt), reverse_opts(registers(alt))}, #{}}
        },
        configure_pins(Slot),
        NewS = verify_device(S),
        initialize_device(NewS)
    catch
        Class:Reason ->
            restore_pins(Slot),
            erlang:raise(Class, Reason, erlang:get_stacktrace())
    end,
    grisp_devices:register(Slot, ?MODULE),
    {ok, State};
init(Slot) ->
    error({incompatible_slot, Slot}).

% @private
handle_call(Call, _From, State) ->
    try execute_call(Call, State)
    catch throw:Reason -> {reply, {error, Reason}, State}
    end.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, #{slot := Slot}) -> restore_pins(Slot).

%--- Internal ------------------------------------------------------------------

call(Call) ->
    Dev = grisp_devices:default(?MODULE),
    case gen_server:call(Dev#device.pid, Call) of
        {error, Reason} -> error(Reason);
        Result          -> Result
    end.

execute_call({config, Comp, Options}, State) ->
    {Result, NewState} = write_config(State, Comp, Options),
    {reply, Result, NewState};
execute_call({read, Comp, Registers, Opts}, State) ->
    {Result, NewState} = read_and_convert(State, Comp, Registers, Opts),
    {reply, Result, NewState};
execute_call(Request, _State) ->
    error({unknown_call, Request}).

configure_pins(Slot) ->
    % Disable chip select for SPI1
    grisp_gpio:configure_slot(Slot, disable_cs),
    % Configure pin 9 and 10 for output pulled high
    grisp_gpio:configure(spi1_pin9, output_1),
    grisp_gpio:configure(spi1_pin10, output_1).

restore_pins(Slot) ->
    grisp_gpio:configure_slot(Slot, enable_cs),
    grisp_gpio:configure(spi1_pin9, input),
    grisp_gpio:configure(spi1_pin10, input).

verify_device(State) ->
    lists:foldl(fun verify_reg/2, State, [
        {acc, who_am_i, <<2#01101000>>},
        {mag, who_am_i, <<2#00111101>>},
        {alt, who_am_i, <<2#10111101>>}
    ]).

verify_reg({Comp, Reg, Expected}, State) ->
    case read_and_convert(State, Comp, [Reg], #{}) of
        {[Expected], NewState} ->
            NewState;
        {[Other], _NewState} ->
            error({register_mismatch, Comp, Reg, Other})
    end.

initialize_device(State) ->
    {_Result, NewState} = write_config(State, acc, #{odr_xl => {hz, 10}}),
    NewState.

write_config(State, Comp, Options) ->
    {{Defs, RevOpts}, Cache} = maps:get(Comp, State),
    Partitions = partition(Options, RevOpts),
    NewCache = maps:map(fun(Reg, Opts) ->
        Bin = case maps:find(Reg, Cache) of
            {ok, Value} -> Value;
            error       -> read_bin(State, Comp, Reg)
        end,
        {Addr, _Size, Conv} = maps:get(Reg, Defs),
        NewBin = render_bits(Conv, Bin, Opts),
        write_bin(State, Comp, Addr, NewBin)
    end, Partitions),
    {ok, mapz:deep_merge(State, #{Comp => {{Defs, RevOpts}, NewCache}})}.

partition(Options, RevOpts) ->
    maps:fold(fun(K, V, Acc) ->
        Reg = try
            maps:get(K, RevOpts)
        catch error:{badkey, K} ->
            throw({unknown_option, K})
        end,
        maps:update_with(Reg, fun(M) -> maps:put(K, V, M) end, #{K => V}, Acc)
    end, #{}, Options).

reverse_opts(Registers) ->
    maps:fold(fun
        (Reg, {_Addr, _Size, Defs}, Rev) when is_list(Defs) ->
            lists:foldl(fun(D, R) ->
                maps:put(D, Reg, R)
            end, Rev, proplists:get_keys(Defs));
        (_Reg, _Spec, Rev) ->
            Rev
    end, #{}, Registers).

read_and_convert(State, Comp, Registers, Opts) ->
    {Values, NewState} = read_regs(State, Comp, Registers),
    convert_regs(NewState, Comp, Opts, Values).

read_regs(State, Comp, Registers) ->
    {{Defs, RevOpts}, Cache} = maps:get(Comp, State),
    % FIXME: Not pulling pins between requests creates garbage data?
    {Values, NewCache} = lists:foldl(fun(Reg, {Acc, C}) ->
        select(Comp, fun() ->
            Value = read_bin(State, Comp, Reg),
            NewC = case maps:get(Reg, Defs) of
                {_Addr, _Size, Conv} when is_list(Conv) ->
                    maps:put(Reg, Value, C);
                _ReadOnlyReg ->
                    C
            end,
            {[{Reg, Value}|Acc], NewC}
        end)
    end, {[], Cache}, Registers),
    {lists:reverse(Values), maps:put(Comp, {{Defs, RevOpts}, NewCache}, State)}.

convert_regs(State, Comp, Opts, Values) ->
    {Results, NewState} = lists:foldl(fun(R, {Acc, S}) ->
        {Result, NewS} = convert_reg(S, Comp, Opts, R),
        {[Result|Acc], NewS}
    end, {[], State}, Values),
    {lists:reverse(Results), NewState}.

convert_reg(State, _Comp, #{unit := raw}, {_Reg, Value}) ->
    {Value, State};
convert_reg(State, Comp, Opts, {Reg, Value}) ->
    % FIXME: Move component upwards
    {{Defs, RevOpts}, Cache} = maps:get(Comp, State),
    case maps:get(Reg, Defs) of
        {_Addr, _Size, Conv} when is_function(Conv) ->
            Conv(Value, {Comp, State}, Opts);
        {_Addr, _Size, Conv} when is_list(Conv) ->
            % FIXME: Put reg in cache here instead of in read_regs!!!?
            NewCache = maps:put(Reg, Value, Cache),
            NewS = maps:put(Comp, {{Defs, RevOpts}, NewCache}, State),
            {parse_bits(Conv, Value), NewS};
        {_Addr, _Size, raw} ->
            {Value, State}
    end.

write_bin(#{slot := Slot}, Comp, Reg, Value) ->
    select(Comp, fun() ->
        <<>> = request(Slot, write_request(Comp, Reg, Value), 0)
    end),
    Value.

read_bin(#{slot := Slot} = State, Comp, Reg) ->
    {{Registers, _RevOpts}, _Cache} = maps:get(Comp, State),
    {Addr, Size, _Conv} = maps:get(Reg, Registers),
    request(Slot, read_request(Comp, Addr), Size).

write_request(acc, Reg, Val) -> <<?RW_WRITE:1, Reg:7, Val/binary>>;
write_request(mag, Reg, Val) -> <<?RW_WRITE:1, ?MS_INCR:1, Reg:6, Val/binary>>;
write_request(alt, Reg, Val) -> <<?RW_WRITE:1, ?MS_INCR:1, Reg:6, Val/binary>>.

read_request(acc, Reg) -> <<?RW_READ:1, Reg:7>>;
read_request(mag, Reg) -> <<?RW_READ:1, ?MS_SAME:1, Reg:6>>;
read_request(alt, Reg) -> <<?RW_READ:1, ?MS_SAME:1, Reg:6>>.

request(Slot, Request, Pad) ->
    grisp_spi:send_recv(Slot, ?SPI_MODE, Request, byte_size(Request), Pad).

select(Component, Fun) ->
    Pin = pin(Component),
    try
        grisp_gpio:clear(Pin),
        Fun()
    after
        grisp_gpio:set(Pin)
    end.

render_bits(Defs, Bin, Opts) ->
    render_bits(Defs, Bin, Opts, 0).

render_bits([{Name, {Size, Mapping}}|Defs], Bin, Opts, Pos) ->
    NewBin = case maps:find(Name, Opts) of
        {ok, Value} ->
            % TODO: Detect value overflow in regards to Size here?
            Real = case maps:find(Value, Mapping) of
                {ok, V} -> V;
                error when is_integer(Value) -> Value;
                error -> throw({invalid_option, Name, Value})
            end,
            grisp_bitmap:set_bits(Bin, Pos, <<Real:Size>>);
        error ->
            Bin
    end,
    render_bits(Defs, NewBin, Opts, Pos + Size);
render_bits([], Bin, _Opts, Pos) when bit_size(Bin) == Pos ->
    Bin.

parse_bits(Conv, Bin) -> parse_bits(Conv, Bin, #{}).

parse_bits([{Name, {Size, Map}}|Conv], Bin, Opts) ->
    <<Val:Size, Rest/bitstring>> = Bin,
    Values = mapz:inverse(Map),
    parse_bits(Conv, Rest, maps:put(Name, maps:get(Val, Values), Opts));
parse_bits([{0, {Size}}|Conv], Bin, Opts) ->
    <<_:Size, Rest/bitstring>> = Bin,
    parse_bits(Conv, Rest, Opts);
parse_bits([], <<>>, Opts) ->
    Opts.

setting(Reg, Opt, {Comp, State}) ->
    {{Registers, _RevOpts}, Cache} = maps:get(Comp, State),
    {_Addr, _RegSize, Defs} = maps:get(Reg, Registers),
    {Parsed, NewState} = case maps:find(Reg, Cache) of
        {ok, Cached} -> {parse_bits(Defs, Cached), State};
        error        -> read_and_convert(State, Comp, [Reg], #{})
    end,
    {maps:get(Opt, Parsed), NewState}.

pin(acc) -> ss1;
pin(mag) -> spi1_pin9;
pin(alt) -> spi1_pin10.

registers(acc) ->
    #{
        ctrl_reg5_xl => {16#1F, 1, [
            {dec, {2, #{
                no_decimation => 2#00,
                {samples, 2}  => 2#01,
                {samples, 4}  => 2#10,
                {samples, 8}  => 2#11
            }}},
            {zen_xl, {1, #{
                true  => 1,
                false => 0
            }}},
            {yen_xl, {1, #{
                true  => 1,
                false => 0
            }}},
            {xen_xl, {1, #{
                true  => 1,
                false => 0
            }}},
            {0, {3}}
        ]},
        ctrl_reg6_xl => {16#20, 1, [
            {odr_xl, {3, #{
                power_down => 2#000,
                {hz, 10}   => 2#001
            }}},
            {fs_xl, {2, #{
                {g, 2}  => 2#00,
                {g, 4}  => 2#10,
                {g, 8}  => 2#11,
                {g, 16} => 2#01
            }}},
            {bw_scal_odr, {1, #{
                odr   => 2#0,
                bw_xl => 2#1
            }}},
            {bw_xl, {2, #{
                {hz, 408} => 2#00,
                {hz, 211} => 2#01,
                {hz, 105} => 2#10,
                {hz, 50}  => 2#11
            }}}
        ]},
        out_x_xl   => {16#28, 2, fun convert_g/3},
        out_x_l_xl => {16#28, 1, raw},
        out_x_h_xl => {16#29, 1, raw},
        out_y_xl   => {16#2A, 2, fun convert_g/3},
        out_z_xl   => {16#2C, 2, fun convert_g/3},
        who_am_i   => {16#0F, 1, raw}
    };
registers(mag) ->
    #{
        who_am_i => {16#0F, 1, raw}
    };
registers(alt) ->
    #{
        who_am_i => {16#0F, 1, raw}
    }.

convert_g(<<Value:16/signed-little>>, State, Opts) ->
    Scale = case maps:get(unit, Opts, g) of
        g     -> 0.001;
        mg    -> 1;
        Other -> throw({unknown_option, #{unit => Other}})
    end,
    {FS, NewState} = setting(ctrl_reg6_xl, fs_xl, State),
    Result = case FS of
        {g, 2}  -> Value * 0.061 * Scale;
        {g, 4}  -> Value * 0.122 * Scale;
        {g, 8}  -> Value * 0.244 * Scale;
        {g, 16} -> Value * 0.732 * Scale;
        _       -> Value
    end,
    {Result, NewState}.
