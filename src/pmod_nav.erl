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
        S = #{slot => Slot},
        configure_pins(Slot),
        verify_device(S),
        initialize_device(S)
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
% FIXME: Handle throws in every call
handle_call({config, Comp, Options}, _From, State) ->
    {Result, NewState} = try
        write_config(State, Comp, Options)
    catch
        throw:Reason ->
            {{error, Reason}, State}
    end,
    {reply, Result, NewState};
handle_call({read, Comp, Registers, Opts}, _From, State) ->
    {reply, read_regs(State, Comp, Registers, Opts), State};
handle_call(Request, _From, _State) ->
    error({unknown_call, Request}).

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
    verify_reg(State, acc, who_am_i, <<2#01101000>>),
    verify_reg(State, mag, who_am_i, <<2#00111101>>),
    verify_reg(State, alt, who_am_i, <<2#10111101>>).

verify_reg(State, Comp, Reg, Value) ->
    case read_regs(State, Comp, [Reg], #{}) of
        [Value] -> ok;
        [Other] -> error({register_mismatch, Comp, Reg, Other})
    end.

initialize_device(State) ->
    {_Result, NewState} = write_config(State, acc, #{odr_xl => {hz, 10}}),
    NewState.

write(Slot, Comp, Registers) ->
    [write(Slot, Comp, Reg, Value)|| {Reg, Value} <- Registers],
    ok.

write(Slot, Comp, Reg, Value) ->
    select(Comp, fun() ->
        <<>> = request(Slot, write_request(Comp, Reg, Value), 0)
    end).

write_config(#{slot := Slot} = State, Comp, Options) ->
    Partitions = partition(Comp, Options),
    Cache = maps:with(maps:keys(Partitions), maps:get(Comp, State, #{})),
    Final = subtree:deep_merge(Cache, Partitions),
    Result = write(Slot, Comp, compile(Comp, Final)),
    NewState = subtree:deep_merge(State, #{Comp => Final}),
    {Result, NewState}.

partition(Comp, Options) ->
    maps:fold(fun(K, V, Acc) ->
        Reg = try
            maps:get(K, rev_opts(Comp))
        catch error:{badkey, K} ->
            throw({unknown_option, K})
        end,
        maps:update_with(Reg, fun(M) -> maps:put(K, V, M) end, #{K => V}, Acc)
    end, #{}, Options).

compile(Comp, Options) ->
    maps:fold(fun(Reg, Opts, Acc) -> [render(Comp, Reg, Opts)|Acc] end, [], Options).

render(Comp, Reg, Opts) ->
    {Addr, _Size, Defs} = maps:get(Reg, registers(Comp)),
    {Addr, << <<(render_opt(D, Opts))/bitstring>> || D <- Defs >>}.

render_opt({0, {Size, Default}}, _Opts) ->
    <<Default:Size>>;
render_opt({Opt, {Size, Default, Mapping}}, Opts) ->
    case maps:get(Opt, Opts, Default) of
        Raw when is_integer(Raw) -> <<Raw:Size>>; % TODO: Detect overflow here?
        Alias                    -> <<(maps:get(Alias, Mapping)):Size>>
    end.

read_regs(State, Comp, Registers, Opts) ->
    % FIXME: Not pulling pins between requests creates garbage data?!
    select(Comp, fun() ->
        [read_reg(State, Comp, Reg, Opts) || Reg <- Registers]
    end).

read_reg(#{slot := Slot} = State, Comp, Reg, Opts) ->
    {Addr, Size, Conv} = maps:get(Reg, registers(Comp)),
    Value = request(Slot, read_request(Comp, Addr), Size),
    conv(Comp, Value, State, Opts, Conv).

conv(Comp, Value, State, Opts, Conv) when is_function(Conv) ->
    Conv(Value, {Comp, maps:get(acc, State)}, Opts);
conv(_Comp, Value, _State, _Opts, _Defs) ->
    Value.

setting([Reg, Opt] = Path, {Comp, CompState}) ->
    {_Addr, _RegSize, Defs} = maps:get(Reg, registers(Comp)),
    {Opt, {_OptSize, Default, _Mapping}} = lists:keyfind(Opt, 1, Defs),
    subtree:get(Path, CompState, Default).

write_request(acc, Reg, Value) -> <<?RW_WRITE:1, Reg:7, Value/binary>>.

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

pin(acc) -> ss1;
pin(mag) -> spi1_pin9;
pin(alt) -> spi1_pin10.

rev_opts(acc) ->
    #{
        dec    => ctrl_reg5_xl,
        zen_xl => ctrl_reg5_xl,
        fs_xl  => ctrl_reg6_xl,
        hr     => ctrl_reg6_xl,
        odr_xl => ctrl_reg6_xl
    }.

registers(acc) ->
    #{
        ctrl_reg5_xl => {16#1F, 1, [
            {dec, {2, no_decimation, #{
                no_decimation => 2#00,
                {samples, 2}  => 2#01,
                {samples, 4}  => 2#10,
                {samples, 8}  => 2#11
            }}},
            {zen_xl, {1, true, #{
                true  => 1,
                false => 0
            }}},
            {yen_xl, {1, true, #{
                true  => 1,
                false => 0
            }}},
            {xen_xl, {1, true, #{
                true  => 1,
                false => 0
            }}},
            {0, {3, 2#000}}
        ]},
        ctrl_reg6_xl => {16#20, 1, [
            {odr_xl, {3, power_down, #{
                power_down => 2#000,
                {hz, 10}   => 2#001
            }}},
            {fs_xl, {2, {g, 2}, #{
                {g, 2}  => 2#00,
                {g, 4}  => 2#10,
                {g, 8}  => 2#11,
                {g, 16} => 2#01
            }}},
            {bw_scal_odr, {1, odr, #{
                odr   => 2#0,
                bw_xl => 2#1
            }}},
            {bw_xl, {2, {hz, 408}, #{
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

convert_g(Value, _State, #{unit := raw}) ->
    Value;
convert_g(<<Value:16/signed-little>>, State, Opts) ->
    Scale = case maps:get(unit, Opts, g) of
        g     -> 0.001;
        mg    -> 1;
        Other -> throw({unknown_option, #{unit => Other}})
    end,
    case setting([ctrl_reg6_xl, fs_xl], State) of
        {g, 2}  -> Value * 0.061 * Scale;
        {g, 4}  -> Value * 0.122 * Scale;
        {g, 8}  -> Value * 0.244 * Scale;
        {g, 16} -> Value * 0.732 * Scale;
        _       -> Value
    end.
