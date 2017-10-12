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

config(Comp, Options) when is_map(Options) -> call({config, Comp, Options}).

read(Comp, Registers) -> read(Comp, Registers, #{}).

read(Comp, Registers, Opts) when is_list(Registers) ->
    call({read, Comp, Registers, Opts}).

%--- Callbacks -----------------------------------------------------------------

% @private
init(Slot = spi1) ->
    process_flag(trap_exit, true),
    State = #{
        slot => Slot,
        acc => init_comp(acc),
        mag => init_comp(mag),
        alt => init_comp(alt)
    },
    configure_pins(Slot),
    try
        NewState = verify_device(State),
        initialize_device(NewState),
        grisp_devices:register(Slot, ?MODULE),
        {ok, NewState}
    catch
        Class:Reason ->
            restore_pins(Slot),
            erlang:raise(Class, Reason, erlang:get_stacktrace())
    end;
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

init_comp(Comp) ->
    Regs = registers(Comp),
    #{regs => Regs, rev => reverse_opts(Regs), cache => #{}}.

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
    {_Result, NewState} = write_config(State, acc, #{
        odr_xl => {hz, 10},
        odr_g => {hz, 14.9}
    }),
    NewState.

write_config(State, Comp, Options) ->
    % TODO: Move component upwards?
    Partitions = partition(Options, mapz:deep_get([Comp, rev], State)),
    NewCache = maps:map(fun(Reg, Opts) ->
        Bin = case maps:find(Reg, mapz:deep_get([Comp, cache], State)) of
            {ok, Value} -> Value;
            error       -> read_bin(State, Comp, Reg)
        end,
        {Addr, read_write, _Size, Conv} = mapz:deep_get([Comp, regs, Reg], State),
        NewBin = render_bits(Conv, Bin, Opts),
        write_bin(State, Comp, Addr, NewBin)
    end, Partitions),
    {ok, mapz:deep_put([Comp, cache], NewCache, State)}.

partition(Options, RevOpts) ->
    maps:fold(fun(K, V, Acc) ->
        Reg = case maps:find(K, RevOpts) of
            {ok, Value} -> Value;
            error       -> throw({unknown_option, K})
        end,
        maps:update_with(Reg, fun(M) -> maps:put(K, V, M) end, #{K => V}, Acc)
    end, #{}, Options).

reverse_opts(Registers) ->
    maps:fold(fun
        (Reg, {_Addr, read_write, _Size, Conv}, Rev) when is_list(Conv) ->
            lists:foldl(fun(D, R) ->
                maps:put(D, Reg, R)
            end, Rev, proplists:get_keys(Conv));
        (_Reg, _Spec, Rev) ->
            Rev
    end, #{}, Registers).

read_and_convert(State, Comp, Registers, Opts) ->
    {Values, NewState} = read_regs(State, Comp, Registers),
    convert_regs(NewState, Comp, Opts, Values).

read_regs(State, Comp, Registers) ->
    % TODO: Move comp higher?
    % FIXME: Not pulling pins between requests creates garbage data?
    {Values, NewCache} = lists:foldl(fun(Reg, {Acc, Cache}) ->
        select(Comp, fun() ->
            Value = read_bin(State, Comp, Reg),
            NewC = case mapz:deep_get([Comp, regs, Reg], State) of
                {_Addr, _Size, Conv} when is_list(Conv) ->
                    maps:put(Reg, Value, Cache);
                _ReadOnlyReg ->
                    Cache
            end,
            {[{Reg, Value}|Acc], NewC}
        end)
    end, {[], mapz:deep_get([Comp, cache], State)}, Registers),
    {lists:reverse(Values), mapz:deep_put([Comp, cache], NewCache, State)}.

convert_regs(State, Comp, Opts, Values) ->
    {Results, NewState} = lists:foldl(fun(R, {Acc, S}) ->
        {Result, NewS} = convert_reg(S, Comp, Opts, R),
        {[Result|Acc], NewS}
    end, {[], State}, Values),
    {lists:reverse(Results), NewState}.

convert_reg(State, _Comp, #{unit := raw}, {_Reg, Value}) ->
    {Value, State};
convert_reg(State, Comp, Opts, {Reg, Value}) ->
    % TODO: Move component upwards
    case mapz:deep_get([Comp, regs, Reg], State) of
        {_Addr, _RW, _Size, Conv} when is_function(Conv) ->
            {Converted, {Comp, NewState}} = Conv(Value, {Comp, State}, Opts),
            {Converted, NewState};
        {_Addr, _RW, _Size, Conv} when is_list(Conv) ->
            % FIXME: Put reg in cache here instead of in read_regs!!!?
            NewS = mapz:deep_put([Comp, cache, Reg], Value, State),
            {parse_bits(Conv, Value), NewS};
        {_Addr, _RW, _Size, raw} ->
            {Value, State}
    end.

write_bin(#{slot := Slot}, Comp, Reg, Value) ->
    select(Comp, fun() ->
        <<>> = request(Slot, write_request(Comp, Reg, Value), 0)
    end),
    Value.

read_bin(#{slot := Slot} = State, Comp, Reg) ->
    case mapz:deep_find([Comp, regs, Reg], State) of
        {ok, {Addr, _RW, Size, _Conv}} ->
            request(Slot, read_request(Comp, Addr), Size);
        error ->
            throw({unknown_register, Comp, Reg})
    end.

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

render_bits([{Name, Size, Mapping}|Defs], Bin, Opts, Pos) ->
    NewBin = case {maps:find(Name, Opts), Mapping} of
        {{ok, Value}, raw} when is_integer(Value) ->
            % TODO: Detect value overflow in regards to Size here?
            grisp_bitmap:set_bits(Bin, Pos, <<Value:Size>>);
        {{ok, Value}, raw} when is_bitstring(Value), bit_size(Value) == Size ->
            grisp_bitmap:set_bits(Bin, Pos, Value);
        {{ok, Value}, raw} ->
            throw({invalid_option, Name, Value});
        {{ok, Value}, Mapping} ->
            case maps:find(Value, Mapping) of
                {ok, Real} -> grisp_bitmap:set_bits(Bin, Pos, <<Real:Size>>);
                error      -> throw({invalid_option, Name, Value})
            end;
        _ ->
            Bin
    end,
    render_bits(Defs, NewBin, Opts, Pos + Size);
render_bits([{0, Size}|Defs], Bin, Opts, Pos) ->
    NewBin = grisp_bitmap:set_bits(Bin, Pos, <<0:Size>>),
    render_bits(Defs, NewBin, Opts, Pos + Size);
render_bits([], Bin, _Opts, Pos) when bit_size(Bin) == Pos ->
    Bin.

parse_bits(Conv, Bin) -> parse_bits(Conv, Bin, #{}).

parse_bits([{Name, Size, raw}|Conv], Bin, Opts) ->
    <<Val:Size/bitstring, Rest/bitstring>> = Bin,
    parse_bits(Conv, Rest, maps:put(Name, Val, Opts));
parse_bits([{Name, Size, Defs}|Conv], Bin, Opts) ->
    <<Val:Size, Rest/bitstring>> = Bin,
    Values = mapz:inverse(Defs),
    parse_bits(Conv, Rest, maps:put(Name, maps:get(Val, Values), Opts));
parse_bits([{0, Size}|Conv], Bin, Opts) ->
    <<_:Size, Rest/bitstring>> = Bin,
    parse_bits(Conv, Rest, Opts);
parse_bits([], <<>>, Opts) ->
    Opts.

setting(Reg, Opt, {Comp, State}) ->
    {_Addr, _RW, _RegSize, Conv} = mapz:deep_get([Comp, regs, Reg], State),
    {Parsed, NewState} = case mapz:deep_find([Comp, cache, Reg], State) of
        {ok, Cached} -> {[parse_bits(Conv, Cached)], State};
        error        -> read_and_convert(State, Comp, [Reg], #{})
    end,
    {maps:get(Opt, hd(Parsed)), {Comp, NewState}}.

pin(acc) -> ss1;
pin(mag) -> spi1_pin9;
pin(alt) -> spi1_pin10.

registers(acc) ->
    #{
        act_ths => {16#04, read_write, 1, [
            {sleep_on_inact_en, 1, #{
                gyroscope_power_down => 0,
                gyroscope_sleep => 1
            }},
            {act_ths, 7, raw}
        ]},
        act_dur => {16#05, read_write, 1, [{act_dur, 8, raw}]},
        int_gen_cfg_xl => {16#06, read_write, 1, [
            {aoi_xl,  1, #{or_combination => 0, and_combination => 1}},
            {'6d',    1, #{disabled => 0, enabled => 1}},
            {zhie_xl, 1, #{disabled => 0, enabled => 1}},
            {zlie_xl, 1, #{disabled => 0, enabled => 1}},
            {yhie_xl, 1, #{disabled => 0, enabled => 1}},
            {ylie_xl, 1, #{disabled => 0, enabled => 1}},
            {xhie_xl, 1, #{disabled => 0, enabled => 1}},
            {xlie_xl, 1, #{disabled => 0, enabled => 1}}
        ]},
        int_gen_ths_x_xl => {16#07, read_write, 1, [{ths_xl_x, 8, raw}]},
        int_gen_ths_y_xl => {16#08, read_write, 1, [{ths_xl_y, 8, raw}]},
        int_gen_ths_z_xl => {16#09, read_write, 1, [{ths_xl_z, 8, raw}]},
        int_gen_dur_xl => {16#0A, read_write, 1, [
            {wait_xl, 1, #{off => 0, on => 1}},
            {dur_xl, 7, raw}
        ]},
        reference_g => {16#0B, read_write, 1, [{ref_g, 8, raw}]},
        int1_ctrl => {16#0C, read_write, 1, [
            {int1_ig_g,   1, #{disabled => 0, enabled => 1}},
            {int_ig_xl,   1, #{disabled => 0, enabled => 1}},
            {int_fss5,    1, #{disabled => 0, enabled => 1}},
            {int_ovr,     1, #{disabled => 0, enabled => 1}},
            {int_fth,     1, #{disabled => 0, enabled => 1}},
            {int_boot,    1, #{disabled => 0, enabled => 1}},
            {int_drdy_g,  1, #{disabled => 0, enabled => 1}},
            {int_drdy_xl, 1, #{disabled => 0, enabled => 1}}
        ]},
        int2_ctrl => {16#0D, read_write, 1, [
            {int2_inact,     1, #{false => 0, true => 1}},
            {0,              1},
            {int2_fss5,      1, #{disabled => 0, enabled => 1}},
            {int2_ovr,       1, #{disabled => 0, enabled => 1}},
            {int2_fth,       1, #{disabled => 0, enabled => 1}},
            {int2_drdy_temp, 1, #{disabled => 0, enabled => 1}},
            {int2_drdy_g,    1, #{disabled => 0, enabled => 1}},
            {int2_drdy_xl,   1, #{disabled => 0, enabled => 1}}
        ]},
        who_am_i => {16#0F, read, 1, raw},
        ctrl_reg1_g => {16#10, read_write, 1, [
            {odr_g, 3, #{
                power_down => 2#000,
                {hz, 14.9} => 2#001,
                {hz, 59.5} => 2#010,
                {hz, 119}  => 2#011,
                {hz, 238}  => 2#100,
                {hz, 476}  => 2#101,
                {hz, 952}  => 2#110
            }},
            {fs_g, 2, #{
                {dps, 245}  => 2#00,
                {dps, 500}  => 2#01,
                {dps, 2000} => 2#11
            }},
            {0, 1},
            {bw_g, 2, raw}
        ]},
        ctrl_reg2_g => {16#11, read_write, 1, [
            {0,       4},
            {int_sel, 2, raw},
            {out_sel, 2, raw}
        ]},
        ctrl_reg3_g => {16#12, read_write, 1, [
            {lp_mode, 1, #{disabled => 0, enabled => 1}},
            {hp_en,   1, #{disabled => 0, enabled => 1}},
            {0,       2},
            {hpcf_g,  4, raw}
        ]},
        orient_cfg_g => {16#13, read_write, 1, [
            {0,       2},
            {signx_g, 1, #{positive => 0, negative => 1}},
            {signy_g, 1, #{positive => 0, negative => 1}},
            {signz_g, 1, #{positive => 0, negative => 1}},
            {orient,  3, raw}
        ]},
        int_gen_src_g => {16#14, read, 1, [
            {0,    1},
            {ia_g, 1, #{false => 0, true => 1}},
            {zh_g, 1, #{false => 0, true => 1}},
            {zl_g, 1, #{false => 0, true => 1}},
            {yh_g, 1, #{false => 0, true => 1}},
            {yl_g, 1, #{false => 0, true => 1}},
            {xh_g, 1, #{false => 0, true => 1}},
            {xl_g, 1, #{false => 0, true => 1}}
        ]},
        out_temp => {16#15, read, 2, fun convert_temp/3},
        out_temp_l => {16#15, read, 1, raw},
        out_temp_h => {16#16, read, 1, raw},
        status_reg => {16#17, read, 1, [
            {0,           1},
            {ig_xl,       1, #{false => 0, true => 1}},
            {ig_g,        1, #{false => 0, true => 1}},
            {inact,       1, #{false => 0, true => 1}},
            {boot_status, 1, #{no_boot_running => 0, boot_running => 1}},
            {tda,         1, #{false => 0, true => 1}},
            {gda,         1, #{false => 0, true => 1}},
            {xlda,        1, #{false => 0, true => 1}}
        ]},
        out_x_g => {16#18, read, 2, fun convert_dps/3},
        out_y_g => {16#18, read, 2, fun convert_dps/3},
        out_z_g => {16#18, read, 2, fun convert_dps/3},
        ctrl_reg4 => {16#1E, read_write, 1, [
            {0,        2},
            {zen_g,    1, #{disabled => 0, enabled => 1}},
            {yen_g,    1, #{disabled => 0, enabled => 1}},
            {xen_g,    1, #{disabled => 0, enabled => 1}},
            {0,        1},
            {lir_xl1,  1, #{not_latched => 0, latched => 1}},
            {'4d_xl1', 1, #{'6d' => 0, '4d' => 1}}
        ]},
        ctrl_reg5_xl => {16#1F, read_write, 1, [
            {dec, 2, #{
                no_decimation => 2#00,
                {samples, 2}  => 2#01,
                {samples, 4}  => 2#10,
                {samples, 8}  => 2#11
            }},
            {zen_xl, 1, #{disabled => 0, enabled  => 1}},
            {yen_xl, 1, #{disabled => 0, enabled  => 1}},
            {xen_xl, 1, #{disabled => 0, enabled  => 1}},
            {0, 3}
        ]},
        ctrl_reg6_xl => {16#20, read_write, 1, [
            {odr_xl, 3, #{
                power_down => 2#000,
                {hz, 10}   => 2#001,
                {hz, 50}   => 2#010,
                {hz, 119}  => 2#011,
                {hz, 238}  => 2#100,
                {hz, 476}  => 2#101,
                {hz, 952}  => 2#110
            }},
            {fs_xl, 2, #{
                {g, 2}  => 2#00,
                {g, 4}  => 2#10,
                {g, 8}  => 2#11,
                {g, 16} => 2#01
            }},
            {bw_scal_odr, 1, #{odr => 2#0, bw_xl => 2#1}},
            {bw_xl, 2, #{
                {hz, 408} => 2#00,
                {hz, 211} => 2#01,
                {hz, 105} => 2#10,
                {hz, 50}  => 2#11
            }}
        ]},
        ctrl_reg7_xl => {16#21, read_write, 1, [
            {hr,    1, #{disabled => 0, enabled => 1}},
            {dcf,   2, raw},
            {0,     2},
            {fds,   1, #{disabled => 0, enabled => 1}},
            {0,     1},
            {hpis1, 1, #{disabled => 0, enabled => 1}}
        ]},
        ctr_reg8 => {16#22, read_write, 1, [
            {boot,       1, #{normal => 0, reboot_memory => 1}},
            {bdu,        1, #{continious => 0, read => 1}},
            {h_lactive,  1, #{high => 0, low => 1}},
            {pp_od,      1, #{push_pull => 0, open_drain => 1}},
            {sim,        1, #{'4-wire' => 0, '3-wire' => 1}},
            {if_add_inc, 1, #{disabled => 0, enabled => 1}},
            {ble,        1, #{lsb => 0, msb => 1}},
            {sw_reset,   1, #{normal => 0, reset => 1}}
        ]},
        ctrl_reg9 => {16#23, read_write, 1, [
            {0, 1},
            {sleep_g, 1, #{disabled => 0, enabled => 1}},
            {0, 1},
            {fifo_temp_en, 1, #{disabled => 0, enabled => 1}},
            {drdy_mask_bit, 1, #{disabled => 0, enabled => 1}},
            {i2c_disable, 1, #{false => 0, true => 1}},
            {fifo_en, 1, #{disabled => 0, enabled => 1}},
            {stop_on_fth, 1, #{not_limited => 0, threshold => 1}}
        ]},
        ctrl_reg10 => {16#24, read_write, 1, [
            {0,     5},
            {st_g,  1, #{disabled => 0, enabled => 1}},
            {0,     1},
            {st_xl, 1, #{disabled => 0, enabled => 1}}
        ]},
        int_gen_src_xl => {16#26, read, 1, [
            {0, 1},
            {ia_xl, 1, #{false => 1, true => 1}},
            {zh_xl, 1, #{false => 1, true => 1}},
            {zl_xl, 1, #{false => 1, true => 1}},
            {yh_xl, 1, #{false => 1, true => 1}},
            {yl_xl, 1, #{false => 1, true => 1}},
            {xh_xl, 1, #{false => 1, true => 1}},
            {xl_xl, 1, #{false => 1, true => 1}}
        ]},
        status_reg2 => {16#27, read, 1, [
            {0,           1},
            {ig_xl,       1, #{false => 0, true => 1}},
            {ig_g,        1, #{false => 0, true => 1}},
            {inact,       1, #{false => 0, true => 1}},
            {boot_status, 1, #{no_boot_running => 0, boot_running => 1}},
            {tda,         1, #{false => 0, true => 1}},
            {gda,         1, #{false => 0, true => 1}},
            {xlda,        1, #{false => 0, true => 1}}
        ]},
        out_x_xl   => {16#28, read, 2, fun convert_g/3},
        out_x_l_xl => {16#28, read, 1, raw},
        out_x_h_xl => {16#29, read, 1, raw},
        out_y_xl   => {16#2A, read, 2, fun convert_g/3},
        out_y_l_xl => {16#2A, read, 1, raw},
        out_y_h_xl => {16#2B, read, 1, raw},
        out_z_xl   => {16#2C, read, 2, fun convert_g/3},
        out_z_l_xl => {16#2C, read, 1, raw},
        out_z_h_xl => {16#2D, read, 1, raw}
    };
registers(mag) ->
    #{
        who_am_i => {16#0F, read, 1, raw}
    };
registers(alt) ->
    #{
        who_am_i => {16#0F, read, 1, raw}
    }.

convert_g(<<Value:16/signed-little>> = Raw, Context, Opts) ->
    Scale = case maps:get(xl_unit, Opts, g) of
        g     -> 0.001;
        mg    -> 1.0;
        Other -> throw({unknown_option, #{xl_unit => Other}})
    end,
    {FS, NewContext} = setting(ctrl_reg6_xl, fs_xl, Context),
    Result = case FS of
        {g, 2}  -> Value * 0.061 * Scale;
        {g, 4}  -> Value * 0.122 * Scale;
        {g, 8}  -> Value * 0.244 * Scale;
        {g, 16} -> Value * 0.732 * Scale;
        _       -> Raw
    end,
    {Result, NewContext}.

convert_temp(<<Value:16/signed-little>>, Context, _Opts) ->
    {Value / 16 + 25, Context}.

convert_dps(<<Value:16/signed-little>> = Raw, Context, Opts) ->
    Scale = case maps:get(g_unit, Opts, dps) of
        dps   -> 0.001;
        mdps  -> 1.0;
        Other -> throw({unknown_option, #{g_unit => Other}})
    end,
    {AR, NewContext} = setting(ctrl_reg1_g, fs_g, Context),
    Result = case AR of
        {dps, 245}  -> Value * 8.75  * Scale;
        {dps, 500}  -> Value * 17.50 * Scale;
        {dps, 2000} -> Value * 70.0  * Scale;
        _           -> Raw
    end,
    {Result, NewContext}.
