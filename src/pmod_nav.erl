-module(pmod_nav).

-behavior(gen_server).

% API
-export([start_link/2]).
-export([config/2]).
-export([read/2]).
-export([read/3]).
-export([registers/0]).
-export([registers/1]).

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
start_link(Slot, Opts) -> gen_server:start_link(?MODULE, [Slot, Opts], []).

config(Comp, Options) when is_map(Options) -> call({config, Comp, Options}).

read(Comp, Registers) -> read(Comp, Registers, #{}).

read(Comp, Registers, Opts) when is_list(Registers) ->
    call({read, Comp, Registers, Opts}).

%--- Callbacks -----------------------------------------------------------------

% @private
init([Slot = spi1, Opts]) ->
    process_flag(trap_exit, true),
    State = #{
        slot => Slot,
        acc => init_comp(acc),
        mag => init_comp(mag),
        alt => init_comp(alt),
        debug => maps:get(debug, Opts, false)
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
        {acc, who_am_i,   <<2#01101000>>},
        {mag, who_am_i_m, <<2#00111101>>},
        {alt, who_am_i,   <<2#10111101>>}
    ]).

verify_reg({Comp, Reg, Expected}, State) ->
    case read_and_convert(State, Comp, [Reg], #{}) of
        {[Expected], NewState} ->
            NewState;
        {[Other], _NewState} ->
            error({register_mismatch, Comp, Reg, Other})
    end.

initialize_device(State) ->
    lists:foldl(fun({Comp, Opts}, S) ->
        {_R, NewS} = write_config(S, Comp, Opts),
        NewS
    end, State, [
        {acc, #{
            odr_xl => {hz, 10},
            fs_xl  => {g, 2},
            odr_g  => {hz, 14.9}
        }},
        {mag, #{
            md  => continuous_conversion,
            om  => ultra_high,
            omz => ultra_high
        }},
        {alt, #{pd => active}},
        {alt, #{odr => {hz, 25}}}
    ]).

write_config(State, Comp, Options) ->
    % TODO: Move component upwards
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
            lists:foldl(fun
                (0, R) -> R;
                (D, R) -> maps:put(D, Reg, R)
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
                {_Addr, read_write, _Size, Conv} when is_list(Conv) ->
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
        {_Addr, _RW, _Size, Type} ->
            {decode(Type, Value), State}
    end.

write_bin(#{slot := Slot, debug := Debug}, Comp, Reg, Value) ->
    select(Comp, fun() ->
        [debug_write(Comp, Reg, Value) || Debug],
        <<>> = request(Slot, write_request(Comp, Reg, Value), 0)
    end),
    Value.

read_bin(#{slot := Slot, debug := Debug} = State, Comp, Reg) ->
    case mapz:deep_find([Comp, regs, Reg], State) of
        {ok, {Addr, _RW, Size, _Conv}} ->
            Result = request(Slot, read_request(Comp, Addr), Size),
            [debug_read(Comp, Addr, Result) || Debug],
            Result;
        error ->
            throw({unknown_register, Comp, Reg})
    end.

write_request(acc, Reg, Val) -> <<?RW_WRITE:1, Reg:7, Val/binary>>;
write_request(mag, Reg, Val) -> <<?RW_WRITE:1, ?MS_INCR:1, Reg:6, Val/binary>>;
write_request(alt, Reg, Val) -> <<?RW_WRITE:1, ?MS_INCR:1, Reg:6, Val/binary>>.

read_request(acc, Reg) -> <<?RW_READ:1, Reg:7>>;
read_request(mag, Reg) -> <<?RW_READ:1, ?MS_INCR:1, Reg:6>>;
read_request(alt, Reg) -> <<?RW_READ:1, ?MS_INCR:1, Reg:6>>.

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
    NewBin = case maps:find(Name, Opts) of
        {ok, Value} ->
            Bits = case {Value, Mapping} of
                {<<Value/bitstring>>, _} when bit_size(Value) == Size ->
                    Value;
                {Value, Mapping} when is_map(Mapping) ->
                    case maps:find(Value, Mapping) of
                        {ok, Mapped} -> <<Mapped:Size>>;
                        error        -> throw({invalid_value, Name, Value})
                    end;
                {Value, Type} ->
                    encode(Name, Type, Size, Value)
            end,
            grisp_bitmap:set_bits(Bin, Pos, Bits);
        error ->
            Bin
    end,
    render_bits(Defs, NewBin, Opts, Pos + Size);
render_bits([{0, Size}|Defs], Bin, Opts, Pos) ->
    NewBin = grisp_bitmap:set_bits(Bin, Pos, <<0:Size>>),
    render_bits(Defs, NewBin, Opts, Pos + Size);
render_bits([], Bin, _Opts, Pos) when bit_size(Bin) == Pos ->
    Bin.

parse_bits(Conv, Bin) -> parse_bits(Conv, Bin, #{}).

parse_bits([{Name, Size, Type}|Conv], Bin, Opts) ->
    <<Raw:Size/bitstring, Rest/bitstring>> = Bin,
    parse_bits(Conv, Rest, maps:put(Name, decode(Type, Raw), Opts));
parse_bits([{0, Size}|Conv], Bin, Opts) ->
    <<_:Size, Rest/bitstring>> = Bin,
    parse_bits(Conv, Rest, Opts);
parse_bits([], <<>>, Opts) ->
    Opts.

encode(_Name, unsigned_little, Size, Value) when is_integer(Value) ->
    <<Value:Size/unsigned-little>>;
encode(_Name, {unsigned_little, Min, Max}, Size, Value)
  when is_integer(Value) andalso Value >= Min andalso Value =< Max ->
    <<Value:Size/unsigned-little>>;
encode(_Name, signed_little, Size, Value) when is_integer(Value) ->
    <<Value:Size/signed-little>>;
encode(_Name, raw, Size, Value) when bit_size(Value) =< Size ->
    <<Value:Size/bitstring>>;
encode(_Name, raw, Size, Value) when is_integer(Value) ->
    <<Value:Size/unsigned-integer>>;
encode(Name, _Type, _Size, Value) ->
    throw({invalid_value, Name, Value}).

decode(raw, Raw) ->
    Raw;
decode(unsigned_little, Raw) ->
    Size = bit_size(Raw),
    <<Value:Size/unsigned-little>> = Raw,
    Value;
decode({unsigned_little, _Min, _Max}, Raw) ->
    Size = bit_size(Raw),
    <<Value:Size/unsigned-little>> = Raw,
    Value;
decode(signed_little, Raw) ->
    Size = bit_size(Raw),
    <<Value:Size/signed-little>> = Raw,
    Value;
decode(Mapping, Raw) when is_map(Mapping) ->
    Size = bit_size(Raw),
    <<Value:Size/unsigned-little>> = Raw,
    Values = mapz:inverse(Mapping),
    case maps:find(Value, Values) of
        {ok, Mapped} -> Mapped;
        error        -> Raw
    end.

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

registers() ->
    #{
        acc => registers(acc),
        mag => registers(mag),
        alt => registers(alt)
    }.

registers(acc) ->
    #{
        act_ths => {16#04, read_write, 1, [
            {sleep_on_inact_en, 1, #{
                gyroscope_power_down => 0,
                gyroscope_sleep => 1
            }},
            {act_ths, 7, unsigned_little}
        ]},
        act_dur => {16#05, read_write, 1, [{act_dur, 8, unsigned_little}]},
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
        int_gen_ths_x_xl => {16#07, read_write, 1, [
            {ths_xl_x, 8, unsigned_little}
        ]},
        int_gen_ths_y_xl => {16#08, read_write, 1, [
            {ths_xl_y, 8, unsigned_little}
        ]},
        int_gen_ths_z_xl => {16#09, read_write, 1, [
            {ths_xl_z, 8, unsigned_little}
        ]},
        int_gen_dur_xl => {16#0A, read_write, 1, [
            {wait_xl, 1, #{off => 0, on => 1}},
            {dur_xl,  7, unsigned_little}
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
        out_y_g => {16#1A, read, 2, fun convert_dps/3},
        out_z_g => {16#1C, read, 2, fun convert_dps/3},
        ctrl_reg4 => {16#1E, read_write, 1, [
            {0,        2},
            {zen_g,    1, #{disabled => 0, enabled => 1}},
            {yen_g,    1, #{disabled => 0, enabled => 1}},
            {xen_g,    1, #{disabled => 0, enabled => 1}},
            {0,        1},
            {lir_xl1,  1, #{false => 0, true => 1}},
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
        ctrl_reg6_xl => {16#20, read_write, 1, [ % FIXME: Verify default settings when booting the device!
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
        ctrl_reg8 => {16#22, read_write, 1, [
            {boot,       1, #{normal => 0, reboot_memory => 1}},
            {bdu,        1, #{continuous => 0, read => 1}},
            {h_lactive,  1, #{high => 0, low => 1}},
            {pp_od,      1, #{push_pull => 0, open_drain => 1}},
            {sim,        1, #{'4-wire' => 0, '3-wire' => 1}},
            {if_add_inc, 1, #{disabled => 0, enabled => 1}},
            {ble,        1, #{lsb => 0, msb => 1}},
            {sw_reset,   1, #{normal => 0, reset => 1}}
        ]},
        ctrl_reg9 => {16#23, read_write, 1, [
            {0,             1},
            {sleep_g,       1, #{disabled => 0, enabled => 1}},
            {0,             1},
            {fifo_temp_en,  1, #{disabled => 0, enabled => 1}},
            {drdy_mask_bit, 1, #{disabled => 0, enabled => 1}},
            {i2c_disable,   1, #{false => 0, true => 1}},
            {fifo_en,       1, #{disabled => 0, enabled => 1}},
            {stop_on_fth,   1, #{false => 0, true => 1}}
        ]},
        ctrl_reg10 => {16#24, read_write, 1, [
            {0,     5},
            {st_g,  1, #{disabled => 0, enable_csd => 1}},
            {0,     1},
            {st_xl, 1, #{disabled => 0, enabled => 1}}
        ]},
        int_gen_src_xl => {16#26, read, 1, [
            {0, 1},
            {ia_xl, 1, #{false => 0, true => 1}},
            {zh_xl, 1, #{false => 0, true => 1}},
            {zl_xl, 1, #{false => 0, true => 1}},
            {yh_xl, 1, #{false => 0, true => 1}},
            {yl_xl, 1, #{false => 0, true => 1}},
            {xh_xl, 1, #{false => 0, true => 1}},
            {xl_xl, 1, #{false => 0, true => 1}}
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
        out_z_h_xl => {16#2D, read, 1, raw},
        fifo_ctrl => {16#2E, read_write, 1, [
            {fmode, 3, raw},
            {fth,   5, unsigned_little}
        ]},
        fifo_src => {16#2F, read, 1, [
            {fth,  1, #{false => 0, true => 1}},
            {ovrn, 1, #{false => 0, true => 1}},
            {fss,  6, unsigned_little}
        ]},
        int_gen_cfg_g => {16#30, read_write, 1, [
            {aoi_g,  1, #{'or' => 0, 'and' => 1}},
            {lir_g,  1, #{false => 0, true => 1}},
            {zhie_g, 1, #{disabled => 0, enabled => 1}},
            {zlie_g, 1, #{disabled => 0, enabled => 1}},
            {yhie_g, 1, #{disabled => 0, enabled => 1}},
            {ylie_g, 1, #{disabled => 0, enabled => 1}},
            {xhie_g, 1, #{disabled => 0, enabled => 1}},
            {xlie_g, 1, #{disabled => 0, enabled => 1}}
        ]},
        int_gen_ths_x_g => {16#31, read_write, 2, [
            {dcrm_g,  1, #{reset => 0, decrement => 1}},
            {ths_g_x, 15, signed_little}
        ]},
        int_gen_ths_xh_g => {16#31, read_write, 1, raw},
        int_gen_ths_xl_g => {16#32, read_write, 1, raw},
        int_gen_ths_y_g => {16#33, read_write, 2, [
            {0,       1},
            {ths_g_y, 15, signed_little}
        ]},
        int_gen_ths_yh_g => {16#33, read_write, 1, raw},
        int_gen_ths_yl_g => {16#34, read_write, 1, raw},
        int_gen_ths_z_g => {16#35, read_write, 2, [
            {0,       1},
            {ths_g_z, 15, signed_little}
        ]},
        int_gen_ths_zh_g => {16#35, read_write, 1, raw},
        int_gen_ths_zl_g => {16#36, read_write, 1, raw},
        int_gen_dur_g => {16#37, read_write, 1, [
            {wait_g, 1, #{disabled => 0, enabled => 1}},
            {dur_g,  7, unsigned_little}
        ]}
    };
registers(mag) ->
    #{
        offset_x_reg_m =>  {16#05, read_write, 2, [
            {ofxm, 16, signed_little}
        ]},
        offset_x_reg_l_m => {16#05, read_write, 1, raw},
        offset_x_reg_h_m => {16#06, read_write, 1, raw},
        offset_y_reg_m => {16#07, read_write, 2, [
            {ofym, 16, signed_little}
        ]},
        offset_y_reg_l_m => {16#07, read_write, 1, raw},
        offset_y_reg_h_m => {16#08, read_write, 1, raw},
        offset_z_reg_m => {16#09, read_write, 2, [
            {ofzm, 16, signed_little}
        ]},
        offset_z_reg_l_m => {16#09, read_write, 1, raw},
        offset_z_reg_h_m => {16#0A, read_write, 1, raw},
        who_am_i_m => {16#0F, read, 1, raw},
        ctrl_reg1_m => {16#20, read_write, 1, [
            {temp_comp, 1, #{disabled => 0, enabled => 1}},
            {om,        2, #{
                low        => 2#00,
                medium     => 2#01,
                high       => 2#10,
                ultra_high => 2#11
            }},
            {do,        3, #{
                {hz, 0.625} => 2#000,
                {hz, 1.25}  => 2#001,
                {hz, 2.5}   => 2#010,
                {hz, 5}     => 2#011,
                {hz, 10}    => 2#100,
                {hz, 20}    => 2#101,
                {hz, 40}    => 2#110,
                {hz, 80}    => 2#111
            }},
            {fast_odr,  1, #{disabled => 0, enabled => 1}},
            {st,        1, #{disabled => 0, enabled => 1}}
        ]},
        ctrl_reg2_m => {16#21, read_write, 1, [
            {0,        1},
            {fs,       2, #{
                {gauss, 4}  => 2#00,
                {gauss, 8}  => 2#01,
                {gauss, 12} => 2#10,
                {gauss, 16} => 2#11
            }},
            {0,        1},
            {reboot,   1, #{normal => 0, reboot_memory => 1}},
            {soft_rst, 1, #{default => 0, reset => 1}},
            {0,        2}
        ]},
        ctrl_reg3_m => {16#22, read_write, 1, [
            {i2c_disable, 1, #{false => 0, true => 1}},
            {0,           1},
            {lp,          1, #{false => 0, true => 1}},
            {0,           2},
            {sim,         1, #{write_only => 0, read_write => 1}},
            {md,          2, #{
                continuous_conversion => 2#00,
                single_conversion    => 2#01,
                power_down           => 2#11
            }}
        ]},
        ctrl_reg4_m => {16#23, read_write, 1, [
            {0,   4},
            {omz, 2, #{
                low        => 2#00,
                medium     => 2#01,
                high       => 2#10,
                ultra_high => 2#11
            }},
            {ble, 1, #{lsb => 0, msb => 1}},
            {0,   1}
        ]},
        ctrl_reg5_m => {16#24, read_write, 1, [
            {fast_read, 1, #{disabled => 0, enabled => 0}},
            {bdu,       1, #{continious => 0, read => 1}},
            {0,         6}
        ]},
        status_reg_m => {16#27, read, 1, [
            {zyxor, 1, #{false => 0, true => 1}},
            {zor,   1, #{false => 0, true => 1}},
            {yor,   1, #{false => 0, true => 1}},
            {'xor', 1, #{false => 0, true => 1}},
            {zyxda, 1, #{false => 0, true => 1}},
            {zda,   1, #{false => 0, true => 1}},
            {yda,   1, #{false => 0, true => 1}},
            {xda,   1, #{false => 0, true => 1}}
        ]},
        out_x_m => {16#28, read, 2, fun convert_gauss/3},
        out_x_l_m => {16#28, read, 1, raw},
        out_x_h_m => {16#29, read, 1, raw},
        out_y_m => {16#2A, read, 2, fun convert_gauss/3},
        out_y_l_m => {16#2A, read, 1, raw},
        out_y_h_m => {16#2B, read, 1, raw},
        out_z_m => {16#2C, read, 2, fun convert_gauss/3},
        out_z_l_m => {16#2C, read, 1, raw},
        out_z_h_m => {16#2D, read, 1, raw},
        int_cfg_m => {16#30, read_write, 1, [
            {xien, 1, #{disabled => 0, enabled => 1}},
            {yien, 1, #{disabled => 0, enabled => 1}},
            {zien, 1, #{disabled => 0, enabled => 1}},
            {0,    2},
            {iea,  1, #{low => 0, high => 1}},
            {iel,  1, #{false => 0, true => 1}},
            {ien,  1, #{disabled => 0, enabled => 1}}
        ]},
        int_src_m => {16#31, read, 1, [
            {pth_x, 1, #{false => 0, true => 1}},
            {pth_y, 1, #{false => 0, true => 1}},
            {pth_z, 1, #{false => 0, true => 1}},
            {nth_x, 1, #{false => 0, true => 1}},
            {nth_y, 1, #{false => 0, true => 1}},
            {nth_z, 1, #{false => 0, true => 1}},
            {mroi,  1, #{false => 0, true => 1}},
            {int,   1, #{false => 0, true => 1}}
        ]},
        int_ths_m => {16#32, read_write, 2, [
            {ths, 16, {unsigned_little, 0, 32767}}
        ]},
        int_ths_l_m => {16#32, read_write, 1, raw},
        int_ths_h_m => {16#33, read_write, 1, raw}
    };
registers(alt) ->
    #{
        ref_p => {16#08, read_write, 3, raw},
        ref_p_xl => {16#08, read_write, 1, raw},
        ref_p_l => {16#09, read_write, 1, raw},
        ref_p_h => {16#0A, read_write, 1, raw},
        who_am_i => {16#0F, read, 1, raw},
        res_conf => {16#10, read_write, 1, [
            {0, 4},
            {avgt, 2, #{
                {int_avg, 8}  => 2#00,
                {int_avg, 16} => 2#01,
                {int_avg, 32} => 2#10,
                {int_avg, 64} => 2#11
            }},
            {avgp, 2, #{
                {int_avg, 8}   => 2#00,
                {int_avg, 32}  => 2#01,
                {int_avg, 128} => 2#10,
                {int_avg, 512} => 2#11
            }}
        ]},
        ctrl_reg1 => {16#20, read_write, 1, [
            {pd,       1, #{power_down => 0, active => 1}},
            {odr,      3, #{
                one_shot   => 2#000,
                {hz, 1}    => 2#001,
                {hz, 7}    => 2#010,
                {hz, 12.5} => 2#011,
                {hz, 25}   => 2#100
            }},
            {diff_en,  1, #{disabled => 0, enabled => 1}},
            {bdu,      1, #{continuous => 0, read => 1}},
            {reset_az, 1, #{normal => 0, reset => 1}},
            {sim,      1, #{'4-wire' => 0, '3-wire' => 1}}
        ]},
        ctrl_reg2 => {16#21, read_write, 1, [
            {boot,          1, #{normal => 0, reboot_memory => 1}},
            {fifo_en,       1, #{disabled => 0, enabled => 1}},
            {stop_on_fth,   1, #{false => 0, true => 1}},
            {fifo_mean_dec, 1, #{disabled => 0, enabled => 1}},
            {i2c_dis,       1, #{false => 0, true => 1}},
            {swreset,       1, #{normal => 0, reset => 1}},
            {autozero,      1, #{disabled => 0, enabled => 1}},
            {one_shot,      1, #{idle => 0, trigger => 1}}
        ]},
        ctrl_reg3 => {16#22, read_write, 1, [
            {int_h_l,  1, #{high => 0, low => 1}},
            {pp_od,    1, #{push_pull => 0, open_drain => 1}},
            {0,        4},
            {int_s,    2, #{
                data_signal => 2#00,
                p_high      => 2#01,
                p_low       => 2#10,
                p_or        => 2#11
            }}
        ]},
        ctrl_reg4 => {16#23, read_write, 1, [
            {0,       4},
            {f_empty, 1, #{disabled => 0, enabled => 1}},
            {f_fth,   1, #{disabled => 0, enabled => 1}},
            {f_ovr,   1, #{disabled => 0, enabled => 1}},
            {drdy,    1, #{disabled => 0, enabled => 1}}
        ]},
        interrupt_cfg => {16#24, read_write, 1, [
            {0,    5},
            {lir,  1, #{false => 0, true => 1}},
            {pl_e, 1, #{disabled => 0, enabled => 1}},
            {ph_e, 1, #{disabled => 0, enabled => 1}}
        ]},
        int_source => {16#25, read, 1, [
            {0,  5},
            {ia, 1, #{false => 0, true => 1}},
            {pl, 1, #{false => 0, true => 1}},
            {ph, 1, #{false => 0, true => 1}}
        ]},
        status_reg => {16#27, read, 1, [
            {0,    2},
            {p_or, 1, #{false => 0, true => 1}},
            {t_or, 1, #{false => 0, true => 1}},
            {0,    2},
            {p_da, 1, #{false => 0, true => 1}},
            {t_da, 1, #{false => 0, true => 1}}
        ]},
        press_out => {16#28, read, 3, fun convert_pressure/3},
        press_out_xl => {16#28, read, 1, raw},
        press_out_l => {16#29, read, 1, raw},
        press_out_h => {16#2A, read, 1, raw},
        temp_out => {16#2B, read, 2, fun convert_alt_temp/3},
        temp_out_l => {16#2B, read, 1, raw},
        temp_out_h => {16#2C, read, 1, raw},
        fifo_ctrl => {16#2E, read_write, 1, [
            {f_mode, 3, #{
                bypass           => 2#000,
                fifo             => 2#001,
                stream           => 2#010,
                stream_to_fifo   => 2#011,
                bypass_to_stream => 2#100,
                not_available    => 2#101,
                fifo_mean        => 2#110,
                bypass_to_fifo   => 2#111
            }},
            {wtm_point, 5, #{
                {sample, 2}  => 2#00001,
                {sample, 4}  => 2#00011,
                {sample, 8}  => 2#00111,
                {sample, 16} => 2#01111,
                {sample, 32} => 2#11111
            }}
        ]},
        fifo_status => {16#2F, read, 1, [
            {fth_fifo,   1, #{false => 0, true => 1}},
            {ovr,        1, #{false => 0, true => 1}},
            {empty_fifo, 1, #{false => 0, true => 1}},
            {fss,        5, unsigned_little}
        ]},
        ths_p => {16#30, read_write, 1, unsigned_little},
        ths_p_l => {16#30, read_write, 1, raw},
        ths_p_h => {16#31, read_write, 1, raw},
        rpds => {16#39, read_write, 1, signed_little},
        rpds_l => {16#39, read_write, 1, raw},
        rpds_h => {16#3A, read_write, 1, raw}
    }.

convert_g(Raw, Context, Opts) ->
    Value = decode(signed_little, Raw),
    Scale = case maps:get(xl_unit, Opts, g) of
        g     -> 0.001;
        mg    -> 1.0;
        Other -> throw({unknown_option, #{xl_unit => Other}})
    end,
    {FS, NewContext} = setting(ctrl_reg6_xl, fs_xl, Context),
    Result = case FS of
        {g, 2}  -> Value * 0.061 * Scale;
        {g, 8}  -> Value * 0.244 * Scale;
        {g, 4}  -> Value * 0.122 * Scale;
        {g, 16} -> Value * 0.732 * Scale;
        _       -> Raw
    end,
    {Result, NewContext}.

convert_temp(Raw, Context, _Opts) ->
    {decode(signed_little, Raw) / 16 + 25, Context}.

convert_dps(Raw, Context, Opts) ->
    Value = decode(signed_little, Raw),
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

convert_gauss(Raw, Context, Opts) ->
    Value = decode(unsigned_little, Raw),
    Scale = case maps:get(mag_unit, Opts, gauss) of
        gauss  -> 0.001;
        mgauss -> 1.0;
        Other  -> throw({unknown_option, #{mag_unit => Other}})
    end,
    {MagSensitivity, NewContext} = setting(ctrl_reg2_m, fs, Context),
    Result = case MagSensitivity of
        {gauss, 4}  -> Value * 0.14 * Scale;
        {gauss, 8}  -> Value * 0.29 * Scale;
        {gauss, 12} -> Value * 0.43 * Scale;
        {gauss, 16} -> Value * 0.58 * Scale
    end,
    {Result, NewContext}.

convert_pressure(Raw, Context, _Opts) ->
    {decode(signed_little, Raw) / 4096, Context}.

convert_alt_temp(Raw, Context, _Opts) ->
    {42.5 + decode(signed_little, Raw) / 480, Context}.

debug_read(Comp, Reg, Value) ->
    io:format("[PmodNAV][~p] read  16#~2.16.0B --> ~s~n",
        [Comp, Reg, debug_bitstring(Value)]
    ).

debug_write(Comp, Reg, Value) ->
    io:format("[PmodNAV][~p] write 16#~2.16.0B <-- ~s~n",
        [Comp, Reg, debug_bitstring(Value)]
    ).

debug_bitstring(Bitstring) ->
    lists:flatten([io_lib:format("2#~8.2.0B ", [X]) || <<X>> <= Bitstring]).
