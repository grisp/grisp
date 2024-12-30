-module(grisp_pwm).
-export([setup/4]).
-export([default_pwm_config/0]).

-compile(export_all).

-record(pwm_config, {
    sample_repeat :: sample_repeat(),
    prescale :: prescale(),
    clock :: clock(),
    output_config :: output_config(),
    swap_half_word :: swap_half_word(),
    swap_sample :: swap_sample(),
    run_if_debug :: run_if_debug(),
    run_if_wait :: run_if_wait(),
    run_if_doze :: run_if_doze(),
    run_if_stop :: run_if_stop(),
    flag_empty_water_mark :: flag_empty_water_mark()
}).

-record(pwm_interrupt_config, {
    compare_interrupt :: compare_interrupt(),
    rollover_interrupt :: rollover_interrupt(),
    fifo_empty_interrupt :: fifo_empty_interrupt()
}).

-record(status, {
    fifo_available :: fifo_available(),
    fifo_empty :: fifo_empty(),
    roll_over_event :: roll_over_event(),
    compare_event :: compare_event(),
    fifo_write_error :: fifo_write_error()
}).


-type pwm_id() :: 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8.

-type pwm_activation() :: true | false.

-type sample_repeat() :: 1 | 2 | 3 | 4.
-type prescale() :: 1..4096.
-type clock() :: off | ipg_clk | ipg_clk_highfreq | ipg_clk_32k.
-type output_config() :: disconnected | set_at_rollover | set_at_comparison.
-type swap_half_word() :: true | false.
-type swap_sample() :: true | false.
-type run_if_debug() :: true | false.
-type run_if_wait() :: true | false.
-type run_if_doze() :: true | false.
-type run_if_stop() :: true | false.
-type flag_empty_water_mark() :: 1 | 2 | 3 | 4.
-type pwm_config() :: #pwm_config{}.

-type compare_interrupt() :: true | false.
-type rollover_interrupt() :: true | false.
-type fifo_empty_interrupt() :: true | false.
-type pwm_interrupt_config() :: #pwm_interrupt_config{}.

-type sample() :: <<_:16>>.

-type fifo_available() :: 0 | 1 | 2 | 3 | 4.
-type fifo_empty() :: true | false.
-type roll_over_event() :: true | false.
-type compare_event() :: true | false.
-type fifo_write_error() :: true | false.
-type status() :: #status{}.

-type period() :: 0..65535.

% the names correspond to the
% i.MX 6UltraLite Applications
% Processor Reference Manual
-define(ADDRESSES, #{
        "PWM1_PWMCR"  => 16#208_0000,
        "PWM1_PWMSR"  => 16#208_0004,
        "PWM1_PWMIR"  => 16#208_0008,
        "PWM1_PWMSAR" => 16#208_000C,
        "PWM1_PWMPR"  => 16#208_0010,
        "PWM1_PWMCNR" => 16#208_0014,

        "PWM2_PWMCR"  => 16#208_4000,
        "PWM2_PWMSR"  => 16#208_4004,
        "PWM2_PWMIR"  => 16#208_4008,
        "PWM2_PWMSAR" => 16#208_400C,
        "PWM2_PWMPR"  => 16#208_4010,
        "PWM2_PWMCNR" => 16#208_4014,

        "PWM3_PWMCR"  => 16#208_8000,
        "PWM3_PWMSR"  => 16#208_8004,
        "PWM3_PWMIR"  => 16#208_8008,
        "PWM3_PWMSAR" => 16#208_800C,
        "PWM3_PWMPR"  => 16#208_8010,
        "PWM3_PWMCNR" => 16#208_8014,

        "PWM4_PWMCR"  => 16#208_C000,
        "PWM4_PWMSR"  => 16#208_C004,
        "PWM4_PWMIR"  => 16#208_C008,
        "PWM4_PWMSAR" => 16#208_C00C,
        "PWM4_PWMPR"  => 16#208_C010,
        "PWM4_PWMCNR" => 16#208_C014,

        "PWM5_PWMCR"  => 16#20F_0000,
        "PWM5_PWMSR"  => 16#20F_0004,
        "PWM5_PWMIR"  => 16#20F_0008,
        "PWM5_PWMSAR" => 16#20F_000C,
        "PWM5_PWMPR"  => 16#20F_0010,
        "PWM5_PWMCNR" => 16#20F_0014,

        "PWM6_PWMCR"  => 16#20F_4000,
        "PWM6_PWMSR"  => 16#20F_4004,
        "PWM6_PWMIR"  => 16#20F_4008,
        "PWM6_PWMSAR" => 16#20F_400C,
        "PWM6_PWMPR"  => 16#20F_4010,
        "PWM6_PWMCNR" => 16#20F_4014,

        "PWM7_PWMCR"  => 16#20F_8000,
        "PWM7_PWMSR"  => 16#20F_8004,
        "PWM7_PWMIR"  => 16#20F_8008,
        "PWM7_PWMSAR" => 16#20F_800C,
        "PWM7_PWMPR"  => 16#20F_8010,
        "PWM7_PWMCNR" => 16#20F_8014,

        "PWM8_PWMCR"  => 16#20F_C000,
        "PWM8_PWMSR"  => 16#20F_C004,
        "PWM8_PWMIR"  => 16#20F_C008,
        "PWM8_PWMSAR" => 16#20F_C00C,
        "PWM8_PWMPR"  => 16#20F_C010,
        "PWM8_PWMCNR" => 16#20F_C014
}).

-define(PIMUXING, #{
        gpio1_8  => #{pwm_id => 3, register => 16#20E_0120, value => <<1:32>>},
        gpio1_4  => #{pwm_id => 3, register => 16#20E_006C, value => <<1:32>>},
        gpio1_2  => #{pwm_id => 4, register => 16#20E_0070, value => <<1:32>>},
        gpio_2_6 => #{pmw_id => 8, register => 16#20E_01E0, value => <<6:32>>},
        spi2_7   => #{pmw_id => 7, register => 16#20E_01DC, value => <<6:32>>},
        uart_8   => #{pmw_id => 1, register => 16#20E_0118, value => <<1:32>>},
        uart_9   => #{pmw_id => 2, register => 16#20E_011C, value => <<1:32>>},
        jtag_4   => #{pmw_id => 7, register => 16#20E_0054, value => <<4:32>>},
        jtag_8   => #{pmw_id => 6, register => 16#20E_0050, value => <<4:32>>}
}).

-spec default_pwm_config() -> pwm_config().
default_pwm_config() ->
    #pwm_config{
        sample_repeat = 1,
        prescale = 1,
        clock = ipg_clk_32k,
        output_config = set_at_rollover,
        swap_half_word = false,
        swap_sample = false,
        run_if_debug = true,
        run_if_wait = false,
        run_if_doze = false,
        run_if_stop = false,
        flag_empty_water_mark = 2
      }.

-spec default_interrupt_config() -> pwm_interrupt_config().
default_interrupt_config() ->
    #pwm_interrupt_config{
       compare_interrupt = false,
       rollover_interrupt = false,
       fifo_empty_interrupt = false
    }.

-spec setup(pwm_id()) -> status().
setup(PWMId) ->
    setup(PWMId, grisp_pwm:default_pwm_config(), 256, <<128:16>>).

-spec setup(pwm_id(), pwm_config(), period(), sample()) -> status().
setup(PWMId, Config = #pwm_config{}, Period, Sample) when is_number(PWMId), is_number(Period), is_binary(Sample) ->
    % make sure PMW is disabled
    set_activation(PWMId, false),
    % configure PWM Control Register
    configure(PWMId, Config),
    % configure PWM Interrupt Register
    configure_interrupts(PWMId, default_interrupt_config()),
    % write to PWM Sample Register
    set_sample(PWMId, Sample),
    set_sample(PWMId, Sample),
    set_sample(PWMId, Sample),
    % get PWM status
    Status = status(PWMId),
    % check FIFO Write Error status bit
    false = Status#status.fifo_write_error,
    % check Compare status bit
    false = Status#status.compare_event,
    % check Roll-over status bit
    false = Status#status.roll_over_event,
    % write PWM Period Register
    set_pwm_period(PWMId, Period),
    % enable PWM
    set_activation(PWMId, true),
    status(PWMId).

-spec set_pwm_period(pwm_id(), period()) -> ok.
set_pwm_period(PWMId, Period) when is_integer(PWMId), is_integer(Period) ->
    set_register(address(PWMId, "PWMPR"), <<0:16, <<Period:16/big>>/binary>>).


-spec status(pwm_id()) -> status().
status(PWMId) ->
    <<_:25, FWE:1, CMP:1, ROV:1, FE:1, FIFOAV1:1, FIFOAV2:1, FIFOAV3:1>> = get_register(address(PWMId, "PWMSR")),
    FIFOAV =
    case {FIFOAV1, FIFOAV2, FIFOAV3} of
        {0, 0, 0} -> 0;
        {0, 0, 1} -> 1;
        {0, 1, 0} -> 2;
        {0, 1, 1} -> 3;
        {1, 0, 0} -> 4
    end,
    #status{
        fifo_available = FIFOAV,
        fifo_empty = (FE == 1),
        roll_over_event = (ROV == 1),
        compare_event = (CMP == 1),
        fifo_write_error = (FWE == 1)
     }.

-spec set_sample(pwm_id(), sample()) -> ok.
set_sample(PWMId, Sample) when is_integer(PWMId), is_binary(Sample) ->
    set_register(address(PWMId, "PWMSAR"), <<0:16, Sample/binary>>).

-spec configure_interrupts(pwm_id(), pwm_interrupt_config()) -> pwm_interrupt_config().
configure_interrupts(PWMId, Interrupts = #pwm_interrupt_config{}) when is_integer(PWMId) ->
    CompareInterrupt =
    case Interrupts#pwm_interrupt_config.compare_interrupt of
        false -> <<0:1>>;
        true  -> <<1:1>>
    end,
    RolloverInterrupt =
    case Interrupts#pwm_interrupt_config.rollover_interrupt of
        false -> <<0:1>>;
        true  -> <<1:1>>
    end,
    FifoEmptyInterrupt =
    case Interrupts#pwm_interrupt_config.fifo_empty_interrupt of
        false -> <<0:1>>;
        true  -> <<1:1>>
    end,
    Data = <<
        <<0:29>>/bitstring,
        CompareInterrupt/bitstring,
        RolloverInterrupt/bitstring,
        FifoEmptyInterrupt/bitstring
    >>,
    set_register(address(PWMId, "PWMIR"), Data).


-spec configure( pwm_id(), pwm_config()) -> ok.
configure(PWMId, Config = #pwm_config{}) when is_integer(PWMId) ->
    FlagEmptyWatermark =
    case Config#pwm_config.flag_empty_water_mark of
        1 -> <<0:1, 0:1>>;
        2 -> <<0:1, 1:1>>;
        3 -> <<1:1, 0:1>>;
        4 -> <<1:1, 1:1>>
    end,
    RunIfStop =
    case Config#pwm_config.run_if_stop of
        false -> <<0:1>>;
        true  -> <<1:1>>
    end,
    RunIfDoze =
    case Config#pwm_config.run_if_doze of
        false -> <<0:1>>;
        true  -> <<1:1>>
    end,
    RunIfWait =
    case Config#pwm_config.run_if_wait of
        false -> <<0:1>>;
        true  -> <<1:1>>
    end,
    RunIfDebug =
    case Config#pwm_config.run_if_debug of
        false -> <<0:1>>;
        true  -> <<1:1>>
    end,
    SwapSample =
    case Config#pwm_config.swap_sample of
        false -> <<0:1>>;
        true  -> <<1:1>>
    end,
    SwapHalfWord =
    case Config#pwm_config.swap_half_word of
        false -> <<0:1>>;
        true  -> <<1:1>>
    end,
    OutputConfig =
    case Config#pwm_config.output_config of
        set_at_rollover ->   <<0:1, 0:1>>;
        set_at_comparison -> <<0:1, 1:1>>;
        disconnected ->      <<0:1, 0:0>>
    end,
    Clock =
    case Config#pwm_config.clock of
        off              -> <<0:1, 0:1>>;
        ipg_clk          -> <<0:1, 1:1>>;
        ipg_clk_highfreq -> <<1:1, 0:1>>;
        ipg_clk_32k      -> <<1:1, 1:1>>
    end,
    Prescale = <<(Config#pwm_config.prescale - 1):12/unsigned-integer>>,
    SoftwareReset = <<0:1>>,
    SampleRepeat =
    case Config#pwm_config.sample_repeat of
        1 -> <<0:1, 0:1>>;
        2 -> <<0:1, 1:1>>;
        3 -> <<1:1, 0:1>>;
        4 -> <<1:1, 1:1>>
    end,
    Enable = <<0:1>>,
    Data = <<
        <<0:4>>/bitstring,
        FlagEmptyWatermark/bitstring,
        RunIfStop/bitstring,
        RunIfDoze/bitstring,
        RunIfWait/bitstring,
        RunIfDebug/bitstring,
        SwapSample/bitstring,
        SwapHalfWord/bitstring,
        OutputConfig/bitstring,
        Clock/bitstring,
        Prescale/bitstring,
        SoftwareReset/bitstring,
        SampleRepeat/bitstring,
        Enable/bitstring
    >>,
    set_register(address(PWMId, "PWMCR"), Data),
    ok.

-spec reset(pwm_id()) -> ok.
reset(PWMId) when is_integer(PWMId)->
    Address = address(PWMId, "PWMCR"),
    <<Pre:28, _Reset:1, Post:3>> = get_register(Address),
    Data = <<Pre:28, 1:1, Post:3>>,
    set_register(Address, Data).


-spec set_activation(pwm_id(), pwm_activation()) -> ok.
set_activation(PWMId, Active) when is_number(PWMId), is_atom(Active) ->
    ActiveBit =
    case Active of
        true -> <<1:1>>;
        false -> <<0:1>>
    end,
    Address = address(PWMId, "PWMCR"),
    <<Rest:31, _:1>> = get_register(Address),
    RegisterWithActivation = <<<<Rest:31>>/bitstring, ActiveBit/bitstring>>,
    set_register(Address, RegisterWithActivation),
    ok.

address(PWMId, Key) when is_number(PWMId), is_list(Key) ->
    maps:get(("PWM" ++ integer_to_list(PWMId) ++ "_" ++ Key), ?ADDRESSES).

set_register(Address, <<Value:32/big>>) when is_number(Address) ->
    grisp_gpio:set_register32(Address, Value).

get_register(Address) when is_number(Address) ->
    Value = grisp_gpio:get_register32(Address),
    <<Value:32/big>>.
