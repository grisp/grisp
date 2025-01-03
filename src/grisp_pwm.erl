% @doc GriSP Pulse Width Modulation (PWM) API.
%
% Pulse Width Modulation (PWM) is used to generate a rectangular wave with a varying duty cycle
% to control the average power or amplitude delivered.
% The ARM Cortex-A7 has eight PWM units that can be multiplexed to drive a few different pins.
%
% === Typical Use ===
% ```
% 1> grisp_pwm:start_driver().
% {device,pwm,grisp_pwm,<0.353.0>,
%         #Ref<0.838610995.3080454145.146844>}
% 2> grisp_pwm:open(gpio1_8, default, 0.75).
% ok
% '''
%
% This creates a rectangular wave on pin `gpio1_8' with a 155.5μs cycle time (6.43MHz) and a duty cycle of 75%
% (see <a href="#figure_1">Figure 1</a>).
%
% You can change the duty cycle by setting a new sample:
% ```
% 3> grisp_pwm:set_sample(gpio1_8, 0.5).
% ok
% '''
% If you want to stop using PWM on this pin you can call:
% ```
%  4> grisp_pwm:close(gpio1_8).
%  ok
% '''
%
% <figure id="figure_1">
%   <img src="images/pmw_example.png" width="1000px" alt=""/>
%   <figcaption><em>Figure 1. Oscilloscope trace with a 0.75 % duty cycle and the default configuration.</em></figcaption>
% </figure>
%
% === Ramp Up Example ===
% You can ramp up the duty cycle from 0% to 100% in one second like this:
% ```
% 1> RampSample = fun(X) -> grisp_pwm:set_sample(gpio1_8, (X/100)), timer:sleep(10) end.
% #Fun<erl_eval.42.39164016>
% 2> [RampSample(X) || X <- lists:seq(1, 100)].
% [ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,
% ok,ok,ok,ok,ok,ok,ok,ok,ok,ok|...]
% '''
% === Sinusoidal Example ===
% You can create a sine wave like this:
% ```
% 1> SinSample = fun(X) -> V = math:sin(math:pi()/2*(X/10))/2+0.5, grisp_pwm:set_sample(gpio1_8, V), timer:sleep(10) end.
% #Fun<erl_eval.42.39164016>
% 2> [SinSample(X) || X <- lists:seq(1, 1000)].
% [ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,
% ok,ok,ok,ok,ok,ok,ok,ok,ok,ok|...]
% '''
% === Pin Mappings ===
%
% <table border="1" cellpadding="8">
%   <caption>GRiSP 2 Pin Mappings</caption>
%   <tr>
%     <th rowspan="2">ID</th>
%     <th colspan="4">Mapping</th>
%   </tr>
%   <tr>
%     <th>Slot</th>
%     <th>Type</th>
%     <th>#</th>
%     <th>Schematic</th>
%   </tr>
%   <tr><td>`gpio1_2'</td>    <td>GPIO1</td>    <td>PMOD 1A</td>  <td>2</td> <td>X1404.2</td></tr>
%   <tr><td>`gpio1_4'</td>    <td>GPIO1</td>    <td>PMOD 1A</td>  <td>4</td> <td>X1404.4</td></tr>
%   <tr><td>`gpio1_8'</td>    <td>GPIO1</td>    <td>PMOD 1A</td>  <td>8</td> <td>X1404.8</td></tr>
%   <tr><td>`gpio_2_6'</td>   <td>GPIO_2/4</td> <td>Generic</td>  <td></td>  <td>X1301.6</td></tr>
%   <tr><td>`spi2_7'</td>     <td>SPI2</td>     <td>SPI</td>      <td></td>  <td>X1402.7</td></tr>
%   <tr><td>`uart_8'</td>     <td>UART</td>     <td>UART</td>     <td></td>  <td>X1405.8</td></tr>
%   <tr><td>`uart_9'</td>     <td>UART</td>     <td>UART</td>     <td></td>  <td>X1405.9</td></tr>
%   <tr><td>`jtag_4'</td>     <td>JTAG</td>     <td>JTAG</td>     <td></td>  <td>X1503.4</td></tr>
%   <tr><td>`jtag_8'</td>     <td>JTAG</td>     <td>JTAG</td>     <td></td>  <td>X1503.8</td></tr>
% </table><br/>
%
%
% === Other drivers ===
% This driver might use pins that are also used by other drivers (GPIO, SPI, UART). When opening a pin the PWM driver
% takes control and configures the multixplexing so the pin is wired to the corresponding PWM unit.
% When closing the pin the driver will restore the pevious multiplexing setting, handing back control.
%
% Example: `grisp_gpio' is used to set a pin to high, then `grisp_pwm:open/3' is used to drive the pin.
% After `grisp_pwm:close/1' is called, the pin is set to high and `grisp_gpio' is again in control.

-module(grisp_pwm).
-behaviour(gen_server).
-include("grisp_nif.hrl").

% driver
-export([start_driver/0]).

% API
-export([
    start_link/0,
    start_link/2,
    open/3,
    close/1,
    set_sample/2
]).

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% Callbacks
-export([on_load/0]).
-on_load(on_load/0).

% export for testability
-export([get_register/1, set_register/2, setup/3]).

-record(pwm_config, {
    sample_repeat :: sample_repeat(),
    prescale :: prescale(),
    clock :: clock(),
    period :: period(),
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

-record(pin_state, {
    pin :: pin(),
    pwm_id :: pwm_id(),
    mux_register :: number(),
    previous_mux_value :: <<_:32>>,
    config :: #pwm_config{}
}).

-record(state, {
    pin_states :: #{atom() => #pin_state{}}
}).

-type pin() :: gpio1_2 | gpio1_4 | gpio1_8 | gpio_2_6 | spi2_7 | uart_8 | uart_9 | jtag_4 | jtag_8.

-type pwm_id() :: 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8.

-type config() :: default | {prescale(), period()} | {clock(), prescale(), period()}.

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

-type sample() :: <<_:16>> | float().

-type fifo_available() :: 0 | 1 | 2 | 3 | 4.
-type fifo_empty() :: true | false.
-type roll_over_event() :: true | false.
-type compare_event() :: true | false.
-type fifo_write_error() :: true | false.
-type status() :: #status{}.

-type period() :: <<_:16>>.

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

-define(PINMUXING, #{
        gpio1_2  => #{pwm_id => 4, register => 16#20E_0070, value => <<1:32>>},
        gpio1_4  => #{pwm_id => 3, register => 16#20E_006C, value => <<1:32>>},
        gpio1_8  => #{pwm_id => 3, register => 16#20E_0120, value => <<1:32>>},
        gpio_2_6 => #{pmw_id => 8, register => 16#20E_01E0, value => <<6:32>>},
        spi2_7   => #{pmw_id => 7, register => 16#20E_01DC, value => <<6:32>>},
        uart_8   => #{pmw_id => 1, register => 16#20E_0118, value => <<1:32>>},
        uart_9   => #{pmw_id => 2, register => 16#20E_011C, value => <<1:32>>},
        jtag_4   => #{pmw_id => 7, register => 16#20E_0054, value => <<4:32>>},
        jtag_8   => #{pmw_id => 6, register => 16#20E_0050, value => <<4:32>>}
}).

%% API
% @doc Starts the driver and registers a PWM device.
start_driver() ->
    grisp:add_device(pwm, ?MODULE, #{}).

%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
% interface for grisp_devices
start_link(pwm, #{}) ->
    start_link().

% @doc Opens a pin and sets a configuration and a sample.
%
% === Default Config Example ===
% The default configuratin uses the `ipg_clk' clock, a prescale of 10 and a period of `<<1024:16>>'.
% This results in a 155.5μs cycle time (6.43MHz) and 10bit resolution.
% ```
% 1> grisp_pwm:open(gpio1_8, default, 0.75).
% ok
% '''
% === Prescale and Period Config Example ===
% Here we set the prescaler to 1 (10 times higher frequency than the default) and the period to `<<512:16>>' (further doubling the frequency and halfing the resolution).
% The result is a 7,775μs cycle time (128.6MHz) and 9bit resolution.
% ```
% 1> grisp_pwm:open(gpio1_8, {1, <<512:16>>}, 0.5).
% ok
% '''
% === Clock, Prescale and Period Config Example ===
% This sets a low frequency clock and results in a 500μs cycle time (2Hz) and 4bit resolution.
% ```
% 1> grisp_pwm:open(gpio1_8, {ipg_clk_32k, 1, <<16:16>>}, 0.5).
% ok
% '''

-spec open(pin(), config(), sample()) -> ok | {error, _}.
open(Pin, default, Sample) when is_atom(Pin), is_binary(Sample) or is_float(Sample) ->
    open(Pin, {10, <<1024:16>>}, Sample);
open(Pin, {Prescale, Period = <<_:16>>}, Sample) when is_number(Prescale), is_atom(Pin), is_binary(Sample) or is_float(Sample) ->
    open(Pin, {ipg_clk, Prescale, Period}, Sample);
open(Pin, {Clock, Prescale, Period = <<_:16>>}, Sample) when is_number(Prescale), is_atom(Pin), is_binary(Sample) or is_float(Sample) ->
    open_with_pwm_config(Pin, config(Clock, Prescale, Period), Sample).

-spec open_with_pwm_config(pin(), pwm_config(), sample()) -> ok | {error, _}.
open_with_pwm_config(Pin, Config, Sample) when is_atom(Pin), is_binary(Sample) or is_float(Sample) ->
    case sample_to_bin(Sample, Config#pwm_config.period) of
        {ok, SampleBin} ->
            gen_server:call(?MODULE, {open, Pin, Config, SampleBin});
        Error ->
            Error
        end.

% @doc Closes a pin.
-spec close(pin()) -> ok.
close(Pin) when is_atom(Pin) ->
    gen_server:call(?MODULE, {close, Pin}).

% @doc Sets a sample to define the duty cycle.
%
% You can pass a float between 0.0 and 1.0 or a 16bit binary.
% The binary value must be below or equal to the period (`<<1024:16>>' by default).
% === Float Sample Example ===
% This sets the duty cycle to 25%. This method is independent of the period used.
% ```
% 1> grisp_pwm:set_sample(gpio1_8, 0.25).
% ok
% '''
% === Binary Sample Example ===
% This sets the duty cycle to 25% given a period of `<<1024:16>>'.
% ```
% 1> grisp_pwm:set_sample(gpio1_8, <<256:16>>).
% ok
% '''
-spec set_sample(pin(), sample()) -> ok | {error | _}.
set_sample(Pin, Sample) when is_atom(Pin), is_binary(Sample) or is_float(Sample) ->
    gen_server:call(?MODULE, {set_sample, Pin, Sample}).

% @doc Creates a custom configuration for PWM.
%
% This creates a configuration with a given clock, prescale and period.
% This is useful if you want to define the cycle time or the duty cycle resolution.
% Different clocks can be selected to provide different source frequencies.
-spec config(clock(), prescale(), period()) -> pwm_config().
config(Clock, Prescale, Period = <<_:16>>) when is_atom(Clock), is_integer(Prescale), Prescale >= 1  ->
    #pwm_config{
        sample_repeat = 1,
        prescale = Prescale,
        clock = Clock,
        period = Period,
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

%% gen_server callbacks
%% @private
init([]) ->
    % Since this might be a restart we reset all PWM units to a known state.
    % This might stop running units but at least it ensures that the server
    % state matches the state of the hardware.
    [reset(PWMId) || PWMId <- lists:seq(1, 8)],
    ok = grisp_devices:register(pwm, ?MODULE),
    {ok, #state{pin_states = #{}}}.

%% @private
handle_call({open, Pin, Config, Sample}, _From, State) ->
    case maps:get(Pin, State#state.pin_states, nil) of
        nil ->
            case maps:get(Pin, ?PINMUXING, nil) of
                nil ->
                    {reply, {error, unknown_pin}, State};
                #{pwm_id := PWMId, register := MuxRegister, value := MuxValue} ->
                    % we have to make sure that the PWM unit is not used already
                    case [PinState || PinState <- maps:values(State#state.pin_states), PinState#pin_state.pwm_id == PWMId] of
                        [#pin_state{pin = ConflictingPin}] ->
                            {reply, {error, conflicting_pin, ConflictingPin}, State};
                        [] ->
                            % looks good, we can go ahead
                            % lets remember the current pin multiplexing
                            % so we can restore it on close/1
                            PreviousMuxValue = ?MODULE:get_register(MuxRegister),
                            % set the multiplexing options to connect
                            % the desired pin with the appropriate PWM unit
                            ?MODULE:set_register(MuxRegister, MuxValue),
                            PinState = #pin_state{
                                pin = Pin,
                                pwm_id = PWMId,
                                mux_register = MuxRegister,
                                previous_mux_value = PreviousMuxValue,
                                config = Config
                            },
                            ok = ?MODULE:setup(PWMId, Config, Sample),
                            NextState = State#state{ pin_states = maps:put(Pin, PinState, State#state.pin_states)},
                            {reply, ok, NextState}
                    end
            end;
        _   ->
            {reply, {error, already_open}, State}
    end;
handle_call({close, Pin}, _From, State) ->
    case maps:get(Pin, State#state.pin_states, nil) of
        nil ->
            % the pin is not in the state, we just confirm the close
            % so close/1 is idempotent
            {reply, ok, State};
        #pin_state{
            pwm_id = PWMId,
            mux_register = MuxRegister,
            previous_mux_value = PreviousMuxValue
        } ->
            reset(PWMId),
            ?MODULE:set_register(MuxRegister, PreviousMuxValue),
            NextState = State#state{ pin_states = maps:remove(Pin, State#state.pin_states)},
            {reply, ok, NextState}
    end;
handle_call({set_sample, Pin, Sample}, _From, State) ->
    Reply =
    case maps:get(Pin, State#state.pin_states, nil) of
        nil ->
            {error, pin_not_open};
        #pin_state{
            pwm_id = PWMId,
            config = #pwm_config{period = Period}
        } ->
            case sample_to_bin(Sample, Period) of
                {ok, SampleBin} ->
                    fill_sample_fifo(PWMId, SampleBin);
                Error ->
                    Error
            end
    end,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internal functions

%% @private
-spec setup(pwm_id(), pwm_config(), sample()) -> ok.
setup(PWMId, Config = #pwm_config{}, Sample) when is_number(PWMId), is_binary(Sample) ->
    % make sure PMW is disabled
    set_activation(PWMId, false),
    % configure PWM Control Register
    configure(PWMId, Config),
    % configure PWM Interrupt Register
    configure_interrupts(PWMId, default_interrupt_config()),
    % write to PWM Sample Register
    fill_sample_fifo(PWMId, Sample),
    % get PWM status
    Status = status(PWMId),
    % check FIFO Write Error status bit
    false = Status#status.fifo_write_error,
    % check Compare status bit
    false = Status#status.compare_event,
    % check Roll-over status bit
    false = Status#status.roll_over_event,
    % write PWM Period Register
    set_pwm_period(PWMId, Config#pwm_config.period),
    % enable PWM
    set_activation(PWMId, true),
    ok.

-spec set_pwm_period(pwm_id(), period()) -> ok.
set_pwm_period(PWMId, Period) when is_integer(PWMId), is_binary(Period) ->
    ?MODULE:set_register(address(PWMId, "PWMPR"), <<0:16, Period/binary>>).

-spec status(pwm_id()) -> status().
status(PWMId) ->
    <<_:25, FWE:1, CMP:1, ROV:1, FE:1, FIFOAV1:1, FIFOAV2:1, FIFOAV3:1>> = ?MODULE:get_register(address(PWMId, "PWMSR")),
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

-spec fill_sample_fifo(pwm_id(), sample()) -> ok.
fill_sample_fifo(PWMId, Sample) when is_integer(PWMId), is_binary(Sample) ->
    % setting one of the slots of the FIFO is enough
    ?MODULE:set_register(address(PWMId, "PWMSAR"), <<0:16, Sample/binary>>).

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
    ?MODULE:set_register(address(PWMId, "PWMIR"), Data).


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
        disconnected ->      <<1:1, 0:1>>
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
    ?MODULE:set_register(address(PWMId, "PWMCR"), Data),
    ok.

-spec reset(pwm_id()) -> ok.
reset(PWMId) when is_integer(PWMId)->
    Address = address(PWMId, "PWMCR"),
    <<Pre:28, _Reset:1, Post:3>> = ?MODULE:get_register(Address),
    Data = <<Pre:28, 1:1, Post:3>>,
    ?MODULE:set_register(Address, Data).


-spec set_activation(pwm_id(), pwm_activation()) -> ok.
set_activation(PWMId, Active) when is_number(PWMId), is_atom(Active) ->
    ActiveBit =
    case Active of
        true -> <<1:1>>;
        false -> <<0:1>>
    end,
    Address = address(PWMId, "PWMCR"),
    <<Rest:31, _:1>> = ?MODULE:get_register(Address),
    RegisterWithActivation = <<<<Rest:31>>/bitstring, ActiveBit/bitstring>>,
    ?MODULE:set_register(Address, RegisterWithActivation).

-spec sample_to_bin(sample(), period()) -> {ok, <<_:16>>} | {error, _}.
sample_to_bin(+0.0, _) -> {ok, <<0:16>>};
sample_to_bin(-0.0, _) -> {ok, <<0:16>>};
sample_to_bin(1.0, Period) -> {ok, Period};
sample_to_bin(Sample, Period = <<PeriodInt:16>>) when is_float(Sample), Sample >= 0.0, Sample =< 1.0 ->
    SampleInt = trunc(PeriodInt * Sample),
    sample_to_bin(<<SampleInt:16>>, Period);
sample_to_bin(Sample, Period) when is_binary(Sample), Sample =< Period ->
    {ok, Sample};
sample_to_bin(_, _) ->
    {error, sample_out_of_range}.

address(PWMId, Key) when is_number(PWMId), is_list(Key) ->
    maps:get(("PWM" ++ integer_to_list(PWMId) ++ "_" ++ Key), ?ADDRESSES).

% @private
on_load() -> ?NIF_LOAD.

%% @private
set_register(Address, <<Value:32/big>>) when is_number(Address) ->
    pwm_set_register32_nif(Address, Value).

%% @private
get_register(Address) when is_number(Address) ->
    Value = pwm_get_register32_nif(Address),
    <<Value:32/big>>.

pwm_get_register32_nif(Address) -> ?NIF_STUB([Address]).

pwm_set_register32_nif(Address, Value) -> ?NIF_STUB([Address, Value]).
