-module(pmod_nav).

-behavior(gen_server).

% API
-export([start_link/1]).
-export([linear_acceleration/0]).

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

% TODO: Make a termination fun for grisp_devices:register

%--- Records -------------------------------------------------------------------

-record(state, {
    slot,
    acc = #{
        mode => '2g'
    },
    mag,
    alt
}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot) -> gen_server:start_link(?MODULE, Slot, []).

linear_acceleration() -> convert_acceleration(call(linear_acceleration)).

%--- Callbacks -----------------------------------------------------------------

% @private
% FIXME: Raise proper error for port mismatch
init(Slot = spi1) ->
    try
        configure_pins(Slot),
        verify_device(Slot),
        initialize_device(Slot)
    catch
        Class:Reason ->
            restore_pins(Slot),
            erlang:raise(Class, Reason, erlang:get_stacktrace())
    end,
    grisp_devices:register(Slot, ?MODULE),
    {ok, #state{slot = Slot}}.

% @private
handle_call(linear_acceleration, _From, State) ->
    {reply, {State#state.acc, linear_acceleration(State#state.slot)}, State};
handle_call(Request, _From, _State) ->
    error({unknown_call, Request}).

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, State) -> restore_pins(State#state.slot).

%--- Internal ------------------------------------------------------------------

call(Call) ->
    Dev = grisp_devices:default(?MODULE),
    gen_server:call(Dev#device.pid, Call).

verify_device(Slot) ->
    [verify_device_who_am_i(Slot, V) || V <- [
        {acc,
            <<?RW_READ:1, ?ACC_WHO_AM_I:7>>,
            <<?ACC_WHO_AM_I_DEFAULT>>
        },
        {mag,
            <<?RW_READ:1, ?MS_SAME:1, ?MAG_WHO_AM_I:6>>,
            <<?MAG_WHO_AM_I_DEFAULT>>
        },
        {alt,
            <<?RW_READ:1, ?MS_SAME:1, ?ALT_WHO_AM_I:6>>,
            <<?ALT_WHO_AM_I_DEFAULT>>
        }
    ]].

verify_device_who_am_i(Slot, {Component, Request, Val}) ->
    select(Component, fun() ->
        case request(Slot, Request, byte_size(Val)) of
            Val   -> ok;
            Other -> error({device_mismatch, Component, who_am_i, Other})
        end
    end).

initialize_device(Slot) ->
    select(acc, fun() ->
        % Enable all 3 axises
        request(Slot, <<?RW_WRITE:1, ?CTRL_REG5_XL:7, 2#00111000>>, 0),
        % Enable 10Hz mode
        request(Slot, <<?RW_WRITE:1, ?CTRL_REG6_XL:7, 2#001:3, 0:5>>, 0)
    end).

linear_acceleration(Slot) ->
    select(acc, fun() ->
        <<X:16/signed-little, Y:16/signed-little, Z:16/signed-little>>
            = request(Slot, <<?RW_READ:1, ?ACC_OUT_X_XL:7>>, 6),
        {X, Y, Z}
    end).

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

convert_acceleration({#{mode := Mode}, {X, Y, Z}}) ->
    {X * lsb_range(Mode), Y * lsb_range(Mode), Z * lsb_range(Mode)}.

lsb_range('2g')  -> 0.000061;
lsb_range('4g')  -> 0.000122;
lsb_range('8g')  -> 0.000244;
lsb_range('16g') -> 0.000732.
