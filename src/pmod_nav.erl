-module(pmod_nav).

-behavior(gen_server).

% API
-export([start_link/1]).
-export([g/0]).

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
    slot
}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot) -> gen_server:start_link(?MODULE, Slot, []).

g() -> 0.

%--- Callbacks -----------------------------------------------------------------

% @private
% FIXME: Raise proper error for port mismatch
init(Slot = spi1) ->
    % Disable chip select for SPI1 and pull it low
    grisp_gpio:configure(ss1, output_0),
    % Configure pin 9 and 10 for output pulled high
    grisp_gpio:configure(spi1_pin9, output_1),
    grisp_gpio:configure(spi1_pin10, output_1),
    % ss1 pulled low and pin 9 and 10 pulled high means accelerometer is active
    State = #state{slot = Slot},
    State2 = verify_device(State),
    grisp_devices:register(State2#state.slot, ?MODULE),
    {ok, State2}.

% @private
handle_call(Request, _From, _State) -> error({unknown_call, Request}).

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, State) ->
    grisp_gpio:configure_slot(State#state.slot, enable_cs),
    grisp_gpio:configure(spi1_pin9, input),
    grisp_gpio:configure(spi1_pin10, input).

%--- Internal ------------------------------------------------------------------

call(Call) ->
    Dev = grisp_device:default(?MODULE),
    gen_server:call(Dev#device.pid, Call).

verify_device(#state{slot = Slot}) ->
    [verify_device_who_am_i(Slot, V) || V <- [
        {accelerometer,
            <<?RW_READ:1, ?WHO_AM_I:7>>,
            <<?WHO_AM_I_DEFAULT>>
        },
        {magnetometer,
            <<?RW_READ:1, ?MS_INCR:1, ?WHO_AM_I_M:6>>,
            <<?WHO_AM_I_M_DEFAULT>>
        }
    ]].

verify_device_who_am_i(Slot, {Component, Request, Val}) ->
    select(Component, fun() ->
        case request(Slot, Request, byte_size(Val)) of
            Val   -> ok;
            Other -> error({device_mismatch, Component, who_am_i, Other})
        end
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

pin(accelerometer) -> ss1;
pin(magnetometer)  -> spi1_pin9;
pin(altimeter)     -> spi1_pin10.
