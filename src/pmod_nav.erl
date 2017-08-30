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

%--- Records -------------------------------------------------------------------

-record(state, {
    slot,
    chip
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
    State = #state{slot = Slot, chip = accelerometer},
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

verify_device(State) ->
    Verifications = [
        {accelerometer, <<?RW_READ:1, ?WHO_AM_I:7>>, <<?WHO_AM_I_DEFAULT>>},
        {magnetometer, <<?RW_READ:1, ?MS_INCR:1, ?WHO_AM_I_M:6>>, <<?WHO_AM_I_M_DEFAULT>>}
    ],
    lists:foldl(fun verify_device_who_am_i/2, State, Verifications).

verify_device_who_am_i({Component, Request, Val}, State) ->
    case request(State, Component, Request, byte_size(Val)) of
        {NewState, Val} -> NewState;
        {_State, Other} -> error({device_mismatch, Component, who_am_i, Other})
    end.

request(State, Component, Request, Pad) ->
    NewState = chip_select(State, Component),
    Result = grisp_spi:send_recv(State#state.slot, ?SPI_MODE, Request, byte_size(Request), Pad),
    {NewState, Result}.

chip_select(#state{chip = Chip} = State, Chip) ->
    State;
chip_select(State, accelerometer) ->
    grisp_gpio:set(spi1_pin9),
    grisp_gpio:set(spi1_pin10),
    grisp_gpio:clear(ss1),
    State#state{chip = accelerometer};
chip_select(State, magnetometer) ->
    grisp_gpio:set(ss1),
    grisp_gpio:set(spi1_pin10),
    grisp_gpio:clear(spi1_pin9),
    State#state{chip = magnetometer};
chip_select(State, altimeter) ->
    grisp_gpio:set(ss1),
    grisp_gpio:set(spi1_pin9),
    grisp_gpio:clear(spi1_pin10),
    State#state{chip = altimeter}.
