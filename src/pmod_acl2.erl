-module(pmod_acl2).

-behavior(gen_server).

-include("grisp.hrl").
-include("pmod_acl2.hrl").

% API
-export([start_link/2]).
-export([raw/0]).
-export([g/0]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(SPI_MODE, #{cpol => low, cpha => leading}).

%--- Records -------------------------------------------------------------------

-record(state, {
    slot,
    mode = '2g'
}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot, _Opts) -> gen_server:start_link(?MODULE, Slot, []).

raw() ->
    {_Mode, Result} = call(raw),
    Result.

g() ->
    {Mode, Result} = call(raw),
    scale(Mode, Result).

%--- Callbacks -----------------------------------------------------------------

% @private
init(Slot) ->
    verify_device(Slot),
    grisp_devices:register(Slot, ?MODULE),
    Req = <<?WRITE_REGISTER, ?POWER_CTL, 0:6, ?MEASUREMENT_MODE:2>>,
    grisp_spi:send_recv(Slot, ?SPI_MODE, Req),
    {ok, #state{slot = Slot}}.

% @private
handle_call(raw, _From, State) ->
    Raw = xyz(State#state.slot),
    {reply, {State#state.mode, Raw}, State}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------

call(Call) ->
    Dev = grisp_devices:default(?MODULE),
    gen_server:call(Dev#device.pid, Call).

xyz(Slot) ->
    <<
        XDATA_L,
        _:4, XDATA_H:4,
        YDATA_L,
        _:4, YDATA_H:4,
        ZDATA_L,
        _:4, ZDATA_H:4
    >> = grisp_spi:send_recv(Slot, ?SPI_MODE, <<?READ_REGISTER, ?XDATA_L>>, 2, 6),
    <<X:12/signed>> = <<XDATA_H:4, XDATA_L>>,
    <<Y:12/signed>> = <<YDATA_H:4, YDATA_L>>,
    <<Z:12/signed>> = <<ZDATA_H:4, ZDATA_L>>,
    {X, Y, Z}.

scale('2g', {X, Y, Z}) -> {X / 1000, Y / 1000, Z / 1000}.

verify_device(Slot) ->
    [verify_device_reg(Slot, Msg, Reg, Val) || {Msg, Reg, Val} <- [
        {analog_devices_device_id,      ?DEVID_AD,  ?AD_DEVID},
        {analog_devices_mems_device_id, ?DEVID_MST, ?AD_MEMS_DEVID},
        {device_id,                     ?PARTID,    ?DEVID}
    ]].

verify_device_reg(Slot, Message, Reg, Val) ->
    case grisp_spi:send_recv(Slot, ?SPI_MODE, <<?READ_REGISTER, Reg>>, 2, 1) of
        <<Val>> -> ok;
        Other   -> error({device_mismatch, {Message, Other}})
    end.
