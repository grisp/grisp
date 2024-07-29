% @doc Digilent Pmod_ALS module
% This component provides ambient light-to-digital
% sensing through the SPI2 interface on a 6-pin connector.
%
% For further information about the Digilent Pmod_ALS
% and its components :
% https://store.digilentinc.com/pmod-als-ambient-light-sensor/
%
% Texas Instrument's ADC081S021 analog-to-digital converter
% http://www.ti.com/lit/ds/symlink/adc081s021.pdf
%
% Vishay Semiconductor's TEMT6000X01.
% http://www.vishay.com/docs/81579/temt6000.pdf
%
% Start the driver with
% ```
%  1> grisp:add_device(spi2, pmod_als).
% '''
% @end
-module(pmod_als).

-behaviour(gen_server).

-include("grisp_internal.hrl").

% API
-export([start_link/2]).
-export([read/0]).
-export([percentage/0]).

% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%--- Macros --------------------------------------------------------------------

-define(SPI_MODE, #{clock => {high, trailing}}).

%--- Records -------------------------------------------------------------------

-record(state , {
    % options = [],
    bus
}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot, _Opts) -> gen_server:start_link(?MODULE, Slot, []).

% @doc Returns the ambient light value that is currently sensed
% by the ALS module. On success, the return value is a number
% in the 0..255 range that is proportional to the luminous
% intensity.
%
% Technically, the values are representative of the
% power perceived by the phototransistor from the light source
% incoming at an angle of ±60°. It is a model of the response
% of the human eye to the same light source that is obtained
% by calculating the luminosity function.
%
% The peak wavelength sensitivity of the module is at 570nm
% making it close to the human eye (555nm). This implies that
% return values will be the highest when the ALS is exposed to
% wavelengths of green light with a slight yellow tint.
-spec read() -> 0..255 | no_return().
read() ->
    Dev = grisp_devices:default(?MODULE),
    case gen_server:call(Dev#device.pid, read) of
        {error, Reason} -> error(Reason);
        Result          -> Result
    end.

% @doc Returns a the percentage of current ambient light
% based on the {@link pmod_als:read/0} function. The value
% is rounded to the closest integer.
-spec percentage() -> 0..100.
percentage() ->
    Raw = read(),
    round((Raw / 255) * 100).

%--- Callbacks -----------------------------------------------------------------

% @private
init(Slot) ->
    ok = grisp_devices:register(Slot, ?MODULE),
    {ok, #state{bus = grisp_spi:open(Slot)}}.

% @private
handle_call(read, _From, #state{bus = Bus} = State) ->
    Val = get_value(Bus),
    {reply, Val, State};

handle_call(Request, _From, _State) -> error({unknown_call, Request}).

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
terminate(_Reason, _State) -> ok.

% @private
code_change(_OldVsn, State, _Extra) -> {ok , State}.

%--- Internal ------------------------------------------------------------------

get_value(Bus) ->
    [<<_:3, Resp:8, _Pad:5>>] = grisp_spi:transfer(Bus, [
        {?SPI_MODE, <<0:8>>, 0, 1}
    ]),
    Resp.
