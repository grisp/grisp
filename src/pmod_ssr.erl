% @doc
% <a href="https://digilent.com/reference/pmod/pmodssr/reference-manual">
% PmodSSR</a>
% module.
%
% The Pmod SSR (Solid State Relay) module is designed to control high-power devices using a GPIO pin from a GRiSP board.
%
% Pin 1: ON/OFF control (connected to GPIO).
% Pin 5: Ground (GND).
% Pin 6: 3.3V Power Supply (VCC).
%
% This relay can be switched on or off by sending a control signal through a single GPIO pin (minimal setup needed for controlling high-power loads).
% The driver allows you to easily turn the relay on and off through simple commands.
%
%
% Start the driver with
% ```
% 1> grisp:add_device(gpio1, pmod_ssr).
% '''
% @end
-module(pmod_ssr).

-behaviour(gen_server).

-include("grisp_internal.hrl").
%--- Exports -------------------------------------------------------------------

% API
-export([start_link/2]).
-export([on/0]).
-export([off/0]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).


%--- API -----------------------------------------------------------------------

% @private
start_link(Slot, _Opts) ->
    gen_server:start_link(?MODULE, Slot, []).

% @doc Turn the relay on and off.
%
% === Examples ===
% To turn on, use:
% ```
% 2> pmod_ssr:on().
% ok
% '''
% To turn back off, use:
% ```
% 3> pmod_ssr:off().
% ok
% '''

-spec on() -> ok | {error, any()}.
on() ->
    call(on).

-spec off() -> ok | {error, any()}.
off() ->
    call(off).


%--- Callbacks -----------------------------------------------------------------

% @private
init(Slot) ->
    % TODO: expand for more possible Slots
    case {grisp_hw:platform(), Slot} of
        {_, gpio1} -> ok;
        {grisp_base, gpio2} -> ok;
        {P, S} -> error({incompatible_slot, P, S})
    end,
    Pin1 = grisp_gpio:open(gpio1_1, #{mode => {output, 0}}),
    process_flag(trap_exit, true),
    grisp_devices:register(Slot, ?MODULE),
    {ok, #{pin => Pin1}}.

% @private
handle_call(Call, _From, State) ->
    try execute_call(Call, State)
        catch throw:Reason -> {reply, {error, Reason}, State}
    end.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(_Any, State) ->
    {noreply, State}.


%--- Internal ------------------------------------------------------------------

call(Call) ->
    Dev = grisp_devices:default(?MODULE),
    case gen_server:call(Dev#device.pid, Call) of
        {error, Reason} -> error(Reason);
        Result          -> Result
    end.

execute_call(on, #{pin := Pin} = State) ->
    {reply, set(Pin, 1), State};
execute_call(off, #{pin := Pin} = State) ->
    {reply, set(Pin, 0), State};
execute_call(Request, _State) ->
    error({unknown_call, Request}).

set(Pin, Value) ->
    grisp_gpio:set(Pin, Value).
