% @doc High level DIO-API
%
% This module provides a high-level API for the PmodDIO. It is built on top of the low level driver that you can find under the name `pmod_dio'.
%
% To start the API process:
% ```
% 1> grisp_dio:start(#{devices => [{spi2, pmod_dio, #{chips => [1]}}]}).
% '''
% Here we start the process for the chip with the address 0x00 on the spi2 bus
%
% @end
-module(grisp_dio).

-behavior(gen_server).

% API
-export([start/1]).
-export([pins/0]).
-export([configure/3]).
-export([describe/1]).
-export([get/1]).

% Callbacks
-export([start_link/0]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-include("grisp_internal.hrl").

-define(DEFAULT_OPTS, #{devices => []}).

%--- Types ---------------------------------------------------------------------

-type pin() :: {grisp_spi:bus(), pmod_dio:chip(), pmod_dio:channel()}.
-type option() :: direction.

%--- API -----------------------------------------------------------------------

start(Opts) -> grisp_sup:start_child(?MODULE, [Opts]).

%% @doc gets an array of all the possible combination of {Bus, Chip, Channel}
%% Bus is a SPI bus on the board
%% Chip is the id of a connected pmod (from 1 to 4)
%% Channel is one of the channel number of the specific Chip
%%
%% === Example ===
%% In this case, 2 chips (0x00 and 0x10) are connected to the spi2 bus:
%% ```
%% 1> grisp_dio:pins().
%% [{spi2, 1, 1},
%%  {spi2, 1, 2},
%%  {spi2, 1, 3},
%%  {spi2, 1, 4},
%%  {spi2, 4, 1},
%%  {spi2, 4, 2},
%%  {spi2, 4, 3},
%%  {spi2, 4, 4}].
%% '''
%% @end
-spec pins() -> [pin()].
pins() -> call(pins).

%% @doc Configure the given pin
%% For a given pin you can change its configuration option.
%% List of options:
%% <ul>
%%     <li>`direction': Describes if the channel is either `input' or `output'</li>
%% </ul>
%% @end
-spec configure(Pin, Opt, Value) -> ok when
      Pin   :: pin(),
      Opt   :: option(),
      Value :: input | output.
configure(Pin, Opt, Value) -> call({configure, validate(Pin), Opt, Value}).

%% @doc Gives the configuration of the provided pin
%%
%% === Example ===
%% ==== Output Mode Example ====
%% Describe the pin 1 of device 0x00 on spi2 bus
%% ```
%% 1> grisp_dio:describe({spi2, 1, 1}).
%% #{output => 
%%     #{current_limit =>
%%         #{current_limit => {miliampere, 600},
%%           inrush => {milisecond, 20}},
%%       mode => {high_side, {inrush_multiplier, 1}}}}
%% '''
%% ==== Input Mode Example ====
%% 1> grisp_dio:describe({spi2, 1, 2}).
%% #{input => #{}}
%% '''
%% @end
-spec describe(Pin :: pin()) -> map().
describe(Pin) -> call({describe, validate(Pin)}).

%% @doc Get the status of DoiLevel/VDDOKFault for the given pin
-spec get(Pin::pin()) -> 0 | 1.
get(Pin) -> call({get, validate(Pin)}).

%--- Callbacks -----------------------------------------------------------------

start_link() -> start_link(#{}).

start_link(UserOpts) ->
    Opts = maps:merge(?DEFAULT_OPTS, UserOpts),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

init(#{devices := Devices}) ->
    State = lists:foldl(
        fun(D, S) -> add_device(D, S) end, #{devices => #{}}, Devices
    ),
    {ok, State}.

handle_call(pins, _From, #{devices := Devices} = State) ->
    Pins = [
        {S, C, P}
     || #device{slot = S} <- maps:values(Devices),
        C <- pmod_dio:chips(S),
        P <- [1, 2, 3, 4]
    ],
    {reply, Pins, State};
handle_call({configure, Pin, Opt, Value}, _From, State) ->
    {reply, ok, configure_pin(Pin, Opt, Value, State)};
handle_call({describe, Pin}, _From, State) ->
    {reply, describe_pin(Pin), State};
handle_call({get, Pin}, _From, State) ->
    {reply, get_pin(Pin), State};
handle_call(Request, From, _State) ->
    error({unknown_request, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info(Info, _State) -> error({unknown_info, Info}).

%--- Internal ------------------------------------------------------------------

validate({_Slot, _Chip, N} = Pin) when N >= 1, N =< 4 -> Pin;
validate(Pin) -> error({invalid_pin, Pin}).

call(Request) ->
    case gen_server:call(?MODULE, Request) of
        {error, Reason} -> error(Reason);
        Reply -> Reply
    end.

add_device({Slot, Mod, Args}, #{devices := Devices} = State) ->
    case maps:find({Slot, Mod}, Devices) of
        {ok, _} ->
            error({device_already_added, Slot, Mod});
        error ->
            DeviceArgs = maps:merge(Args, #{access => exclusive}),
            Dev = grisp:add_device(Slot, Mod, DeviceArgs),
            configure_device(Dev),
            State#{devices := Devices#{{Slot, Mod} => Dev}}
    end.

configure_device(#device{slot = Slot, driver = pmod_dio}) ->
    lists:foreach(
        fun(Chip) -> configure_chip(Slot, Chip) end,
        pmod_dio:chips(Slot)
    ).

configure_chip(Slot, Chip) ->
    lists:foreach(fun({Reg, Value}) -> write_reg(Slot, Chip, Reg, Value) end, [
        {'Config1', #{'FLEDSet' => false, 'SLEDSet' => false}}
    ]).

describe_pin({Slot, Chip, N}) ->
    #{{'SetDi_', N} := Direction} = read_reg(Slot, Chip, 'SetOUT'),
    #{{'DoMode_', N} := OutputMode} = read_reg(Slot, Chip, 'ConfigDO'),
    #{{'CL_', N} := CurrentLimit} = read_reg(Slot, Chip, 'CurrLim'),
    case Direction of
        output ->
            #{
                Direction => #{
                    mode => OutputMode,
                    current_limit => CurrentLimit
                }
            };
        input ->
            #{Direction => #{}}
    end.

configure_pin({Slot, Chip, N}, direction, Direction, State) ->
    write_reg(Slot, Chip, 'SetOUT', #{{'SetDi_', N} => Direction}),
    State;
configure_pin(_Pin, Opt, _Value, State) ->
    throw({reply, {error, {unknown_option, Opt}}, State}).

get_pin({Slot, Chip, N}) ->
    #{{'DoiLevel_/VDDOKFault_', N} := Level} = read_reg(Slot, Chip, 'DoiLevel'),
    Level.

read_reg(Slot, Chip, Reg) ->
    #{result := #{Reg := Current}} = pmod_dio:read(Slot, Chip, Reg),
    Current.

write_reg(Slot, Chip, Reg, Value) ->
    Current = read_reg(Slot, Chip, Reg),
    pmod_dio:write(Slot, Chip, Reg, maps:merge(Current, Value)).
