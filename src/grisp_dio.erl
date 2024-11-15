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

%--- API -----------------------------------------------------------------------

start(Opts) -> grisp_sup:start_child(?MODULE, [Opts]).

pins() -> call(pins).

configure(Pin, Opt, Value) -> call({configure, validate(Pin), Opt, Value}).

describe(Pin) -> call({describe, validate(Pin)}).

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
