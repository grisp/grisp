-module(ieee802154).
-behaviour(gen_server).

%%% @headerfile "ieee802154.hrl"

% API
-export([start_link/1]).
-export([start/1]).
-export([stop_link/0]).
-export([stop/0]).

-export([transmission/1]).
-export([transmission/2]).
-export([reception/0]).

-export([rx_on/0]).
-export([rx_off/0]).

-export([get_pib_attribute/1]).
-export([set_pib_attribute/2]).

-export([reset/1]).

% gen_server callbacks
-export([init/1]).
-export([terminate/2]).
-export([code_change/4]).
-export([handle_call/3]).
-export([handle_cast/2]).


% Includes
-include("ieee802154.hrl").
-include("ieee802154_pib.hrl").
-include("mac_frame.hrl").

%--- Types ---------------------------------------------------------------------

-type state() :: #{phy_layer := module(),
                   duty_cycle := gen_duty_cycle:state(),
                   pib := pib_state(),
                   _:=_}.

%--- API -----------------------------------------------------------------------

%% @doc Starts the IEEE 812.15.4 stack and creates a link
%%
%% ```
%% The following code will start the stack with the default parameters
%% 1> ieee802154:start_link(#ieee_parameters{}).
%%
%% Using a custom callback function
%% 2> ieee802154:start_link(#ieee_parameters{input_callback = fun callback/4}).
%%
%% Using a custom phy module
%% 3> ieee802154:start_link(#ieee_parameters{phy_layer = mock_phy_network}).
%% '''
%%
%% @param Params: A map containing the parameters of the IEEE stack
%%
%% @end
-spec start_link(Params) -> {ok, pid()} | {error, any()} when
      Params :: ieee_parameters().
start_link(Params) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

%% @doc Same as start_link/1 but no link is created
%% @end
-spec start(Params) -> {ok, pid()} | {error, any()} when
      Params :: ieee_parameters().
start(Params) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Params, []).

stop_link() ->
    gen_server:stop(?MODULE).

stop() -> gen_server:stop(?MODULE).

%% @doc
%% @equiv transmission(Frame, 0)
%% @end
-spec transmission(Frame) -> Result when
      Frame        :: frame(),
      Result       :: {ok, Ranging} | {error, Error},
      Ranging      :: ranging_informations(),
      Error        :: tx_error().
transmission(Frame) ->
    transmission(Frame, ?NON_RANGING).

%% @doc Performs a transmission on the defined IEEE 802.15.4 stack
%% When ranging has been activated for the frame, the second element of the
%% tuple contains different values that can be used for ranging operations
%% For more informations please consult the IEEE 802.15.4 standard.
%% Note that the variable `Timestamp' is omited because its value is the same
%% as `Ranging counter start'
%%
%% When Ranging isn't activated, the 2nd element of the tuple shall be ignored
%% ```
%% Ranging not activated for transmission
%% 1> ieee802154:transmission(Frame, ?NON_RANGING).
%%
%% Activate ranging for the transmission
%% 2> ieee802154:transmission(Frame, ?ALL_RANGING).
%% '''
%% @end
-spec transmission(Frame, Ranging) -> Result when
      Frame        :: frame(),
      Ranging      :: ranging_tx(),
      Result       :: {ok, RangingInfos} | {error, Error},
      RangingInfos :: ranging_informations(),
      Error        :: tx_error().
transmission(Frame, Ranging) ->
    {FH, _, _} = Frame,
    case FH of
        #frame_control{dest_addr_mode = ?NONE, src_addr_mode = ?NONE} ->
            {error, invalid_address};
        _ ->
            gen_server:call(?MODULE,
                            {tx, Frame, Ranging},
                            infinity)
    end.

%% @doc Performs a reception on the IEEE 802.15.4 stack
%% @deprecated This function will be deprecated
%% @end
-spec reception() -> Result when
      Result :: {ok, frame()} | {error, atom()}.
reception() ->
    gen_server:call(?MODULE, {rx}, infinity).

%% @doc Turns on the continuous reception
%% Ranging is by default switched on
%% @end
-spec rx_on() -> Result when
      Result :: ok | {error, atom()}.
rx_on() ->
    gen_server:call(?MODULE, {rx_on}).

%% @doc Turns off the continuous reception
%% @end
rx_off() ->
    gen_server:call(?MODULE, {rx_off}).

%% @doc Get the value of a PIB attribute
%% @end
-spec get_pib_attribute(Attribute) -> Value when
      Attribute :: pib_attribute(),
      Value     :: term().
get_pib_attribute(Attribute) ->
    gen_server:call(?MODULE, {get, Attribute}).


%% @doc Set the value of a PIB attribute
%% @end
-spec set_pib_attribute(Attribute, Value) -> ok when
      Attribute :: pib_attribute(),
      Value     :: term().
set_pib_attribute(Attribute, Value) ->
    gen_server:call(?MODULE, {set, Attribute, Value}).

-spec reset(SetDefaultPIB) -> Result when
      SetDefaultPIB :: boolean(),
      Result :: ok.
reset(SetDefaultPIB) ->
    gen_server:call(?MODULE, {reset, SetDefaultPIB}).

%--- gen_statem callbacks ------------------------------------------------------

-spec init(Params) -> {ok, State} when
      Params :: ieee_parameters(),
      State  :: state().
init(Params) ->
    {ok, _GenEvent} = gen_event:start_link({local, ?GEN_EVENT}),
    PhyMod = Params#ieee_parameters.phy_layer,
    InputCallback = Params#ieee_parameters.input_callback,
    write_default_conf(PhyMod),

    ok = ieee802154_events:start(#{input_callback => InputCallback}),

    DutyCycleState = gen_duty_cycle:start(Params#ieee_parameters.duty_cycle,
                                          PhyMod),

    Data = #{phy_layer => PhyMod,
             duty_cycle => DutyCycleState,
             pib => ieee802154_pib:init(PhyMod),
             ranging => ?DISABLED},
    {ok, Data}.

-spec terminate(Reason, State) -> ok when
      Reason :: term(),
      State :: state().
terminate(Reason, #{duty_cycle := GenDutyCycleState}) ->
    ieee802154_events:stop(),
    gen_event:stop(?GEN_EVENT),
    gen_duty_cycle:stop(GenDutyCycleState, Reason).

code_change(_, _, _, _) ->
    error(not_implemented).

-spec handle_call(_, _, State) -> Result when
      State   :: state(),
      Result  :: {reply, term(), State}.
handle_call({rx_on}, _From, State) ->
    #{duty_cycle := DCState} = State,
    case gen_duty_cycle:turn_on(DCState) of
        {ok, NewDutyCycleState} ->
            {reply, ok, State#{duty_cycle => NewDutyCycleState}};
        {error, NewDutyCycleState, Error} ->
            {reply, {error, Error}, State#{duty_cycle => NewDutyCycleState}}
    end;
handle_call({rx_off}, _From, #{duty_cycle := DCState} = State) ->
    NewDCState = gen_duty_cycle:turn_off(DCState),
    {reply, ok, State#{duty_cycle => NewDCState}};
handle_call({tx, Frame, Ranging}, _From, State) ->
    #{duty_cycle := DCState, pib := Pib} = State,
    {FrameControl, MacHeader, Payload} = Frame,
    EncFrame = mac_frame:encode(FrameControl, MacHeader, Payload),
    case gen_duty_cycle:tx_request(DCState, EncFrame, Pib, Ranging) of
        {ok, NewDCState, RangingInfos} ->
            timer:sleep(1),
            % timer:sleep(100), % FIXME: IFS
            {reply, {ok, RangingInfos}, State#{duty_cycle => NewDCState}};
        {error, NewDCState, Error} ->
            {reply, {error, Error}, State#{duty_cycle => NewDCState}}
        end;
handle_call({get, Attribute}, _From, State) ->
    #{pib := Pib} = State,
    case ieee802154_pib:get(Pib, Attribute) of
        {error, Error} ->
            {reply, {error, Error}, State};
        Value ->
            {reply, Value, State}
    end;
handle_call({set, Attribute, Value}, _From, State) ->
    #{pib := Pib} = State,
    case ieee802154_pib:set(Pib, Attribute, Value) of
        {ok, NewPib} ->
            {reply, ok, State#{pib => NewPib}};
        {error, NewPib, Error} ->
            {reply, {error, Error}, State#{pib => NewPib}}
    end;
handle_call({reset, SetDefaultPIB}, _From, State) ->
    #{phy_layer := PhyMod, pib := Pib, duty_cycle := DCState} = State,
    NewState = case SetDefaultPIB of
                   true ->
                       PhyMod:write(panadr, #{pan_id => <<16#FFFF:16>>,
                                              short_addr => <<16#FFFF:16>>}),
                       State#{pib => ieee802154_pib:reset(Pib)};
                   _ ->
                       State
               end,
    NewDCState = gen_duty_cycle:turn_off(DCState),
    {reply, ok, NewState#{duty_cycle => NewDCState, ranging => ?DISABLED}};
handle_call(_Request, _From, _State) ->
    error(call_not_recognized).

handle_cast(_, _) ->
    error(not_implemented).

%--- Internal ------------------------------------------------------------------
-spec write_default_conf(PhyMod :: module()) -> ok.
write_default_conf(PhyMod) ->
    PhyMod:write(rx_fwto, #{rxfwto => ?MACACKWAITDURATION}),
    PhyMod:write(sys_cfg, #{ffab => 1,
                            ffad => 1,
                            ffaa => 1,
                            ffam => 1,
                            ffen => 1,
                            autoack => 1,
                            rxwtoe => 1}).