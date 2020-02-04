%% -----------------------------------------------------------------------------
%% @doc GRiSP API for configuration of pins
%% @end
%% -----------------------------------------------------------------------------
-module(grisp_gpio).

-behavior(gen_server).

% Types
-type pin() :: integer()
         | gpio1_1 | gpio1_2 | gpio1_3 | gpio1_4
         | gpio2_1 | gpio2_2 | gpio2_3 | gpio2_4
         | led1_r | led1_g | led1_b | led2_r | led2_g | led2_b
         | jumper_1 | jumper_2 | jumper_3 | jumper_4 | jumper_5
         | spi1_pin7 | spi1_pin8 | spi1_pin9 | spi1_pin10
         | ss1 | ss2
         | uart_1_cts | uart_2_txd | uart_3_rxd | uart_4_rts.
%% See the GRiSP Wiki for the <a href="https://github.com/grisp/grisp/wiki/Physical-to-atom-pin-mappings">physical to atom pin mappings</a>.

-type pin_type() :: periph_a | periph_b | periph_c | periph_d | input
                    | output_0 | output_1.

-type slot() :: gpio1 | gpio2 | spi1 | spi2.

% API
-export([start_link/1]).
-export([configure/2]).
-export([configure/3]).
-export([configure_slot/2]).
-export([get/1]).
-export([clear/1]).
-export([set/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- Records -------------------------------------------------------------------

-record(state, {driver}).

%--- API -----------------------------------------------------------------------

% @private
start_link(DriverMod) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, DriverMod, []).

%% @equiv configure(Pin, Type, [default])
-spec configure(pin(), pin_type()) -> any().
configure(Pin, Type) -> configure(Pin, Type, [default]).

%% @doc Configure pin type.
-spec configure(pin(), pin_type(), [default]) -> any().
configure(Pin, Type, Attr) ->
    Command = <<(index(Pin)):8, 1:8, (map_type(Type)):8, (map_attr(Attr)):8>>,
    gen_server:call(?MODULE, {command, Command}).

%% @doc Configure slot.
%%
%% The GPIO slots can be configured with `{T1, T2, T3, T4}'
%% and the SPI slots allow the configuration `disable_cs | enable_cs'.
-spec configure_slot(Slot::slot(),
                     Config::{T1::pin_type(), T2::pin_type(), T3::pin_type(), T4::pin_type()}
                        | disable_cs | enable_cs) -> ok.
configure_slot(gpio1, {T1, T2, T3, T4}) ->
    configure(gpio1_1, T1),
    configure(gpio1_2, T2),
    configure(gpio1_3, T3),
    configure(gpio1_4, T4),
    ok;
configure_slot(gpio2, {T1, T2, T3, T4}) ->
    configure(gpio2_1, T1),
    configure(gpio2_2, T2),
    configure(gpio2_3, T3),
    configure(gpio2_4, T4),
    ok;
configure_slot(spi1, disable_cs) -> configure(ss1, output_1);
configure_slot(spi1, enable_cs)  -> configure(ss1, periph_c);
configure_slot(spi2, disable_cs) -> configure(ss2, output_1);
configure_slot(spi2, enable_cs)  -> configure(ss2, periph_c).

%% @doc Gets the value of a pin.
%%
%% === Example ===
%% To see whether the red led of `led_1' is on use:
%% ```
%% 2> grisp_gpio:get(led1_r).
%% false
%% '''
%% In this case it was off.
- spec get(pin()) -> any().
get(Pin) -> gen_server:call(?MODULE, {command, <<(index(Pin)):8, 2:8>>}).

%% @doc Sets a pin to `false'.
%%
%% === Example ===
%% Turn off the red led of `led_1' with:
%% ```
%% 1> grisp_gpio:clear(led1_r).
%% <<>>
%% '''
- spec clear(pin()) -> any().
clear(Pin) -> gen_server:call(?MODULE, {command, <<(index(Pin)):8, 3:8>>}).

%% @doc Sets a pin to `true'.
%%
%% === Example ===
%% Turn on the red led of `led_1' with:
%% ```
%% 3> grisp_gpio:set(led1_r).
%% <<>>
%% '''
- spec set(pin()) -> any().
set(Pin) -> gen_server:call(?MODULE, {command, <<(index(Pin)):8, 4:8>>}).


%--- Callbacks -----------------------------------------------------------------

% @private
init(DriverMod) ->
    Ref = DriverMod:open(),
    {ok, #state{driver = {DriverMod, Ref}}}.

% @private
handle_call({command, Command}, _From, State) ->
    {DriverMod, Ref} = State#state.driver,
    Result = DriverMod:command(Ref, Command),
    {reply, bool(Result), State}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------

index(gpio1_1)    -> 0;
index(gpio1_2)    -> 1;
index(gpio1_3)    -> 2;
index(gpio1_4)    -> 3;
index(gpio2_1)    -> 4;
index(gpio2_2)    -> 5;
index(gpio2_3)    -> 6;
index(gpio2_4)    -> 7;
index(led1_r)     -> 8;
index(led1_g)     -> 9;
index(led1_b)     -> 10;
index(led2_r)     -> 11;
index(led2_g)     -> 12;
index(led2_b)     -> 13;
index(jumper_1)   -> 14;
index(jumper_2)   -> 15;
index(jumper_3)   -> 16;
index(jumper_4)   -> 17;
index(jumper_5)   -> 18;
index(spi1_pin7)  -> 19;
index(spi1_pin8)  -> 20;
index(spi1_pin9)  -> 21;
index(spi1_pin10) -> 22;
index(ss1)        -> 23;
index(ss2)        -> 24;
index(uart_1_cts) -> 25;
index(uart_2_txd) -> 26;
index(uart_3_rxd) -> 27;
index(uart_4_rts) -> 28;
index(Any) when is_integer(Any) ->
    Any.

map_type(periph_a) -> 0;
map_type(periph_b) -> 1;
map_type(periph_c) -> 2;
map_type(periph_d) -> 3;
map_type(input)    -> 4;
map_type(output_0) -> 5; % default value 0
map_type(output_1) -> 6. % default value 1

map_attr([default]) -> 0.

bool(<<0>>) -> false;
bool(<<1>>) -> true;
bool(Val)   -> Val.
