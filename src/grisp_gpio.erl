-module(grisp_gpio).

-behavior(gen_server).

% API
-export([start_link/1]).
-export([configure/2, configure/3, configure/6, configure/7]).
-export([configure_slot/2]).
-export([start_polling/0]).
-export([stop_polling/0]).
-export([get/1]).
-export([clear/1]).
-export([set/1]).

% gen_event helper functions
-export([start_link_handlers/0]).
-export([add_handler/2]).
-export([delete_handler/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%--- Macros --------------------------------------------------------------------

-define(EVENT_MSG_CODE,    $E).
-define(CMD_START_POLLING,  1).
-define(CMD_STOP_POLLING,   2).
-define(CMD_PIN_CONFIG,    10).
-define(CMD_PIN_GET,       11).
-define(CMD_PIN_CLEAR,     12).
-define(CMD_PIN_SET,       13).

-define(EVENT_SERVER, grisp_gpio_events).

%--- Records -------------------------------------------------------------------

-record(state, {
    driver      :: {module(), reference()}
}).

%--- API -----------------------------------------------------------------------

% @private
start_link(DriverMod) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, DriverMod, []).

configure(Pin, {Type, HTrigger, LTrigger, PollInterval, DebounceCounter, Attr}) ->
    configure(Pin, Type, HTrigger, LTrigger, PollInterval, DebounceCounter, Attr);
configure(Pin, {Type, HTrigger, LTrigger, PollInterval, Attr}) ->
    configure(Pin, Type, HTrigger, LTrigger, PollInterval, Attr);
configure(Pin, {Type, Attr}) ->
    configure(Pin, Type, Attr);
configure(Pin, Type) ->
    configure(Pin, Type, [default]).

configure(Pin, Type, Attr) ->
    configure(Pin, Type, false, false, 1, 0, Attr).

configure(Pin, Type, HTrigger, LTrigger, PollInterval, Attr) ->
    configure(Pin, Type, HTrigger, LTrigger, PollInterval, 0, Attr).

configure(Pin, Type, HTrigger, LTrigger, PollInterval, DebounceCounter, Attr)
  when is_atom(Pin), is_atom(Type), is_boolean(HTrigger), is_boolean(LTrigger),
       is_integer(PollInterval), (PollInterval > 0),
       is_integer(DebounceCounter), (DebounceCounter >= 0), is_list(Attr) ->
    Command = <<
        ?CMD_PIN_CONFIG:8,
        (pin_index(Pin)):8,
        (map_type(Type)):8,
        (map_attr(Attr)):8,
        (bool_to_int(HTrigger)):8,
        (bool_to_int(LTrigger)):8,
        PollInterval:32/unsigned-little-integer,
        DebounceCounter:32/unsigned-little-integer
    >>,
    gen_server:call(?MODULE, {command, Command, undefined}).

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
configure_slot(jumper, {J1, J2, J3, J4, J5}) ->
    configure(jumper_1, J1),
    configure(jumper_2, J2),
    configure(jumper_3, J3),
    configure(jumper_4, J4),
    configure(jumper_5, J5),
    ok;
configure_slot(spi1, disable_cs) -> configure(ss1, output_1);
configure_slot(spi1, enable_cs)  -> configure(ss1, periph_c);
configure_slot(spi2, disable_cs) -> configure(ss2, output_1);
configure_slot(spi2, enable_cs)  -> configure(ss2, periph_c).

start_polling() ->
    Command = <<?CMD_START_POLLING:8>>,
    gen_server:call(?MODULE, {command, Command, undefined}).

stop_polling() ->
    Command = <<?CMD_STOP_POLLING:8>>,
    gen_server:call(?MODULE, {command, Command, undefined}).

get(Pin) ->
    Command = <<?CMD_PIN_GET:8, (pin_index(Pin)):8>>,
    gen_server:call(?MODULE, {command, Command, fun bin_to_bool/1}).

clear(Pin) ->
    Command = <<?CMD_PIN_CLEAR:8, (pin_index(Pin)):8>>,
    gen_server:call(?MODULE, {command, Command, undefined}).

set(Pin) ->
    Command = <<?CMD_PIN_SET:8, (pin_index(Pin)):8>>,
    gen_server:call(?MODULE, {command, Command, undefined}).

%--- Event Handler Functions ---------------------------------------------------

start_link_handlers() ->
    gen_event:start_link({local, ?EVENT_SERVER}).

add_handler(EventHandler, Params) ->
    gen_event:add_handler(?EVENT_SERVER, EventHandler, Params).

delete_handler(EventHandler) ->
    gen_event:delete_handler(?EVENT_SERVER, EventHandler).

%--- Callbacks -----------------------------------------------------------------

% @private
init(DriverMod) ->
    Ref = DriverMod:open(),
    {ok, #state{driver = {DriverMod, Ref}}}.

% @private
handle_call({command, Command, PostFun}, _From, State) ->
    {reply, driver_command(State, Command, PostFun), State}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info({Port, {data, <<?EVENT_MSG_CODE, PinIdx:8, ValBin:1/binary>>}},
            #state{driver = {_, Port}} = State) ->
    gen_event:notify(?EVENT_SERVER, {pin_name(PinIdx), bin_to_bool(ValBin)}),
    {noreply, State};
handle_info(Info, _State) ->
    error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------

driver_command(State, Command, undefined) ->
    {DriverMod, Ref} = State#state.driver,
    DriverMod:command(Ref, Command),
    ok;
driver_command(State, Command, PostFun) ->
    {DriverMod, Ref} = State#state.driver,
    PostFun(DriverMod:command(Ref, Command)).

pin_index(gpio1_1)     -> 0;
pin_index(gpio1_2)     -> 1;
pin_index(gpio1_3)     -> 2;
pin_index(gpio1_4)     -> 3;
pin_index(gpio2_1)     -> 4;
pin_index(gpio2_2)     -> 5;
pin_index(gpio2_3)     -> 6;
pin_index(gpio2_4)     -> 7;
pin_index(led1_r)      -> 8;
pin_index(led1_g)      -> 9;
pin_index(led1_b)      -> 10;
pin_index(led2_r)      -> 11;
pin_index(led2_g)      -> 12;
pin_index(led2_b)      -> 13;
pin_index(jumper_1)    -> 14;
pin_index(jumper_2)    -> 15;
pin_index(jumper_3)    -> 16;
pin_index(jumper_4)    -> 17;
pin_index(jumper_5)    -> 18;
pin_index({gpio1, 1})  -> 0;
pin_index({gpio1, 2})  -> 1;
pin_index({gpio1, 3})  -> 2;
pin_index({gpio1, 4})  -> 3;
pin_index({gpio2, 1})  -> 4;
pin_index({gpio2, 2})  -> 5;
pin_index({gpio2, 3})  -> 6;
pin_index({gpio2, 4})  -> 7;
pin_index({led1, r})   -> 8;
pin_index({led1, g})   -> 9;
pin_index({led1, b})   -> 10;
pin_index({led2, r})   -> 11;
pin_index({led2, g})   -> 12;
pin_index({led2, b})   -> 13;
pin_index({jumper, 1}) -> 14;
pin_index({jumper, 2}) -> 15;
pin_index({jumper, 3}) -> 16;
pin_index({jumper, 4}) -> 17;
pin_index({jumper, 5}) -> 18;
pin_index(spi1_pin9)   -> 19;
pin_index(spi1_pin10)  -> 20;
pin_index(ss1)         -> 21;
pin_index(ss2)         -> 22;
pin_index(uart_1_cts)  -> 23;
pin_index(uart_2_txd)  -> 24;
pin_index(uart_3_rxd)  -> 25;
pin_index(uart_4_rts)  -> 26;
pin_index(Any) when is_integer(Any), Any >= 0, Any =< 26 ->
    Any.

pin_name(0)  -> {gpio1, 1};
pin_name(1)  -> {gpio1, 2};
pin_name(2)  -> {gpio1, 3};
pin_name(3)  -> {gpio1, 4};
pin_name(4)  -> {gpio2, 1};
pin_name(5)  -> {gpio2, 2};
pin_name(6)  -> {gpio2, 3};
pin_name(7)  -> {gpio2, 4};
pin_name(8)  -> {led1, r};
pin_name(9)  -> {led1, g};
pin_name(10) -> {led1, b};
pin_name(11) -> {led2, r};
pin_name(12) -> {led2, g};
pin_name(13) -> {led2, b};
pin_name(14) -> {jumper, 1};
pin_name(15) -> {jumper, 2};
pin_name(16) -> {jumper, 3};
pin_name(17) -> {jumper, 4};
pin_name(18) -> {jumper, 5};
pin_name(19) -> spi1_pin9;
pin_name(20) -> spi1_pin10;
pin_name(21) -> ss1;
pin_name(22) -> ss2;
pin_name(23) -> uart_1_cts;
pin_name(24) -> uart_2_txd;
pin_name(25) -> uart_3_rxd;
pin_name(26) -> uart_4_rts.

map_type(periph_a) -> 0;
map_type(periph_b) -> 1;
map_type(periph_c) -> 2;
map_type(periph_d) -> 3;
map_type(input)    -> 4;
map_type(output_0) -> 5; % default value 0
map_type(output_1) -> 6. % default value 1

map_attr([default]) -> 0;
map_attr([pullup])  -> 1.

bin_to_bool(<<0>>) -> false;
bin_to_bool(<<1>>) -> true;
bin_to_bool(Val)   -> Val.

bool_to_int(true)  -> 1;
bool_to_int(false) -> 0.
