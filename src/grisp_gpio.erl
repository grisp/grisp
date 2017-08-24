-module(grisp_gpio).

-behavior(gen_server).

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

configure(Pin, Type) -> configure(Pin, Type, [default]).

configure(Pin, Type, Attr) ->
    Command = <<(index(Pin)):8, 1:8, (map_type(Type)):8, (map_attr(Attr)):8>>,
    gen_server:call(?MODULE, {command, Command}).

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

get(Pin) -> gen_server:call(?MODULE, {command, <<(index(Pin)):8, 2:8>>}).

clear(Pin) -> gen_server:call(?MODULE, {command, <<(index(Pin)):8, 3:8>>}).

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
index(spi1_pin9)  -> 19;
index(spi1_pin10) -> 20;
index(ss1)        -> 21;
index(ss2)        -> 22;
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
