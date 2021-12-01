% @doc
% @end
-module(grisp_eeprom).

-behaviour(gen_server).


%--- EXPORTS -------------------------------------------------------------------

% API
-export([start_link/1]).
-export([read/3]).
-export([write/3]).


% Behaviour gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).


%--- INLUDES -------------------------------------------------------------------

-include("grisp_eeprom.hrl").


%--- RECORDS -------------------------------------------------------------------

-record(state, {bus, addr, wait}).


%--- API FUNCTIONS -------------------------------------------------------------

% @private
start_link(Name)->
    gen_server:start_link({local, eeprom_proc(Name)}, ?MODULE, [
        grisp_i2c:open(eeprom_bus(Name)),
        eeprom_addr(Name),
        eeprom_wait(Name)
    ], []).

read(Name, ReadAddr, ReadSize) ->
    case gen_server:call(eeprom_proc(Name), {read, ReadAddr, ReadSize}) of
        {error, Reason} -> error(Reason);
        {ok, Data} -> Data
    end.


write(Name, WriteAddr, Data) ->
    case gen_server:call(eeprom_proc(Name), {write, WriteAddr, Data}) of
        {error, Reason} -> error(Reason);
        ok -> ok
    end.


%--- BEHAVIOUR gen_server CALLBACK FUNCTIONS -----------------------------------

% @private
init([Bus, ChipAddress, Wait]) ->
    {ok, #state{bus = Bus, addr = ChipAddress, wait = Wait}}.

% @private
handle_call({read, ReadAddr, ReadSize}, _From,
            #state{bus = Bus, addr = ChipAddr} = State) ->
    {reply, eeprom_read(Bus, ChipAddr, ReadAddr, ReadSize), State};
handle_call({write, WriteAddr, Data}, _From,
            #state{bus = Bus, addr = ChipAddr, wait = Wait} = State) ->
    {reply, eeprom_write(Bus, ChipAddr, Wait, WriteAddr, Data), State};
handle_call(Call, _From, _State) ->
    error({unknown_call, Call}).

% @private
handle_cast(Request, _State) ->
    error({unknown_cast, Request}).

% @private
handle_info(Info, _State) ->
    error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% @private
terminate(_Reason, _State) ->
    ok.


%--- INTERNAL FUNCTIONS --------------------------------------------------------

eeprom_proc(som) -> ?GRISP_EEPROM_SOM_PROC;
eeprom_proc(board) -> ?GRISP_EEPROM_BOARD_PROC.

eeprom_bus(som) -> ?GRISP_EEPROM_SOM_BUS;
eeprom_bus(board) -> ?GRISP_EEPROM_BOARD_BUS.

eeprom_addr(som) -> ?GRISP_EEPROM_SOM_ADDR;
eeprom_addr(board) -> ?GRISP_EEPROM_BOARD_ADDR.

eeprom_wait(som) -> ?GRISP_EEPROM_SOM_WAIT;
eeprom_wait(board) -> ?GRISP_EEPROM_BOARD_WAIT.

eeprom_read(Bus, ChipAddr, ReadAddr, ReadSize) ->
    Cmds = [
        {write, ChipAddr, 0, <<ReadAddr:16/big-integer>>},
        {read, ChipAddr, 1, ReadSize}
    ],
    case grisp_i2c:transfer(Bus, Cmds) of
        {error, _Reason} = Error -> Error;
        [ok, Data] -> {ok, Data}
    end.

eeprom_write(Bus, ChipAddr, Wait, WriteAddr, <<Data:32/binary, Rest/binary>>) ->
    timer:sleep(Wait),
    case eeprom_write_block(Bus, ChipAddr, WriteAddr, Data) of
        {error, _Reason} = Error -> Error;
        ok ->
            eeprom_write(Bus, ChipAddr, Wait, WriteAddr + byte_size(Data), Rest)
    end;
eeprom_write(_Bus, _ChipAddr, _Wait, _WriteAddr, <<>>) ->
    ok;
eeprom_write(Bus, ChipAddr, Wait, WriteAddr, Data) ->
    timer:sleep(Wait),
    eeprom_write_block(Bus, ChipAddr, WriteAddr, Data).


eeprom_write_block(Bus, ChipAddr, WriteAddr, Data) ->
    Cmds = [
        {write, ChipAddr, 0, <<WriteAddr:16/big-integer>>},
        {write, ChipAddr, 16#4000, Data}
    ],
    case grisp_i2c:transfer(Bus, Cmds) of
        {error, _Reason} = Error -> Error;
        [ok, ok] -> ok
    end.
