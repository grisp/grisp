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

-record(state, {bus, addr, wait, addr_size, page_size}).


%--- API FUNCTIONS -------------------------------------------------------------

-doc(false).
start_link(Name)->
    gen_server:start_link({local, eeprom_proc(Name)}, ?MODULE, [
        grisp_i2c:open(eeprom_bus(Name)),
        eeprom_addr(Name),
        eeprom_wait(Name),
        eeprom_addr_size(Name),
        eeprom_page_size(Name)
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

-doc(false).
init([Bus, ChipAddress, Wait, AddrSize, PageSize]) ->
    {ok, #state{bus = Bus, addr = ChipAddress, wait = Wait,
                addr_size = AddrSize, page_size = PageSize}}.

-doc(false).
handle_call({read, ReadAddr, ReadSize}, _From,
            #state{bus = Bus, addr = ChipAddr, addr_size = AddrSize} = State) ->
    {reply, eeprom_read(Bus, ChipAddr, AddrSize, ReadAddr, ReadSize), State};
handle_call({write, WriteAddr, Data}, _From,
            #state{bus = Bus, addr = ChipAddr, wait = Wait,
                   addr_size = AddrSize, page_size = PageSize} = State) ->
    Res = eeprom_write(Bus, ChipAddr, AddrSize, PageSize, Wait, WriteAddr, Data),
    {reply, Res, State};
handle_call(Call, _From, _State) ->
    error({unknown_call, Call}).

-doc(false).
handle_cast(Request, _State) ->
    error({unknown_cast, Request}).

-doc(false).
handle_info(Info, _State) ->
    error({unknown_info, Info}).

-doc(false).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-doc(false).
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

eeprom_addr_size(som) -> ?GRISP_EEPROM_SOM_ADDR_SIZE;
eeprom_addr_size(board) -> ?GRISP_EEPROM_BOARD_ADDR_SIZE.

eeprom_page_size(som) -> ?GRISP_EEPROM_SOM_PAGE_SIZE;
eeprom_page_size(board) -> ?GRISP_EEPROM_BOARD_PAGE_SIZE.

eeprom_address(1, Addr) when Addr =< 16#FF -> <<Addr:8>>;
eeprom_address(2, Addr) when Addr =< 16#FFFF -><<Addr:16/big-integer>>.

eeprom_read(Bus, ChipAddr, AddrSize, ReadAddr, ReadSize) ->
    Cmds = [
        {write, ChipAddr, 0, eeprom_address(AddrSize, ReadAddr)},
        {read, ChipAddr, 1, ReadSize}
    ],
    case grisp_i2c:transfer(Bus, Cmds) of
        {error, _Reason} = Error -> Error;
        [ok, Data] -> {ok, Data}
    end.

eeprom_write(Bus, ChipAddr, AddrSize, PageSize, Wait, WriteAddr, Data) ->
    case Data of
        <<>> -> ok;
        <<Page:PageSize/binary, Rest/binary>> ->
            case eeprom_write_block(Bus, ChipAddr, AddrSize, WriteAddr, Page) of
                {error, _Reason} = Error -> Error;
                ok ->
                    timer:sleep(Wait),
                    eeprom_write(Bus, ChipAddr, AddrSize, PageSize, Wait,
                                 WriteAddr + byte_size(Page), Rest)
            end;
        Page ->
            Res = eeprom_write_block(Bus, ChipAddr, AddrSize, WriteAddr, Page),
            timer:sleep(Wait),
            Res
    end.

eeprom_write_block(Bus, ChipAddr, AddrSize, WriteAddr, Data) ->
    Cmds = [
        {write, ChipAddr, 0, eeprom_address(AddrSize, WriteAddr)},
        {write, ChipAddr, 16#4000, Data}
    ],
    case grisp_i2c:transfer(Bus, Cmds) of
        {error, _Reason} = Error -> Error;
        [ok, ok] -> ok
    end.
