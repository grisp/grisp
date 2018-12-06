-module(pmod_gps).

-behavior(gen_server).

% API
-export([start_link/2]).
-export([get/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("grisp.hrl").

%--- Records -------------------------------------------------------------------

-record(state, {port, last_gga, last_gsa, last_gsv, last_rmc, last_vtg}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot, _Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Slot, []).

get(Sentence) ->
    call({get, Sentence}).

%--- Callbacks -----------------------------------------------------------------

% @private
init(Slot = uart) ->
    Port = open_port({spawn_driver, "grisp_termios_drv"}, [binary]),
    grisp_devices:register(Slot, ?MODULE),
    {ok, #state{port = Port}}.

% @private
handle_call(Call, _From, State) ->
    try execute_call(Call, State)
    catch throw:Reason -> {reply, {error, Reason}, State}
    end.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    case Data of
        % "$GPGGA...\n"
        <<$$,$G,$P,$G,$G,$A,_/binary>> ->
            {noreply, State#state{last_gga = Data}};
        % "$GPGSA...\n"
        <<$$,$G,$P,$G,$S,$A,_/binary>> ->
            {noreply, State#state{last_gsa = Data}};
        % "$GPGSV...\n"
        <<$$,$G,$P,$G,$S,$V,_/binary>> ->
            {noreply, State#state{last_gsv = Data}};
        % "$GPRMC...\n"
        <<$$,$G,$P,$R,$M,$C,_/binary>> ->
            {noreply, State#state{last_rmc = Data}};
        % "$GPVTG...\n"
        <<$$,$G,$P,$V,$T,$G,_/binary>> ->
            {noreply, State#state{last_vtg = Data}};
        <<$\n>> ->
            {noreply, State};
        _ ->
            {noreply, State}
    end.



% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

%--- Internal -----------------------------------------------------------------

call(Call) ->
    Dev = grisp_devices:default(?MODULE),
    case gen_server:call(Dev#device.pid, Call) of
        {error, Reason} -> error(Reason);
        Result          -> Result
    end.

execute_call({get, gga}, #state{last_gga = Gga} = State) ->
    {reply, Gga, State};
execute_call({get, gsa}, #state{last_gsa = Gsa} = State) ->
    {reply, Gsa, State};
execute_call({get, gsv}, #state{last_gsv = Gsv} = State) ->
    {reply, Gsv, State};
execute_call({get, rmc}, #state{last_rmc = Rmc} = State) ->
    {reply, Rmc, State};
execute_call({get, vtg}, #state{last_vtg = Vtc} = State) ->
    {reply, Vtc, State};
execute_call({get, Sentence}, State) ->
    error({unknown_sentence, Sentence}, State);
execute_call(Request, State) ->
    error({unknown_call, Request}, State).
