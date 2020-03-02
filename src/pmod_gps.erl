% @doc
% <a href="https://reference.digilentinc.com/reference/pmod/pmodgps/reference-manual">
% PmodGPS</a>
% module.
%
% The PmodGPS sends the GPS data over UART.
%
% Start the driver with
% ```
% 1> grisp:add_device(uart, pmod_gps).
% '''
% @end
-module(pmod_gps).

-behaviour(gen_server).

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

% @doc Get the GPS data.
%
% The input parameter specifies which sentence to get. For a description of
% the sentences see the
% <a href="https://reference.digilentinc.com/_media/reference/pmod/pmodgps/pmodgps_rm.pdf">
% PmodGPS Reference Manual
% </a>.
%
% === Example ===
% ```
%  2> pmod_gps:get(gga).
%  <<"$GPGGA,145832.000,5207.3597,N,01135.6957,E,1,5,2.50,61.9,M,46.7,M,,*6F\n">>
%  3> pmod_gps:get(gsa).
%  <<"$GPGSA,A,3,17,06,19,02,24,,,,,,,,2.69,2.51,0.97*0B\n">>
%  4> pmod_gps:get(gsv).
%  <<"$GPGSV,3,3,12,14,22,317,17,17,10,040,35,29,09,203,,22,02,351,*7F\n">>
%  5> pmod_gps:get(rmc).
%  <<"$GPRMC,150007.000,A,5207.3592,N,01135.6895,E,0.46,255.74,120220,,,A*64\n">>
%  6> pmod_gps:get(vtg).
%  <<"$GPVTG,297.56,T,,M,0.65,N,1.21,K,A*33\n">>
% '''
-spec get('gga' | 'gsa' | 'gsv' | 'rmc' | 'vtg') -> binary().
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
