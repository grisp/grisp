-module(pmod_gps).
-moduledoc """

[PmodGPS](https://reference.digilentinc.com/reference/pmod/pmodgps/reference-manual)
module.

The PmodGPS sends the GPS data over UART.

Start the driver with
```
1> grisp:add_device(uart, pmod_gps).
```
""".

-behaviour(gen_server).


%--- Exports -------------------------------------------------------------------

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


%--- Includes ------------------------------------------------------------------

-include("grisp_internal.hrl").


%--- Macros --------------------------------------------------------------------

-define(MAX_CONSECUTIVE_ERRORS, 10).


%--- Records -------------------------------------------------------------------

-record(state, {port, last_sentences, error_count = 0}).


%--- API -----------------------------------------------------------------------

-doc(false).
start_link(Slot, _Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Slot, []).

-doc """
Get the GPS data.

The input parameter specifies which type of sentence to get.
For a description of the sentences see the
[PmodGPS Reference Manual](https://reference.digilentinc.com/_media/reference/pmod/pmodgps/pmodgps_rm.pdf).

The sentence CRC is checked for all the sentence types, but for now only the
GGA values are parsed. If other sentences are needed, `m:grisp_nmea` needs to be
extended to support more types.

### Example
```
 2> pmod_gps:get(gga).
 {gps,gga,#{alt => 61.9,fixed => true,lat => 52.122661666666666,long => 11.594928333333334,time => 53912000}}}
 3> pmod_gps:get(gsa).
 {gps,gsa,<<"A,3,17,06,19,02,24,,,,,,,,2.69,2.51,0.97">>}
 4> pmod_gps:get(gsv).
 {gps,gsv,<<"3,3,12,14,22,317,17,17,10,040,35,29,09,203,,22,02,351">>}
 5> pmod_gps:get(rmc).
 {gps,rmc,<<"150007.000,A,5207.3592,N,01135.6895,E,0.46,255.74,120220,,,A">>}
 6> pmod_gps:get(vtg).
 {gps,vtg,<<"297.56,T,,M,0.65,N,1.21,K,A">>}
```
""".
-spec get(grisp_nmea:message_id()) ->
    {grisp_nmea:talker_id(), grisp_nmea:message_id(), map() | binary()} | undefined.
get(MessageId) ->
    call({get, MessageId}).


%--- Callbacks -----------------------------------------------------------------

-doc(false).
init(Slot = uart) ->
    Port = open_port({spawn_driver, "grisp_termios_drv"}, [binary]),
    grisp_devices:register(Slot, ?MODULE),
    Sentences = maps:from_list([{T, undefined} || T <- [
        dtm, gbq, gbs, gga, gll, glq, gnq, gns, gpq,
        grs, gsa, gst, gsv, rmc, txt, vlw, vtg, zda
    ]]),
    {ok, #state{port = Port, last_sentences = Sentences}}.

-doc(false).
handle_call(Call, _From, State) ->
    try execute_call(Call, State)
    catch throw:Reason -> {reply, {error, Reason}, State}
    end.

-doc(false).
handle_cast(Request, _State) -> error({unknown_cast, Request}).

-doc(false).
% We need to support at least one message failing parsing, because
% when starting to read randomly in the stream of sentences, the first
% one may be truncated. For now, we fail after a maximum number of
% consecutive errors.
handle_info({Port, {data, Data}},
            #state{port = Port, last_sentences = LastSentences,
                   error_count = ErrorCount} = State)
   when is_binary(Data) ->
    case {ErrorCount, grisp_nmea:parse(Data)} of
        {Count, {error, Reason}} when Count > ?MAX_CONSECUTIVE_ERRORS ->
            erlang:error({gps_nmea_parsing_error, Reason});
        {Count, {error, _Reason}} ->
            {noreply, State#state{error_count = Count + 1}};
        {_, {ok, {_TalkerId, MessageType, _Values} = Sentence}} ->
            LastSentences2 = LastSentences#{MessageType => Sentence},
            {noreply, State#state{last_sentences = LastSentences2,
                                  error_count = 0}}
    end;
handle_info(_Any, State) ->
    {noreply, State}.

-doc(false).
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-doc(false).
terminate(_Reason, _State) -> ok.


%--- Internal -----------------------------------------------------------------

call(Call) ->
    Dev = grisp_devices:default(?MODULE),
    case gen_server:call(Dev#device.pid, Call) of
        {error, Reason} -> error(Reason);
        {ok, Result} -> Result
    end.

execute_call({get, MessageId}, #state{last_sentences = Sentences} = State) ->
    case maps:find(MessageId, Sentences) of
        error ->
            {reply, {error, {unknown_sentence, MessageId}}, State};
        {ok, Sentence} ->
            {reply, {ok, Sentence}, State}
    end;
execute_call(Request, _State) ->
    error({unknown_call, Request}).
