% @doc API for the GRiSP EEPROM.
%
% Start the driver with
% ```
% 1> grisp:add_device(i2c, grisp_eeprom).
% '''
% @end
-module(grisp_eeprom).
-behaviour(gen_server).

% API
-export([start_link/2]).
-export([read/0]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

-define(DEVICE_ADR, 16#57).
-define(DEVICE_DATA_SIZE, 3 + (136 div 8)). %% skip first 3 bytes


%% From libgrisp/include/eeprom.h
%%
%% struct grisp_eeprom {
%% 	/** Version of the signature block. Always GRISP_EEPROM_SIG_VERSION. */
%% 	uint8_t sig_version;
%% 	/** Serial number of the board. */
%% 	uint32_t serial;
%% 	/** Production batch. */
%% 	uint16_t batch_nr;
%% 	/** Production year. */
%% 	uint16_t prod_year;
%% 	/** Production month. (1..12) */
%% 	uint8_t prod_month;
%% 	/** Production month. (1..31) */
%% 	uint8_t prod_day;
%% 	/** Mayor hardware version. */
%% 	uint8_t vers_major;
%% 	/** Minor hardware version. */
%% 	uint8_t vers_minor;
%% 	/** Variant of the assembled parts. */
%% 	uint8_t ass_var;
%% 	/** MAC address of the WiFi module. */
%% 	uint8_t mac_addr[6];
%% 	/** CRC for the EEPROM. */
%% 	uint16_t crc16;
%% };



%--- Records -------------------------------------------------------------------
%
-record(state, {bus}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot, _Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Slot, []).

% @doc Read GRiSP meta data from EEPROM
%
% === Example ===
% ```
% 2> grisp_eeprom:read().
% [{grisp_version, "2"},
%  {grisp_serial, 1001},
%  {grisp_pcb_version, "1.2"},
%  {grisp_pcb_variant, 1},
%  {grisp_batch, 1},
%  {grisp_prod_date, {{2021,10,1}, {0,0,0}}}]
% '''
-spec read() -> [].
read() ->
    gen_server:call(?MODULE, read).


%--- Callbacks -----------------------------------------------------------------

% @private
init(i2c = Slot) ->
    Bus = grisp_ni2c:open("/dev/i2c-1"),
    grisp_devices:register(Slot, ?MODULE),
    {ok, #state{bus = Bus}}.

% @private
handle_call(read, _From, #state{bus = Bus} = State) ->
    <<_SigVersion:8,
      _Dummy:24,    %% unused
      Serial:4/little-unit:8,
      BatchNr:2/little-unit:8,
      ProdYear:2/little-unit:8,
      ProdMonth:8,
      ProdDay:8,
      VersMajor:1/binary,   %% contains GRiSP version and PCB Major
      VersMinor:8,          %% actually the PCB minor
      Variant:8,
      _Mac:8,
      _Crc:2/little-unit:8>> = grisp_ni2c:read(Bus, ?DEVICE_ADR, 0, ?DEVICE_DATA_SIZE),
    <<GrispVersion:4, PcbMajor:4>> = VersMajor,
    MetaData = [{grisp_version, lists:flatten(io_lib:format("~p", [GrispVersion]))},
                {grisp_serial, Serial},
                {grisp_pcb_version, lists:flatten(io_lib:format("~p.~p", [PcbMajor, VersMinor]))},
                {grisp_pcb_variant, Variant},
                {grisp_batch, BatchNr},
                {grisp_prod_date, {{ProdYear,ProdMonth,ProdDay},{0,0,0}}}],
    {reply, MetaData, State}.

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).
