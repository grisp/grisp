% MAX14906 specs:
% https://www.analog.com/media/en/technical-documentation/data-sheets/MAX14906.pdf

% @doc Driver module for the PmodDIO
% 
% The PmodDIO is based on the <a href="https://www.analog.com/en/products/max14906.html">MAX14906</a>
%
% Each of the 4 channels of the the pmod are fully configurable to be a digital input (either type 1 and type 3 or type 2) or a digital output (either high-side or push-pull)
%
% You can start the driver with
% ```
% 1> grisp:add_device(spi2, pmod_dio).
% '''
%
% or using the high level module:
% ```
% 1> grisp_dio:start(#{devices => [{spi2, pmod_dio, #{chips => [1]}}]}).
% '''
%
% The pmod accepts up to 4 "chips" on the same SPI bus. They are identified by their addresses
%
% @end
-module(pmod_dio).
-behaviour(gen_server).

% API
-export([start_link/2]).
-export([chips/1]).
-export([read/2]).
-export([read/3]).
-export([read_burst/2]).
-export([read_burst/3]).
-export([write/3]).
-export([write/4]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

% TODO: Verify!
-define(SPI_MODE, #{clock => {low, leading}}).

-define(CRC5_START, 16#1F).
-define(CRC5_POLY, 16#15).

-include("grisp_internal.hrl").

-record(req, {chip, type = single, op = read, reg, value}).

%--- Types ---------------------------------------------------------------------

-type chip() :: 1..4.
-type reg() :: atom(). % TODO encode all possible register
-type result_read() :: #{reg() => map()}.
-type result_write() :: #{{'DiLvl_', 1..4} => 0 | 1,
                          {'F_', 1..4} => boolean()}.
-type response() :: #{'SHTVDD' => boolean(),
                      'AbvVDD' => boolean(),
                      'OWOffF' => boolean(),
                      'OvrCurr' => boolean(),
                      'OvldF' => boolean(),
                      'GLOBLF' => boolean(),
                      result => result_read() | result_write()}.

%--- API -----------------------------------------------------------------------

start_link(Slot, UserOpts) ->
    Opts = opts(maps:merge(#{chips => [1]}, UserOpts)),
    gen_server:start_link(?MODULE, Opts#{slot => Slot}, []).

%% @doc Get the list of the address of the configured chips
-spec chips(Slot :: grisp_spi:bus()) -> Chips :: [chip()].
chips(Slot) -> call(Slot, chips).

%% @doc Read the value of a register for a given chip
%% @equiv read(default, Chip, Reg).
%% @end
-spec read(Chip :: chip(), Reg :: reg()) -> response().
read(Chip, Reg) -> read(default, Chip, Reg).

%% @doc Read the value of a register for a given chip at a given slot
%%
%% === Example ===
%% To read the Interrupt register (0x03) of chip with address 0x01 on spi2:
%% ```
%% 1> pmod_dio:read(spi2, 2, 'Interrupt').
%% #{'SHTVDD' => false,
%%   'AbvVDD' => false,
%%   'OWOffF' => false,
%%   'OvrCurr' => false,
%%   'OvldF' => false,
%%   'GLOBLF' => false,
%%   result => #{
%%      'ComErr' => false,
%%      'SupplyErr' => false,
%%      'DeMagFault' => false,
%%      'ShtVDDFault' => false,
%%      'AboveVDDFault' => false,
%%      'OWOffFault' => false,
%%      'CurrLim' => false,
%%      'OverLdFault' => false}
%%   }.
%% '''
%%
%% @end
-spec read(Slot, Chip, Reg) -> response() when
      Slot :: default | grisp_spi:bus(),
      Chip :: chip(),
      Reg  :: reg().
read(Slot, Chip, Reg) ->
    call(Slot, #req{chip = Chip, reg = Reg, value = <<0>>}).

%% @doc Performs a read_burst on the given chip of the default slot
%% The call will crash if the register is something else than 'DoiLevel'
%% @equiv read_burst(default, Chip, Reg).
%% @end
-spec read_burst(Chip :: chip(), Reg :: reg()) -> response() | no_return().
read_burst(Chip, Reg) -> read_burst(default, Chip, Reg).

%% @doc Performs a read_burst on the given chip of the given slot
%% The call will crash if the register is something else than 'DoiLevel'
%% @end
-spec read_burst(Slot, Chip, Reg) -> response() | no_return() when
      Slot :: default | grisp_spi:bus(),
      Chip :: chip(),
      Reg  :: reg().
read_burst(Slot, Chip, 'DoiLevel' = Reg) ->
    Value = <<0, 0, 0, 0, 0, 0>>,
    call(Slot, #req{chip = Chip, type = burst, reg = Reg, value = Value});
read_burst(_Slot, _Chip, Reg) ->
    error({invalid_burst_register, Reg}).

%% @doc Write on the given register of the given chip on the default slot
%% @equiv write(default, Chip, Reg, Value)
%% @end
-spec write(Chip :: chip(), Reg :: reg(), Value :: map()) -> response().
write(Chip, Reg, Value) -> write(default, Chip, Reg, Value).

%% @doc Write on the griven register of the given chip of the given slot
%%
%% === Examples ===
%% To set the 1st channel of the chip with address 0x00 in input mode :
%% ```
%% 1> pmod_dio:write(spi2, 1, 'SetOUT', #{{'SetDi_', 1} => input}).
%% '''
%%
%% @end
-spec write(Slot, Chip, Reg, Value) -> response() when
      Slot  :: default | grisp_spi:bus(),
      Chip  :: chip(),
      Reg   :: reg(),
      Value :: map().
write(Slot, Chip, Reg, Value) ->
    call(Slot, #req{chip = Chip, op = write, reg = Reg, value = Value}).

%--- Callbacks -----------------------------------------------------------------

init(#{slot := Slot} = Opts) ->
    grisp_devices:register(Slot, ?MODULE),
    Bus = grisp_spi:open(Slot),
    {ok, Opts#{bus => Bus}}.

handle_call(chips, _From, #{chips := Chips} = State) ->
    {reply, maps:keys(Chips), State};
handle_call(Req, _From, #{chips := Chips} = State) when
    is_map_key(Req#req.chip, Chips)
->
    {reply, send_request(State, Req), State};
handle_call(#req{} = Req, _From, State) ->
    {reply, {error, {unknown_chip, Req#req.chip}}, State};
handle_call(Req, From, _State) ->
    error({unknown_call, Req, From}).

handle_cast(Req, _State) -> error({unknown_cast, Req}).

%--- Internal ------------------------------------------------------------------

opts(#{chips := Chips} = Opts) ->
    Opts#{chips := maps:from_list([{C, true} || C <- Chips])}.

call(default, Call) ->
    Dev = grisp_devices:default(?MODULE),
    gen_server:call(Dev#device.pid, Call);
call(Slot, Call) ->
    Dev = grisp_devices:instance(Slot),
    gen_server:call(Dev#device.pid, Call).

send_request(#{bus := Bus}, #req{op = Op, reg = Reg} = Req) ->
    Addr = Req#req.chip - 1,
    Encoded = encode_request(Req),
    Request = request(Req, Addr, Encoded),
    [Response] = grisp_spi:transfer(Bus, [{?SPI_MODE, Request}]),
    decode_response(Op, Addr, Reg, bit_size(Encoded), Response).

request(#req{type = Type, reg = Reg, op = Op}, Addr, Value) ->
    Req =
        <<Addr:2, (type(Type)):1, (reg(Reg)):4, (rw(Op)):1, Value/binary,
            2#000:3>>,
    CRC = crc5(Req),
    <<Req/bitstring, CRC:5>>.

type(single) -> 0;
type(burst) -> 1.

rw(read) -> 0;
rw(write) -> 1.

crc5(Bitstring) -> crc5(Bitstring, ?CRC5_START).

crc5(<<Bit:1, Bin/bitstring>>, R) ->
    R2 =
        case (Bit band 16#01) bxor ((R band 16#10) bsr 4) of
            Val when Val > 0 -> ?CRC5_POLY bxor (R bsl 1) band 16#1f;
            _Else -> (R bsl 1) band 16#1f
        end,
    crc5(Bin, R2);
crc5(<<>>, R) ->
    R.

encode_request(#req{value = Value}) when is_binary(Value) -> Value;
encode_request(#req{reg = Reg, value = Value}) -> reg(encode, Reg, Value).

-define(MAP_FIELD(Name),
    list_to_atom(??Name) => field(decode, list_to_atom(??Name), Name)
).
-define(MAP_FIELD(Variable, Key), Key => field(decode, Key, Variable)).
-define(BIN_FIELD(Name), (field(encode, list_to_atom(??Name), Name))).
-define(BIN_FIELD(Variable, Key), (field(encode, Key, Variable))).

decode_response(Op, Addr, Reg, Len, <<_:2, Response/bitstring>>) ->
    0 = crc5(Response),
    <<
        SHTVDD:1,
        AbvVDD:1,
        OWOffF:1,
        OvrCurr:1,
        OvlDf:1,
        GLOBLF:1,
        Result:Len/bitstring,
        Addr:2,
        ThrErr:1,
        _CRC:5
    >> = Response,
    #{
        ?MAP_FIELD(SHTVDD),
        ?MAP_FIELD(SHTVDD),
        ?MAP_FIELD(AbvVDD),
        ?MAP_FIELD(OWOffF),
        ?MAP_FIELD(OvrCurr),
        ?MAP_FIELD(OvlDf),
        ?MAP_FIELD(GLOBLF),
        ?MAP_FIELD(ThrErr),
        result => decode_result(Op, Reg, Result)
    }.

decode_result(read, Reg, Result) ->
    decode_registers(Reg, Result);
decode_result(write, _Reg, Result) ->
    <<
        DiLvl4:1,
        DiLvl3:1,
        DiLvl2:1,
        DiLvl1:1,
        F4:1,
        F3:1,
        F2:1,
        F1:1
    >> = Result,
    #{
        ?MAP_FIELD(DiLvl4, {'DiLvl_', 4}),
        ?MAP_FIELD(DiLvl3, {'DiLvl_', 3}),
        ?MAP_FIELD(DiLvl2, {'DiLvl_', 2}),
        ?MAP_FIELD(DiLvl1, {'DiLvl_', 1}),
        ?MAP_FIELD(F4, {'F_', 4}),
        ?MAP_FIELD(F3, {'F_', 3}),
        ?MAP_FIELD(F2, {'F_', 2}),
        ?MAP_FIELD(F1, {'F_', 1})
    }.

decode_registers(StartReg, Response) ->
    decode_registers(reg(StartReg), Response, #{}).

decode_registers(Reg, <<Resp:8/bitstring, Response/binary>>, Result) ->
    Name = reg(Reg),
    decode_registers(Reg + 1, Response, Result#{
        Name => reg(decode, Name, Resp)
    });
decode_registers(_Reg, <<>>, Result) ->
    Result.

reg('SetOUT') -> 16#00;
reg('SetLED') -> 16#01;
reg('DoiLevel') -> 16#02;
reg('Interrupt') -> 16#03;
reg('OvrLdChF') -> 16#04;
reg('OpnWirChF') -> 16#05;
reg('ShtVDDChF') -> 16#06;
reg('GlobalErr') -> 16#07;
reg('OpnWrEn') -> 16#08;
reg('ShtVDDEn') -> 16#09;
reg('Config1') -> 16#0A;
reg('Config2') -> 16#0B;
reg('ConfigDI') -> 16#0C;
reg('ConfigDO') -> 16#0D;
reg('CurrLim') -> 16#0E;
reg('Mask') -> 16#0F;
reg(16#00) -> 'SetOUT';
reg(16#01) -> 'SetLED';
reg(16#02) -> 'DoiLevel';
reg(16#03) -> 'Interrupt';
reg(16#04) -> 'OvrLdChF';
reg(16#05) -> 'OpnWirChF';
reg(16#06) -> 'ShtVDDChF';
reg(16#07) -> 'GlobalErr';
reg(16#08) -> 'OpnWrEn';
reg(16#09) -> 'ShtVDDEn';
reg(16#0A) -> 'Config1';
reg(16#0B) -> 'Config2';
reg(16#0C) -> 'ConfigDI';
reg(16#0D) -> 'ConfigDO';
reg(16#0E) -> 'CurrLim';
reg(16#0F) -> 'Mask';
reg(Reg) -> error({invalid_register, Reg}).

reg(decode, 'SetOUT', Content) when is_binary(Content) ->
    <<
        SetDi4:1,
        SetDi3:1,
        SetDi2:1,
        SetDi1:1,
        HighO4:1,
        HighO3:1,
        HighO2:1,
        HighO1:1
    >> = Content,
    #{
        ?MAP_FIELD(SetDi4, {'SetDi_', 4}),
        ?MAP_FIELD(SetDi3, {'SetDi_', 3}),
        ?MAP_FIELD(SetDi2, {'SetDi_', 2}),
        ?MAP_FIELD(SetDi1, {'SetDi_', 1}),
        ?MAP_FIELD(HighO4, {'HighO_', 4}),
        ?MAP_FIELD(HighO3, {'HighO_', 3}),
        ?MAP_FIELD(HighO2, {'HighO_', 2}),
        ?MAP_FIELD(HighO1, {'HighO_', 1})
    };
reg(encode, 'SetOUT', #{
    {'SetDi_', 4} := SetDi4,
    {'SetDi_', 3} := SetDi3,
    {'SetDi_', 2} := SetDi2,
    {'SetDi_', 1} := SetDi1,
    {'HighO_', 4} := HighO4,
    {'HighO_', 3} := HighO3,
    {'HighO_', 2} := HighO2,
    {'HighO_', 1} := HighO1
}) ->
    <<
        ?BIN_FIELD(SetDi4, {'SetDi_', 4}):1,
        ?BIN_FIELD(SetDi3, {'SetDi_', 3}):1,
        ?BIN_FIELD(SetDi2, {'SetDi_', 2}):1,
        ?BIN_FIELD(SetDi1, {'SetDi_', 1}):1,
        ?BIN_FIELD(HighO4, {'HighO_', 4}):1,
        ?BIN_FIELD(HighO3, {'HighO_', 3}):1,
        ?BIN_FIELD(HighO2, {'HighO_', 2}):1,
        ?BIN_FIELD(HighO1, {'HighO_', 1}):1
    >>;
reg(decode, 'SetLED', Content) when is_binary(Content) ->
    <<
        SLED4:1,
        SLED3:1,
        SLED2:1,
        SLED1:1,
        FLED4:1,
        FLED3:1,
        FLED2:1,
        FLED1:1
    >> = Content,
    #{
        ?MAP_FIELD(SLED4, {'SLED_', 4}),
        ?MAP_FIELD(SLED3, {'SLED_', 3}),
        ?MAP_FIELD(SLED2, {'SLED_', 2}),
        ?MAP_FIELD(SLED1, {'SLED_', 1}),
        ?MAP_FIELD(FLED4, {'FLED_', 4}),
        ?MAP_FIELD(FLED3, {'FLED_', 3}),
        ?MAP_FIELD(FLED2, {'FLED_', 2}),
        ?MAP_FIELD(FLED1, {'FLED_', 1})
    };
reg(encode, 'SetLED', #{
    {'SLED_', 4} := SLED4,
    {'SLED_', 3} := SLED3,
    {'SLED_', 2} := SLED2,
    {'SLED_', 1} := SLED1,
    {'FLED_', 4} := FLED4,
    {'FLED_', 3} := FLED3,
    {'FLED_', 2} := FLED2,
    {'FLED_', 1} := FLED1
}) ->
    <<
        ?BIN_FIELD(SLED4, {'SLED_', 4}):1,
        ?BIN_FIELD(SLED3, {'SLED_', 3}):1,
        ?BIN_FIELD(SLED2, {'SLED_', 2}):1,
        ?BIN_FIELD(SLED1, {'SLED_', 1}):1,
        ?BIN_FIELD(FLED4, {'FLED_', 4}):1,
        ?BIN_FIELD(FLED3, {'FLED_', 3}):1,
        ?BIN_FIELD(FLED2, {'FLED_', 2}):1,
        ?BIN_FIELD(FLED1, {'FLED_', 1}):1
    >>;
reg(decode, 'DoiLevel', Content) when is_binary(Content) ->
    <<
        SafeDemagF4:1,
        SafeDemagF3:1,
        SafeDemagF2:1,
        SafeDemagF1:1,
        DoiLevel4_VDDOKFault:1,
        DoiLevel3_VDDOKFault:1,
        DoiLevel2_VDDOKFault:1,
        DoiLevel1_VDDOKFault:1
    >> = Content,
    #{
        ?MAP_FIELD(SafeDemagF4, {'SafeDemagF_', 4}),
        ?MAP_FIELD(SafeDemagF3, {'SafeDemagF_', 3}),
        ?MAP_FIELD(SafeDemagF2, {'SafeDemagF_', 2}),
        ?MAP_FIELD(SafeDemagF1, {'SafeDemagF_', 1}),
        ?MAP_FIELD(DoiLevel4_VDDOKFault, {'DoiLevel_/VDDOKFault_', 4}),
        ?MAP_FIELD(DoiLevel3_VDDOKFault, {'DoiLevel_/VDDOKFault_', 3}),
        ?MAP_FIELD(DoiLevel2_VDDOKFault, {'DoiLevel_/VDDOKFault_', 2}),
        ?MAP_FIELD(DoiLevel1_VDDOKFault, {'DoiLevel_/VDDOKFault_', 1})
    };
reg(decode, 'Interrupt', Content) ->
    <<ComErr:1, SupplyErr:1, DeMagFault:1, ShtVDDFault:1, AboveVDDFault:1,
        OWOffFault:1, CurrLim:1, OverLdFault:1>> = Content,
    #{
        ?MAP_FIELD(ComErr),
        ?MAP_FIELD(SupplyErr),
        ?MAP_FIELD(DeMagFault),
        ?MAP_FIELD(ShtVDDFault),
        ?MAP_FIELD(AboveVDDFault),
        ?MAP_FIELD(OWOffFault),
        ?MAP_FIELD(CurrLim),
        ?MAP_FIELD(OverLdFault)
    };
reg(decode, 'OvrLdChF', Content) ->
    <<
        CL4:1,
        CL3:1,
        CL2:1,
        CL1:1,
        OVL4:1,
        OVL3:1,
        OVL2:1,
        OVL1:1
    >> = Content,
    #{
        ?MAP_FIELD(CL4, {'CL_', 4}),
        ?MAP_FIELD(CL3, {'CL_', 3}),
        ?MAP_FIELD(CL2, {'CL_', 2}),
        ?MAP_FIELD(CL1, {'CL_', 1}),
        ?MAP_FIELD(OVL4, {'OVL_', 4}),
        ?MAP_FIELD(OVL3, {'OVL_', 3}),
        ?MAP_FIELD(OVL2, {'OVL_', 2}),
        ?MAP_FIELD(OVL1, {'OVL_', 1})
    };
reg(decode, 'OpnWirChF', Content) ->
    <<
        AboveVDD4:1,
        AboveVDD3:1,
        AboveVDD2:1,
        AboveVDD1:1,
        OWOff4:1,
        OWOff3:1,
        OWOff2:1,
        OWOff1:1
    >> = Content,
    #{
        ?MAP_FIELD(AboveVDD4, {'AboveVDD_', 4}),
        ?MAP_FIELD(AboveVDD3, {'AboveVDD_', 3}),
        ?MAP_FIELD(AboveVDD2, {'AboveVDD_', 2}),
        ?MAP_FIELD(AboveVDD1, {'AboveVDD_', 1}),
        ?MAP_FIELD(OWOff4, {'OWOff_', 4}),
        ?MAP_FIELD(OWOff3, {'OWOff_', 3}),
        ?MAP_FIELD(OWOff2, {'OWOff_', 2}),
        ?MAP_FIELD(OWOff1, {'OWOff_', 1})
    };
reg(decode, 'ShtVDDChF', Content) ->
    <<
        VDDOV4:1,
        VDDOV3:1,
        VDDOV2:1,
        VDDOV1:1,
        SHVDD4:1,
        SHVDD3:1,
        SHVDD2:1,
        SHVDD1:1
    >> = Content,
    #{
        ?MAP_FIELD(VDDOV4, {'VDDOV_', 4}),
        ?MAP_FIELD(VDDOV3, {'VDDOV_', 3}),
        ?MAP_FIELD(VDDOV2, {'VDDOV_', 2}),
        ?MAP_FIELD(VDDOV1, {'VDDOV_', 1}),
        ?MAP_FIELD(SHVDD4, {'SHVDD_', 4}),
        ?MAP_FIELD(SHVDD3, {'SHVDD_', 3}),
        ?MAP_FIELD(SHVDD2, {'SHVDD_', 2}),
        ?MAP_FIELD(SHVDD1, {'SHVDD_', 1})
    };
reg(decode, 'GlobalErr', Content) ->
    <<
        WDogErr:1,
        LossGND:1,
        ThrmShutd:1,
        VDD_UVLO:1,
        VDD_Warn:1,
        VDD_Low:1,
        V5_UVLO:1,
        VINT_UV:1
    >> = Content,
    #{
        ?MAP_FIELD(WDogErr),
        ?MAP_FIELD(LossGND),
        ?MAP_FIELD(ThrmShutd),
        ?MAP_FIELD(VDD_UVLO),
        ?MAP_FIELD(VDD_Warn),
        ?MAP_FIELD(VDD_Low),
        ?MAP_FIELD(V5_UVLO),
        ?MAP_FIELD(VINT_UV)
    };
reg(decode, 'Config1', Content) when is_binary(Content) ->
    <<
        LedCurrLim:1,
        FLatchEn:1,
        FilterLong:1,
        FFilterEn:1,
        FLEDStretch:2,
        SLEDSet:1,
        FLEDSet:1
    >> = Content,
    #{
        ?MAP_FIELD(LedCurrLim),
        ?MAP_FIELD(FLatchEn),
        ?MAP_FIELD(FilterLong),
        ?MAP_FIELD(FFilterEn),
        ?MAP_FIELD(FLEDStretch),
        ?MAP_FIELD(SLEDSet),
        ?MAP_FIELD(FLEDSet)
    };
reg(encode, 'Config1', #{
    'LedCurrLim' := LedCurrLim,
    'FLatchEn' := FLatchEn,
    'FilterLong' := FilterLong,
    'FFilterEn' := FFilterEn,
    'FLEDStretch' := FLEDStretch,
    'SLEDSet' := SLEDSet,
    'FLEDSet' := FLEDSet
}) ->
    <<
        ?BIN_FIELD(LedCurrLim):1,
        ?BIN_FIELD(FLatchEn):1,
        ?BIN_FIELD(FilterLong):1,
        ?BIN_FIELD(FFilterEn):1,
        ?BIN_FIELD(FLEDStretch):2,
        ?BIN_FIELD(SLEDSet):1,
        ?BIN_FIELD(FLEDSet):1
    >>;
reg(decode, 'Config2', Content) when is_binary(Content) ->
    <<
        WDTo:2,
        OWOffCs:2,
        ShtVddThr:2,
        SynchWDEn:1,
        VDDOnThr:1
    >> = Content,
    #{
        ?MAP_FIELD(WDTo),
        ?MAP_FIELD(OWOffCs),
        ?MAP_FIELD(ShtVddThr),
        ?MAP_FIELD(SynchWDEn),
        ?MAP_FIELD(VDDOnThr)
    };
reg(encode, 'Config2', #{
    'WDTo' := WDTo,
    'OWOffCs' := OWOffCs,
    'ShtVddThr' := ShtVddThr,
    'SynchWDEn' := SynchWDEn,
    'VDDOnThr' := VDDOnThr
}) ->
    <<
        ?BIN_FIELD(WDTo):2,
        ?BIN_FIELD(OWOffCs):2,
        ?BIN_FIELD(ShtVddThr):2,
        ?BIN_FIELD(SynchWDEn):1,
        ?BIN_FIELD(VDDOnThr):1
    >>;
reg(decode, 'ConfigDI', Content) when is_binary(Content) ->
    <<
        Typ2Di:1,
        % Reserved
        0:1,
        VDDFaultDis:1,
        VDDFaultSel:1,
        AboveVDDProtEn:1,
        OVLStretchEn:1,
        OVLBlank:2
    >> = Content,
    #{
        ?MAP_FIELD(Typ2Di),
        ?MAP_FIELD(VDDFaultDis),
        ?MAP_FIELD(VDDFaultSel),
        ?MAP_FIELD(AboveVDDProtEn),
        ?MAP_FIELD(OVLStretchEn),
        ?MAP_FIELD(OVLBlank)
    };
reg(encode, 'ConfigDI', #{
    'Typ2Di' := Typ2Di,
    'VDDFaultDis' := VDDFaultDis,
    'VDDFaultSel' := VDDFaultSel,
    'AboveVDDProtEn' := AboveVDDProtEn,
    'OVLStretchEn' := OVLStretchEn,
    'OVLBlank' := OVLBlank
}) ->
    <<
        ?BIN_FIELD(Typ2Di):1,
        % Reserved
        0:1,
        ?BIN_FIELD(VDDFaultDis):1,
        ?BIN_FIELD(VDDFaultSel):1,
        ?BIN_FIELD(AboveVDDProtEn):1,
        ?BIN_FIELD(OVLStretchEn):1,
        ?BIN_FIELD(OVLBlank):2
    >>;
reg(decode, 'ConfigDO', Content) when is_binary(Content) ->
    <<
        DoMode4:2,
        DoMode3:2,
        DoMode2:2,
        DoMode1:2
    >> = Content,
    #{
        ?MAP_FIELD(DoMode4, {'DoMode_', 4}),
        ?MAP_FIELD(DoMode3, {'DoMode_', 3}),
        ?MAP_FIELD(DoMode2, {'DoMode_', 2}),
        ?MAP_FIELD(DoMode1, {'DoMode_', 1})
    };
reg(encode, 'ConfigDO', #{
    {'DoMode_', 4} := DoMode4,
    {'DoMode_', 3} := DoMode3,
    {'DoMode_', 2} := DoMode2,
    {'DoMode_', 1} := DoMode1
}) ->
    <<
        ?BIN_FIELD(DoMode4, {'DoMode_', 4}):2,
        ?BIN_FIELD(DoMode3, {'DoMode_', 3}):2,
        ?BIN_FIELD(DoMode2, {'DoMode_', 2}):2,
        ?BIN_FIELD(DoMode1, {'DoMode_', 1}):2
    >>;
reg(decode, 'CurrLim', Content) when is_binary(Content) ->
    <<
        CL4:2,
        CL3:2,
        CL2:2,
        CL1:2
    >> = Content,
    #{
        ?MAP_FIELD(CL4, {'CL_', 4}),
        ?MAP_FIELD(CL3, {'CL_', 3}),
        ?MAP_FIELD(CL2, {'CL_', 2}),
        ?MAP_FIELD(CL1, {'CL_', 1})
    };
reg(encode, 'CurrLim', #{
    {'CL_', 4} := CL4,
    {'CL_', 3} := CL3,
    {'CL_', 2} := CL2,
    {'CL_', 1} := CL1
}) ->
    <<
        ?BIN_FIELD(CL4, {'CL_', 4}):2,
        ?BIN_FIELD(CL3, {'CL_', 3}):2,
        ?BIN_FIELD(CL2, {'CL_', 2}):2,
        ?BIN_FIELD(CL1, {'CL_', 1}):2
    >>;
reg(decode, 'Mask', Content) when is_binary(Content) ->
    <<
        CommErrM:1,
        SupplyErrM:1,
        VddOKM:1,
        ShtVddM:1,
        AboveVDDM:1,
        OWOffM:1,
        CurrLimM:1,
        OverLdM:1
    >> = Content,
    #{
        ?MAP_FIELD(CommErrM),
        ?MAP_FIELD(SupplyErrM),
        ?MAP_FIELD(VddOKM),
        ?MAP_FIELD(ShtVddM),
        ?MAP_FIELD(AboveVDDM),
        ?MAP_FIELD(OWOffM),
        ?MAP_FIELD(CurrLimM),
        ?MAP_FIELD(OverLdM)
    };
reg(encode, 'Mask', #{
    'CommErrM' := CommErrM,
    'SupplyErrM' := SupplyErrM,
    'VddOKM' := VddOKM,
    'ShtVddM' := ShtVddM,
    'AboveVDDM' := AboveVDDM,
    'OWOffM' := OWOffM,
    'CurrLimM' := CurrLimM,
    'OverLdM' := OverLdM
}) ->
    <<
        ?BIN_FIELD(CommErrM):1,
        ?BIN_FIELD(SupplyErrM):1,
        ?BIN_FIELD(VddOKM):1,
        ?BIN_FIELD(ShtVddM):1,
        ?BIN_FIELD(AboveVDDM):1,
        ?BIN_FIELD(OWOffM):1,
        ?BIN_FIELD(CurrLimM):1,
        ?BIN_FIELD(OverLdM):1
    >>;
reg(decode, _Reg, Content) ->
    Content;
reg(encode, Reg, Content) ->
    error({invalid_content, Reg, Content}).

%% erlfmt-ignore
-define(field_enc_dec(Field, Enc, Dec),
    field(decode, Field, Enc) -> Dec;
    field(encode, Field, Dec) -> Enc
).
-define(field(Field, Enc0, Dec0, Enc1, Dec1),
    ?field_enc_dec(Field, Enc0, Dec0);
    ?field_enc_dec(Field, Enc1, Dec1)
).
-define(field(Field, Enc0, Dec0, Enc1, Dec1, Enc2, Dec2, Enc3, Dec3),
    ?field_enc_dec(Field, Enc0, Dec0);
    ?field_enc_dec(Field, Enc1, Dec1);
    ?field_enc_dec(Field, Enc2, Dec2);
    ?field_enc_dec(Field, Enc3, Dec3)
).

%% erlfmt-ignore
?field({'DiLvl_', _}, 0, 0, 1, 1);
?field({'DoiLevel_/VDDOKFault_', _}, 0, 0, 1, 1);
?field({'SetDi_', _}, 0, output, 1, input);
?field({'HighO_', _}, 0, open, 1, closed);
?field({'FLED_', _}, 0, off, 1, on);
?field({'SLED_', _}, 0, off, 1, on);
?field('FLEDStretch',
    0, false,
    1, {millisecond, 1_000},
    2, {millisecond, 2_000},
    3, {millisecond, 3_000}
);
?field('WDTo',
    0, false,
    1, {millisecond, 200},
    2, {millisecond, 600},
    3, {millisecond, 1_200}
);
?field('OWOffCs',
    0, {microampere, 60},
    1, {microampere, 100},
    2, {microampere, 300},
    3, {microampere, 600}
);
?field('ShtVDDThr',
    0, {volt, 9},
    1, {volt, 10},
    2, {volt, 12},
    3, {volt, 14}
);
?field('Typ2Di', 0, type_1_3, 1, type_2);
?field('OVLBlack',
    0, false,
    1, {millisecond, 8},
    2, {millisecond, 50},
    3, {millisecond, 300}
);
?field({'DoMode_', _},
    16#00, {high_side, {inrush_multiplier, 1}},
    16#01, {high_side, {inrush_multiplier, 2}},
    16#10, {push_pull, active_clamp},
    16#11, {push_pull, simple}
);
field(decode, {'CL_', _}, 16#00) ->
    #{current_limit => {milliampere, 600}, inrush => {millisecond, 20}};
field(decode, {'CL_', _}, 16#01) ->
    #{current_limit => {milliampere, 130}, inrush => {millisecond, 50}};
field(decode, {'CL_', _}, 16#10) ->
    #{current_limit => {milliampere, 300}, inrush => {millisecond, 40}};
field(decode, {'CL_', _}, 16#11) ->
    #{current_limit => {milliampere, 1200}, inrush => {millisecond, 10}};
field(encode, {'CL_', _},
    #{current_limit := {milliampere, 600}, inrush := {millisecond, 20}}
) ->
    16#00;
field(encode, {'CL_', _},
    #{current_limit := {milliampere, 130}, inrush := {millisecond, 50}}
) ->
    16#01;
field(encode, {'CL_', _},
    #{current_limit := {milliampere, 300}, inrush := {millisecond, 40}}
) ->
    16#10;
field(encode, {'CL_', _},
    #{current_limit := {milliampere, 1200}, inrush := {millisecond, 10}}
) ->
    16#11;
?field(_Name, 0, false, 1, true);
field(encode, Name, Value) ->
    error({invalid_field_value, Name, Value}).
