-module(pmod_dio).
-behaviour(gen_server).

-include_lib("stdlib/include/assert.hrl").

% FIXME: remove!
-export([crc5/1]).
-export([read/2]).
-export([read_burst/2]).
-export([write/3]).

% API
-export([start_link/2]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

% TODO: Verify!
-define(SPI_MODE, #{cpol => low, cpha => leading}).

-define(CRC5_START, 16#1F).
-define(CRC5_POLY, 16#15).

%--- API -----------------------------------------------------------------------

start_link(Connector, _Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Connector, []).

read(Chip, Reg) ->
    Value = <<0>>,
    send_request(Chip, single, read, Reg, Value).

read_burst(Chip, 'DoiLevel') ->
    Value = <<0, 0, 0, 0, 0, 0>>,
    send_request(Chip, burst, read, 'DoiLevel', Value);
read_burst(_Chip, Reg) ->
    error({invalid_burst_register, Reg}).

write(Chip, Reg, Value) ->
    send_request(Chip, single, write, Reg, Value).

%--- Callbacks -----------------------------------------------------------------

init(Slot) ->
    grisp_devices:register(Slot, ?MODULE),
    {ok, #{}}.

handle_call(Request, From, _State) -> error({unknown_call, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

%--- Internal ------------------------------------------------------------------

send_request(Chip, Type, Op, Reg, Value) ->
    Addr = Chip - 1,
    Request = request(Type, Op, Addr, Reg, Value),
    Response = grisp_spi:send_recv(spi2, ?SPI_MODE, Request),
    decode_response(Addr, Reg, bit_size(Value), Response).

request(Type, Op, Addr, Reg, Value) ->
    Req =
        <<Addr:2, (type(Type)):1, (reg(Reg)):4, (rw(Op)):1, Value/binary,
            2#000:3>>,
    CRC = crc5(Req),
    <<Req/bitstring, CRC:5>>.

type(single) -> 0;
type(burst) -> 1.

rw(read) -> 0;
rw(write) -> 1.

crc5(Bitstring) ->
    crc5(Bitstring, ?CRC5_START).

crc5(<<Bit:1, Bin/bitstring>>, R) ->
    R2 =
        case (Bit band 16#01) bxor ((R band 16#10) bsr 4) of
            Val when Val > 0 -> ?CRC5_POLY bxor (R bsl 1) band 16#1f;
            _Else -> (R bsl 1) band 16#1f
        end,
    crc5(Bin, R2);
crc5(<<>>, R) ->
    R.

decode_response(Addr, Reg, Len, Response) ->
    <<_:2, Data:(Len + 9)/bitstring, CRC:5>> = Response,
    ?assertEqual(crc5(Data), CRC),
    <<
        SHTVDD:1,
        AbvVDD:1,
        OWOffF:1,
        OvrCurr:1,
        OvlDf:1,
        GLOBLF:1,
        RegResp:Len/bitstring,
        Addr:2,
        ThrErr:1
    >> = Data,
    #{
        % faults => #{
        %     short_to_vdd => boolean(SHTVDD),
        %     above_vdd => boolean(AbvVDD),
        %     open_wire => boolean(OWOffF),
        %     current_limit => boolean(OvrCurr),
        %     overload => boolean(OvlDf),
        %     global => boolean(GLOBLF)
        % },
        % termal_shutdown => boolean(ThrErr),
        'SHTVDD' => boolean(SHTVDD),
        'AbvVDD' => boolean(AbvVDD),
        'OWOffF' => boolean(OWOffF),
        'OvrCurr' => boolean(OvrCurr),
        'OvlDf' => boolean(OvlDf),
        'GLOBLF' => boolean(GLOBLF),
        'ThrErr' => boolean(ThrErr),
        resp => decode_registers(Reg, RegResp)
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

-define(MAP_FIELD(Name),
    list_to_atom(??Name) => field(decode, list_to_atom(??Name), Name)
).
-define(BIN_FIELD(Name),
    (field(encode, list_to_atom(??Name), Name))
).

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
        ?MAP_FIELD(SetDi4),
        ?MAP_FIELD(SetDi3),
        ?MAP_FIELD(SetDi2),
        ?MAP_FIELD(SetDi1),
        ?MAP_FIELD(HighO4),
        ?MAP_FIELD(HighO3),
        ?MAP_FIELD(HighO2),
        ?MAP_FIELD(HighO1)
    };
reg(encode, 'SetOUT', Content) when is_map(Content) ->
    #{
        'SetDi4' := SetDi4,
        'SetDi3' := SetDi3,
        'SetDi2' := SetDi2,
        'SetDi1' := SetDi1,
        'HighO4' := HighO4,
        'HighO3' := HighO3,
        'HighO2' := HighO2,
        'HighO1' := HighO1
    } = Content,
    <<
        ?BIN_FIELD(SetDi4):1,
        ?BIN_FIELD(SetDi3):1,
        ?BIN_FIELD(SetDi2):1,
        ?BIN_FIELD(SetDi1):1,
        ?BIN_FIELD(HighO4):1,
        ?BIN_FIELD(HighO3):1,
        ?BIN_FIELD(HighO2):1,
        ?BIN_FIELD(HighO1):1
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
        ?MAP_FIELD(SLED4),
        ?MAP_FIELD(SLED3),
        ?MAP_FIELD(SLED2),
        ?MAP_FIELD(SLED1),
        ?MAP_FIELD(FLED4),
        ?MAP_FIELD(FLED3),
        ?MAP_FIELD(FLED2),
        ?MAP_FIELD(FLED1)
    };
reg(encode, 'SetLED', Content) when is_map(Content) ->
    #{
        'SLED4' := SLED4,
        'SLED3' := SLED3,
        'SLED2' := SLED2,
        'SLED1' := SLED1,
        'FLED4' := FLED4,
        'FLED3' := FLED3,
        'FLED2' := FLED2,
        'FLED1' := FLED1
    } = Content,
    <<
        ?BIN_FIELD(SLED4):1,
        ?BIN_FIELD(SLED3):1,
        ?BIN_FIELD(SLED2):1,
        ?BIN_FIELD(SLED1):1,
        ?BIN_FIELD(FLED4):1,
        ?BIN_FIELD(FLED3):1,
        ?BIN_FIELD(FLED2):1,
        ?BIN_FIELD(FLED1):1
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
        ?MAP_FIELD(SafeDemagF4),
        ?MAP_FIELD(SafeDemagF3),
        ?MAP_FIELD(SafeDemagF2),
        ?MAP_FIELD(SafeDemagF1),
        ?MAP_FIELD(DoiLevel4_VDDOKFault),
        ?MAP_FIELD(DoiLevel3_VDDOKFault),
        ?MAP_FIELD(DoiLevel2_VDDOKFault),
        ?MAP_FIELD(DoiLevel1_VDDOKFault)
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
        ?MAP_FIELD(CL4),
        ?MAP_FIELD(CL3),
        ?MAP_FIELD(CL2),
        ?MAP_FIELD(CL1),
        ?MAP_FIELD(OVL4),
        ?MAP_FIELD(OVL3),
        ?MAP_FIELD(OVL2),
        ?MAP_FIELD(OVL1)
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
        ?MAP_FIELD(AboveVDD4),
        ?MAP_FIELD(AboveVDD3),
        ?MAP_FIELD(AboveVDD2),
        ?MAP_FIELD(AboveVDD1),
        ?MAP_FIELD(OWOff4),
        ?MAP_FIELD(OWOff3),
        ?MAP_FIELD(OWOff2),
        ?MAP_FIELD(OWOff1)
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
        ?MAP_FIELD(VDDOV4),
        ?MAP_FIELD(VDDOV3),
        ?MAP_FIELD(VDDOV2),
        ?MAP_FIELD(VDDOV1),
        ?MAP_FIELD(SHVDD4),
        ?MAP_FIELD(SHVDD3),
        ?MAP_FIELD(SHVDD2),
        ?MAP_FIELD(SHVDD1)
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
reg(encode, 'Config1', Content) when is_map(Content) ->
    #{
        'LedCurrLim' := LedCurrLim,
        'FLatchEn' := FLatchEn,
        'FilterLong' := FilterLong,
        'FFilterEn' := FFilterEn,
        'FLEDStretch' := FLEDStretch,
        'SLEDSet' := SLEDSet,
        'FLEDSet' := FLEDSet
    } = Content,
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
reg(encode, 'Config2', Content) when is_map(Content) ->
    #{
        'WDTo' := WDTo,
        'OWOffCs' := OWOffCs,
        'ShtVddThr' := ShtVddThr,
        'SynchWDEn' := SynchWDEn,
        'VDDOnThr' := VDDOnThr
    } = Content,
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
reg(encode, 'ConfigDI', Content) when is_map(Content) ->
    #{
        'Typ2Di' := Typ2Di,
        'VDDFaultDis' := VDDFaultDis,
        'VDDFaultSel' := VDDFaultSel,
        'AboveVDDProtEn' := AboveVDDProtEn,
        'OVLStretchEn' := OVLStretchEn,
        'OVLBlank' := OVLBlank
    } = Content,
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
        ?MAP_FIELD(DoMode4),
        ?MAP_FIELD(DoMode3),
        ?MAP_FIELD(DoMode2),
        ?MAP_FIELD(DoMode1)
    };
reg(encode, 'ConfigDO', Content) when is_map(Content) ->
    #{
        'DoMode4' := DoMode4,
        'DoMode3' := DoMode3,
        'DoMode2' := DoMode2,
        'DoMode1' := DoMode1
    } = Content,
    <<
        ?BIN_FIELD(DoMode4):2,
        ?BIN_FIELD(DoMode3):2,
        ?BIN_FIELD(DoMode2):2,
        ?BIN_FIELD(DoMode1):2
    >>;
reg(decode, 'CurrLim', Content) when is_binary(Content) ->
    <<
        CL4:2,
        CL3:2,
        CL2:2,
        CL1:2
    >> = Content,
    #{
        ?MAP_FIELD(CL4),
        ?MAP_FIELD(CL3),
        ?MAP_FIELD(CL2),
        ?MAP_FIELD(CL1)
    };
reg(encode, 'CurrLim', Content) when is_map(Content) ->
    #{
        'CL4' := CL4,
        'CL3' := CL3,
        'CL2' := CL2,
        'CL1' := CL1
    } = Content,
    <<
        ?BIN_FIELD(CL4):2,
        ?BIN_FIELD(CL3):2,
        ?BIN_FIELD(CL2):2,
        ?BIN_FIELD(CL1):2
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
reg(encode, 'Mask', Content) when is_map(Content) ->
    #{
        'CommErrM' := CommErrM,
        'SupplyErrM' := SupplyErrM,
        'VddOKM' := VddOKM,
        'ShtVddM' := ShtVddM,
        'AboveVDDM' := AboveVDDM,
        'OWOffM' := OWOffM,
        'CurrLimM' := CurrLimM,
        'OverLdM' := OverLdM
    } = Content,
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
    Content.

field(decode, 'FLEDStretch', Value) ->
    pick(Value, {
        false,
        {millisecond, 1_000},
        {millisecond, 2_000},
        {millisecond, 3_000}
    });
field(encode, 'FLEDStretch', false) ->
    0;
field(encode, 'FLEDStretch', {millisecond, 1_000}) ->
    1;
field(encode, 'FLEDStretch', {millisecond, 2_000}) ->
    2;
field(encode, 'FLEDStretch', {millisecond, 3_000}) ->
    3;
field(decode, 'WDTo', Value) ->
    pick(Value, {
        false,
        {millisecond, 200},
        {millisecond, 600},
        {millisecond, 1_200}
    });
field(encode, 'WDTo', false) ->
    0;
field(encode, 'WDTo', {millisecond, 200}) ->
    1;
field(encode, 'WDTo', {millisecond, 600}) ->
    2;
field(encode, 'WDTo', {millisecond, 1_200}) ->
    3;
field(decode, 'OWOffCs', Value) ->
    {microampere, pick(Value, {60, 100, 300, 600})};
field(encode, 'OWOffCs', {microampere, 60}) ->
    0;
field(encode, 'OWOffCs', {microampere, 100}) ->
    1;
field(encode, 'OWOffCs', {microampere, 300}) ->
    2;
field(encode, 'OWOffCs', {micro_apmere, 600}) ->
    3;
field(decode, 'ShtVDDThr', Value) ->
    {volt, pick(Value, {9, 10, 12, 14})};
field(encode, 'ShtVDDThr', {volt, 9}) ->
    0;
field(encode, 'ShtVDDThr', {volt, 10}) ->
    1;
field(encode, 'ShtVDDThr', {volt, 12}) ->
    2;
field(encode, 'ShtVDDThr', {volt, 14}) ->
    3;
field(decode, 'Typ2Di', 0) ->
    type_1_3;
field(decode, 'Typ2Di', 1) ->
    type_2;
field(encode, 'Typ2Di', type_1_3) ->
    0;
field(encode, 'Typ2Di', type_2) ->
    1;
field(decode, 'OVLBlack', Value) ->
    pick(Value, {
        false,
        {millisecond, 8},
        {millisecond, 50},
        {millisecond, 300}
    });
field(encode, 'OVLBlack', false) ->
    0;
field(encode, 'OVLBlack', {millisecond, 8}) ->
    1;
field(encode, 'OVLBlack', {millisecond, 50}) ->
    2;
field(encode, 'OVLBlack', {millisecond, 300}) ->
    3;
field(decode, Reg, Value) when
    Reg == 'DoMode4'; Reg == 'DoMode3'; Reg == 'DoMode2'; Reg == 'DoMode1'
->
    pick(Value, {
        high_side,
        high_side_2x,
        active_clamp_push_pull,
        simple_push_pull
    });
field(encode, Reg, high_side) when
    Reg == 'DoMode4'; Reg == 'DoMode3'; Reg == 'DoMode2'; Reg == 'DoMode1'
->
    16#00;
field(encode, Reg, high_side_2x) when
    Reg == 'DoMode4'; Reg == 'DoMode3'; Reg == 'DoMode2'; Reg == 'DoMode1'
->
    16#01;
field(encode, Reg, active_clamp_push_pull) when
    Reg == 'DoMode4'; Reg == 'DoMode3'; Reg == 'DoMode2'; Reg == 'DoMode1'
->
    16#10;
field(encode, Reg, simple_push_pull) when
    Reg == 'DoMode4'; Reg == 'DoMode3'; Reg == 'DoMode2'; Reg == 'DoMode1'
->
    16#11;
field(decode, Reg, Value) when
    Reg == 'CL4'; Reg == 'CL3'; Reg == 'CL2'; Reg == 'CL1'
->
    pick(
        Value,
        {
            #{current_limit => {milliampere, 600}, inrush => {millisecond, 20}},
            #{current_limit => {milliampere, 130}, inrush => {millisecond, 50}},
            #{current_limit => {milliampere, 300}, inrush => {millisecond, 40}},
            #{current_limit => {milliampere, 1200}, inrush => {millisecond, 10}}
        }
    );
field(encode, Reg, #{
    current_limit := {milliampere, 600}, inrush := {millisecond, 20}
}) when
    Reg == 'CL4'; Reg == 'CL3'; Reg == 'CL2'; Reg == 'CL1'
->
    16#00;
field(encode, Reg, #{
    current_limit := {milliampere, 130}, inrush := {millisecond, 50}
}) when
    Reg == 'CL4'; Reg == 'CL3'; Reg == 'CL2'; Reg == 'CL1'
->
    16#01;
field(encode, Reg, #{
    current_limit := {milliampere, 300}, inrush := {millisecond, 40}
}) when
    Reg == 'CL4'; Reg == 'CL3'; Reg == 'CL2'; Reg == 'CL1'
->
    16#10;
field(encode, Reg, #{
    current_limit := {milliampere, 1200}, inrush := {millisecond, 10}
}) when
    Reg == 'CL4'; Reg == 'CL3'; Reg == 'CL2'; Reg == 'CL1'
->
    16#11;
field(decode, _Name, 0) ->
    false;
field(decode, _Name, 1) ->
    true;
field(encode, _Name, false) ->
    0;
field(encode, _Name, true) ->
    1;
field(encode, Name, Value) ->
    error({invalid_field_value, Name, Value}).

boolean(0) -> false;
boolean(1) -> true.

pick(Index, Values) -> element(Index + 1, Values).
