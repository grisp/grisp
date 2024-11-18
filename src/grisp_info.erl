% @doc GRiSP general information.
%
% When running grisp on a host machine during development, the functions return:
% 
% ```
% 1> grisp_info:hardware().
% #{
%     platform => host,
%     version => <<"1">>,
%     serial => <<"0000">>
%     batch => 1,
%     pcb_variant => 1
%     pcb_version => <<"1">>,
%     prod_date => {{2022, 1, 28},{0,0,0}}
% }
% 2> grisp_info:boot().
% #{
%     boot => host,
%     valid => host,
%     next => host
% }
% '''
%
% For grisp_info:software() to be defined during local development, a term file
% named `MANIFEST' must exists in the current directory and looks like:
%
% ```
% %% coding: utf-8
% {platform, <<"grisp2">>}.
% {id, <<"83613140847d62b47c3f220b6f43e2de561be441">>}.
% {relname, <<"my_app">>}.
% {relvsn, <<"0.1.0">>}.
% {profiles, [dev]}.
% {package, [
%     {toolchain, [{revision, <<"516707805fd33285679e9e64585b995d31d926ec">>}]},
%     {rtems, [{version, <<"5">>}]},
%     {otp, [{version, <<"26.2.5.4">>}]}
% ]}.
% '''
%
% @end
-module(grisp_info).

-include("grisp.hrl").

% API Functions
-export([hardware/0]).
-export([boot/0]).
-export([software/0, software/1]).


%--- Types ---------------------------------------------------------------------

-type hardware_info() :: #{
    platform := atom(),
    version := binary(),
    serial := binary(),
    batch := pos_integer(),
    pcb_variant := pos_integer(),
    pcb_version := binary(),
    prod_date := calendar:datetime()
}.

-type boot_system() :: system_a | system_b.
-type boot_source() :: sdcard | boot_system().

-type boot_info() :: #{
    boot := boot_source(),
    active := boot_system(),
    next := boot_system()
}.

-type software_info() :: #{
    id := undefined | binary(),
    relname := undefined | binary(),
    relvsn := undefined | binary(),
    profiles := undefined | [atom()],
    toolchain_rev := undefined | binary(),
    rtems_ver := undefined | binary(),
    otp_ver := undefined | binary()
}.


%--- API Functions -------------------------------------------------------------

-spec hardware() -> hardware_info().
hardware() ->
    case ?IS_EMULATED of
        true -> emulated_hardware_info();
        false -> hardware_info()
    end.

-spec boot() -> boot_info().
boot() ->
    case ?IS_EMULATED of
        true -> emulated_boot_info();
        false -> boot_info()
    end.

-spec software() -> software_info().
software() ->
    case ?IS_EMULATED of
        true -> emulated_software_info();
        false -> software_info()
    end.

-spec software(Source :: boot_source()) -> software_info() | undefined.
software(Source) ->
    case ?IS_EMULATED of
        true -> emulated_software_info(Source);
        false -> software_info(Source)
    end.


%--- Internal Functions --------------------------------------------------------

system_tag(0) -> system_a;
system_tag(1) -> system_b.

boot_source() ->
    % TODO: Using the module path to figure out the current
    % booted system, should be changed to use the device tree.
    case filename:absname(code:which(?MODULE)) of
        "/media/mmcsd-0-0" ++ _ -> system_a;
        "/media/mmcsd-0-1" ++ _ -> system_b;
        "/media/mmcsd-1-0" ++ _ -> sdcard
    end.

boot_root(sdcard) -> <<"/media/mmcsd-1-0">>;
boot_root(system_a) -> <<"/media/mmcsd-0-0">>;
boot_root(system_b) -> <<"/media/mmcsd-0-1">>.

emulated_boot_info() ->
    #{boot => host, active => host, next => host}.

emulated_hardware_info() ->
    #{
        platform => host,
        version => <<"1">>,
        serial => <<"0000">>,
        batch => 1,
        pcb_variant => 1,
        pcb_version => <<"1">>,
        prod_date => {{2022, 1, 28},{0,0,0}}
    }.

emulated_software_info() ->
    % Read the manifest from the root of the working directory
    read_manifest(<<"MANIFEST">>).

emulated_software_info(host) ->
    emulated_software_info();
emulated_software_info(_) ->
    undefined.

boot_info() ->
    % TODO: Using the module path to figure out the current
    % booted system, should be changed to use the device tree.
    Boot = boot_source(),
    ActiveSysId = grisp_barebox:get([bootstate, active_system]),
    UpdateSysId = grisp_barebox:get([bootstate, update_system]),
    BootCount = grisp_barebox:get([bootstate, update_boot_count]),
    NextSysId = case {ActiveSysId, UpdateSysId, BootCount} of
        {_ActiveSysId, UpdateSysId, Count} when Count > 0 -> UpdateSysId;
        {ActiveSysId, _UpdateSysId, 0} -> ActiveSysId
    end,
    #{boot => Boot,
      active => system_tag(ActiveSysId),
      next => system_tag(NextSysId)}.

hardware_info() ->
    Platform = grisp_hw:platform(),
    % We ignore if the eeprom CRC check fails or not...
    {_, #{
        grisp_batch := Batch,
        grisp_pcb_variant := PcbVariant,
        grisp_pcb_version := PcbVersionStr,
        grisp_prod_date := ProdDate,
        grisp_serial := SerialInt,
        grisp_version := VersionStr
    }} = grisp_hw:eeprom_read(),
    #{
        platform => Platform,
        version => list_to_binary(VersionStr),
        serial => integer_to_binary(SerialInt),
        batch => Batch,
        pcb_variant => PcbVariant,
        pcb_version => list_to_binary(PcbVersionStr),
        prod_date => ProdDate
    }.    

software_info() ->
    software_info(boot_source()).    

software_info(Source) ->
    Root = boot_root(Source),
    ManifestPath = <<Root/binary, "/MANIFEST">>,
    read_manifest(ManifestPath).

read_manifest(Path) ->
    case file:consult(Path) of
        {ok, Term} when is_list(Term) ->
            PackageInfo = proplists:get_value(package, Term, []),
            ToolchainInfo = proplists:get_value(toolchain, PackageInfo, []),
            RtemsInfo = proplists:get_value(rtems, PackageInfo, []),
            OtpInfo = proplists:get_value(otp, PackageInfo, []),
            #{
                id => proplists:get_value(id, Term),
                relname => proplists:get_value(relname, Term),
                relvsn => proplists:get_value(relvsn, Term),
                profiles => proplists:get_value(profiles, Term),
                toolchain_rev => proplists:get_value(revision, ToolchainInfo),
                rtems_ver => proplists:get_value(version, RtemsInfo),
                otp_ver => proplists:get_value(version, OtpInfo)
            };
        _ -> undefined
    end.
