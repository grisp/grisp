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
    prod_date := calendar:date_time()
}.

-type boot_system() :: system_a | system_b.
-type boot_source() :: sdcard | boot_system().

-type boot_info() :: #{
    boot := boot_source(),
    active := boot_system(),
    next := boot_system()
}.

-type software_info() :: #{
    id := binary(),
    relname := binary(),
    relvsn := binary(),
    profiles := [binary()]
}.


%--- API Functions -------------------------------------------------------------

-spec hardware() -> hardware_info().
hardware() ->
    case ?IS_EMULATED of
        false -> emulated_hardware_info();
        true -> hardware_info()
    end.

-spec boot() -> boot_info().
boot() ->
    case ?IS_EMULATED of
        false -> emulated_boot_info();
        true -> boot_info()
    end.

-spec software() -> software_info().
software() ->
    case ?IS_EMULATED of
        false -> emulated_software_info();
        true -> software_info()
    end.

-spec software(Source :: boot_source()) -> software_info() | undefined.
software(Source) ->
    case ?IS_EMULATED of
        false -> emulated_software_info(Source);
        true -> software_info(Source)
    end.



%--- Internal Functions --------------------------------------------------------

system_tag(0) -> system_a;
system_tag(1) -> system_b.

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
    erlang:error(not_implemented).

boot_info() ->
    % TODO: Using the module path to figure out the current
    % booted system, should be changed to use the device tree.
    Boot = case filename:absname(code:which(?MODULE)) of
        "/media/mmcsd-0-0" ++ _ -> system_a;
        "/media/mmcsd-0-1" ++ _ -> system_b;
        "/media/mmcsd-1-" ++ _ -> sdcard
    end,
    ActiveSysId = grisp_barebox:get([bootstate, active_system]),
    UpdateSysId = grisp_barebox:get([bootstate, update_system]),
    BootCount = grisp_barebox:get([bootstate, update_boot_count]),
    NextSysId = case {ActiveSysId, UpdateSysId, BootCount} of
        {_ActiveSysId, UpdateSysId, Count} when Count > 0 -> UpdateSysId;
        {ActiveSysId, _UpdateSysId, 0} -> ActiveSysId
    end,
    #{boot => Boot, active => ActiveSysId, next => NextSysId}.

hardware_info() ->
    erlang:error(not_implemented).


software_info() ->
    erlang:error(not_implemented).

software_info(_Source) ->
    erlang:error(not_implemented).

read_manifest(_Path) ->
    erlang:error(not_implemented).