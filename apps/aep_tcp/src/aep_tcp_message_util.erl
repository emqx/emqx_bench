%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 2月 2021 2:41 下午
%%%-------------------------------------------------------------------
-module(aep_tcp_message_util).
-author("DDDHuang").
-include("aep_tcp.hrl").

%% API
-export([]).

register_message(DeviceID, Password, Version) ->
    DeviceIDLen = length(DeviceID),
    PasswordLen = length(Password),
    VersionLen  = length(Version),
    <<?MESSAGE_TYPE_LOGIN:16, DeviceIDLen:16, DeviceID/binary, PasswordLen:16, Password/binary, VersionLen:16, Version/binary>>.
register_message_standard_module(DeviceID, Password, Version) ->
    DeviceIDLen = length(DeviceID),
    PasswordLen = length(Password),
    VersionLen  = length(Version),
    %% wtf
    Module          = "e_module",
    ModuleLen       = length(Module),
    ChipType        = "e_chiptype",
    ChipTypeLen     = length(ChipType),
    SoftVersion     = "e_sv",
    SoftVersionLen  = length(SoftVersion),
    IMEI            = "e_imei",
    IMEILen         = length(IMEI),
    ICCID           = "e_iccid",
    ICCIDLen        = length(ICCID),
    IMSI            = "e_imsi",
    IMSILen         = length(IMSI),
    StandardModuleInfo =
        <<  ModuleLen:16, Module/binary,
            ChipTypeLen:16, ChipType/binary,
            SoftVersionLen:16, SoftVersion/binary,
            IMEILen:16, IMEI/binary,
            ICCIDLen:16, ICCID/binary,
            IMSILen:16, IMSI/binary
        >>,
    <<?MESSAGE_TYPE_LOGIN:16, DeviceIDLen:16, DeviceID/binary, PasswordLen:16, Password/binary, VersionLen:16, Version/binary, StandardModuleInfo/binary>>.




