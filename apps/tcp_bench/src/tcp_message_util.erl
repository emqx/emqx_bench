%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 2月 2021 2:41 下午
%%%-------------------------------------------------------------------
-module(tcp_message_util).
-author("DDDHuang").
-include("aep_tcp.hrl").

%% API
-export([register_message/3,register_message_standard_module/3]).

-export([heart_beat/0]).

-export([publish/1,publish/3]).

-export([command_response/2,command_response/3]).

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

heart_beat() ->
    <<?MESSAGE_TYPE_HEART_BEAT:16>>.

-spec publish(binary()) -> binary().
publish(Data) ->
    DataLen = length(Data),
    <<?MESSAGE_TYPE_UP:16, DataLen:16, Data/binary>>.

-spec publish(integer(), integer(), binary()) -> binary().
publish(MessageID, DatasetID, Data) ->
    MessageIDBinary = binary:encode_unsigned(MessageID),
    MessageIDLen = size(MessageIDBinary),
    AllDataLen = length(Data) + 2,
    <<?MESSAGE_TYPE_UP:16, MessageIDLen:16, MessageIDBinary/binary, AllDataLen:16, DatasetID:16, Data/binary>>.


%% executed success with dataset id and data
%% or
%% success with no dataset id and no data
%% or
%% fail with no dataset id and no data
command_response(TaskID, DatasetID, ResponseData) ->
    AllResponseData = <<TaskID:2, ?EXECUTE_COMMAND_SUCCESS:16, DatasetID:16, ResponseData/binary>>,
    AllResponseDataSize = size(AllResponseData),
    <<?MESSAGE_TYPE_DOWN_ACK:16, AllResponseDataSize:16, AllResponseData/binary>>.
command_response(TaskID, ExecuteResult) ->
    AllResponseData = <<TaskID:2, ExecuteResult:16>>,
    AllResponseDataSize = size(AllResponseData),
    <<?MESSAGE_TYPE_DOWN_ACK:16, AllResponseDataSize:16, AllResponseData/binary>>.




