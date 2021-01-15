%%%-------------------------------------------------------------------
%%% @author huangdi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%  Type Length Value
%%% @end
%%% Created : 24. 8月 2020 3:04 下午
%%%-------------------------------------------------------------------
-module(tlv_tool).
-author("huangdi").

%% API
-export([encode/1, decode/1]).

-include("tlv.hrl").

encode(#tlv{tag = Tag, id = ID, value = Value}) when is_binary(Value) ->
    {ValueLenTag, ValueLen_Append, LenBytes} = get_value_len_type(Value),
    {IDLenTag, IDBytes} = get_Id_len_type(ID),
    <<Tag:2, IDLenTag:1, ValueLenTag:2, ValueLen_Append:3, IDBytes/bytes, LenBytes/bytes, Value/bytes>>.


decode(Bytes) ->
%%  todo
    noreply.


get_Id_len_type(ID) when (ID =< 255) -> {0, <<ID:8>>};
get_Id_len_type(ID) -> {1, <<ID:16>>}.

get_value_len_type(Value) when (byte_size(Value) =< 7) ->
    {2#0, byte_size(Value), <<>>};
get_value_len_type(Value) when (byte_size(Value) =< 255) ->
    Len = byte_size(Value),
    {2#01, 0, <<Len:8>>};
get_value_len_type(Value) when (byte_size(Value) =< 65535) ->
    Len = byte_size(Value),
    {2#10, 0, <<Len:16>>};
get_value_len_type(Value) when (byte_size(Value) =< 16777215) ->
    Len = byte_size(Value),
    {2#11, 0, <<Len:24>>}.

