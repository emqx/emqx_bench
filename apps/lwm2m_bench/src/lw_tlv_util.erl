%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 1月 2021 3:16 下午
%%%-------------------------------------------------------------------
-module(lw_tlv_util).
-author("DDDHuang").

-include("emqx_lw_tlv.hrl").
%% API
-export([encode/1]).

encode(TLVList) when is_list(TLVList) -> encode(TLVList,[]).

encode([#tlv{type = #tlv_type{code = Code}, identifier = ID, value = Value} | Tail], Result)
    when is_binary(Value) -> encode(Tail, Result ++ [ encode_tlv(Code,ID,Value)]);
encode([#tlv{type = #tlv_type{code = Code}, identifier = ID, value = Value} | Tail], Result)
    when is_record(Value, tlv) or is_list(Value) -> encode(Tail, Result ++ [ encode_tlv(Code, ID, encode(Value))]);
encode([], Result)-> list_to_binary(Result).

encode_tlv(TypeCode, ID, Value )->
    {IDFlag, IDBinary} = encode_id(ID),
    {LengthFlag, LengthBinary} = encode_length(Value),
    <<TypeCode:2, IDFlag:1, LengthFlag:5, IDBinary/binary, LengthBinary/binary, Value/binary>>.

encode_id(ID) when ID =< 16#FF -> {0, binary:encode_unsigned(ID)};
encode_id(ID) when ID >  16#FF -> {1, binary:encode_unsigned(ID)}.
%% return length flag and length encode
-spec encode_length(binary()) -> {integer(), binary()}.
encode_length(Value) when size(Value) =< 7            ->                  {size(Value),     <<>>};
encode_length(Value) when size(Value) =< 16#FF        -> L = size(Value), {2#01000    ,     << L:8  >>};
encode_length(Value) when size(Value) =< 16#FFFF      -> L = size(Value), {2#10000    ,     << L:16 >>};
encode_length(Value) when size(Value) =< 16#FFFFFF    -> L = size(Value), {2#11000    ,     << L:24 >>}.
