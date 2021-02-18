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

-include("tlv.hrl").
-export([encode/1,decode/1]).

-spec encode(list()) -> binary().
encode(TLVList) when is_list(TLVList) -> encode(TLVList,[]).
encode([#tlv{type = TypeCode, identifier = ID, value = Value} | Tail], Result)
    when is_binary(Value) -> encode(Tail, Result ++ [ encode_tlv(TypeCode,ID,Value)]);
encode([#tlv{type = TypeCode, identifier = ID, value = Value} | Tail], Result)
    when is_record(Value, tlv) or is_list(Value) -> encode(Tail, Result ++ [ encode_tlv(TypeCode, ID, encode(Value))]);
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

-spec decode(binary()) -> list().
decode(Data) when is_binary(Data) ->
    decode_tlv(Data, []).
decode_tlv(<<TypeCode:2, IDFlag:1, LengthFlag:5, Data/binary>>, Result) ->
    ID_ = decode_id_size(IDFlag),
    %%  should be like
    %%  {LengthAdd, LengthSize} = decode_len_size(LengthFlag) -> {0, 8|16|24} | {Length, 0}
    %%  <<ID:ID_, Length:LengthSize, Value:(LengthAdd + Length)/binary, Tail/binary>> = Data,
    %%  but erlang do not support code "Value:(LengthAdd + Length)/binary"
    %%  such a pity
    case decode_len_size(LengthFlag) of
        {ok, Length} ->
            <<ID:ID_, Value:Length/binary, Tail/binary>> = Data,
            decode_tlv(Tail, Result ++ [#tlv{type = TypeCode, length = Length, identifier = ID, value = decode_value(TypeCode, Value)}]);
        {more, LengthSize} ->
            <<ID:ID_, Length:LengthSize, Value:Length/binary, Tail/binary>> = Data,
            decode_tlv(Tail, Result ++ [#tlv{type = TypeCode, length = Length, identifier = ID, value = decode_value(TypeCode, Value)}])
    end;
decode_tlv(<<>>, Result) -> Result.

decode_id_size(0) -> 8;
decode_id_size(1) -> 16.

decode_len_size(LengthFlag) when LengthFlag =< 7        -> {ok, LengthFlag};
decode_len_size(LengthFlag) when LengthFlag =:= 2#01000 -> {more, 8};
decode_len_size(LengthFlag) when LengthFlag =:= 2#10000 -> {more, 16};
decode_len_size(LengthFlag) when LengthFlag =:= 2#11000 -> {more, 24};
decode_len_size(LengthFlag) when LengthFlag >  7        -> decode_len_size(LengthFlag band 2#11000).

decode_value(?OBJECT,   Value) -> decode(Value);
decode_value(?RESOURCE, Value) -> Value;
decode_value(?MULTIPLE, Value) -> decode(Value);
decode_value(?VALUE,    Value) -> Value.



