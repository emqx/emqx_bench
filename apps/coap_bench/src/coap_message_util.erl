%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 1月 2021 10:50 上午
%%%-------------------------------------------------------------------
-module(coap_message_util).
-author("DDDHuang").

%% API
-export([serialize/1, parse/1]).
-export([get_option_param/3, get_uri_path/1]).

-export([build_option/2]).

-include("coap.hrl").

%%------------------------------------------------------------------
%% serialize
%%------------------------------------------------------------------
-spec serialize(#coap_message{}) -> {ok, binary()} | {error, not_support}.
serialize(#coap_message{
    type    = Type,
    token   = Token,
    method  = #method{code = Code, detail = Detail},
    id      = MessageID,
    options = Options,
    payload = PayLoad}) ->
    Result  = list_to_binary(
                [<<?VERSION:2,
                (get_code_by_type(Type)):2,
                (size(Token)):4,
                Code:3, Detail:5, MessageID:16>>,
                Token,
                (encode_options(Options)),
                (encode_payload(PayLoad))]),
    {ok, Result};
serialize(_Data) -> {error, not_support}.

%%------------------------------------------------------------------
%% parse
%%------------------------------------------------------------------
-spec parse(binary()) -> {ok, #coap_message{}} | {error, any()}.
parse(<<?VERSION:2, TypeCode:2, TKL:4, MethodCode:3, MethodCodeDetail:5, MessageID:16, Token:TKL/bytes,
    Tail/binary>>) ->
    {Options, Payload} = decode_options_payload(Tail),
    CoAPMessage = #coap_message{
                        type    = get_type_by_code(TypeCode),
                        method  = get_method_by_code(MethodCode, MethodCodeDetail),
                        id      = MessageID,
                        token   = Token,
                        options = Options,
                        payload = Payload
                    },
    {ok, CoAPMessage};
parse(_Data) -> {error, un_support_packet}.
%%------------------------------------------------------------------
%% uri path

get_uri_path(#coap_message{options = Options}) -> {ok, get_uri_path(Options, "")};
get_uri_path(_) -> {error, un_support}.

%%------------------------------------------------------------------
%% any option param

get_option_param(#coap_message{options = Options} = CoAPMessage, Key, DataType)
    when is_record(CoAPMessage, coap_message) ->
    {ok, get_option_param(Options, Key, DataType, [])};
get_option_param(_Data, _Key, _DataType) -> {error, un_support}.

%%------------------------------------------------------------------
%%  internal function
%%------------------------------------------------------------------

%%------------------------------------------------------------------
%% encode function
encode_options(Options) ->
    SortOption = fun(#option{code = Code1}, #option{code = Code2}) -> Code2 >= Code1 end,
    list_to_binary(encode_options(lists:sort(SortOption, Options), 0, [])).
encode_options([], _LastOptionCode, Result) -> Result;
encode_options([Option = #option{code = Code} | Tail], LastOptionCode, Result) ->
    encode_options(Tail, Code, Result ++ [encode_option(LastOptionCode, Option)]).
encode_option(LastOptionCode, #option{value = Value} = Option) when is_integer(Value) ->
    encode_option(LastOptionCode, Option#option{value = binary:encode_unsigned(Value)});
encode_option(LastOptionCode, #option{code = Code, value = Value}) ->
    {Delta, DeltaTail}   = encode_delta_len(Code - LastOptionCode),
    {Length, LengthTail} = encode_delta_len(size(Value)),
    list_to_binary([<<Delta:4, Length:4>>, DeltaTail, LengthTail, Value]).

encode_delta_len(DeltaOrLength) when DeltaOrLength < 13  -> {DeltaOrLength, <<>>};
encode_delta_len(DeltaOrLength) when DeltaOrLength < 269 -> {13, <<(DeltaOrLength - 13):8>>};
encode_delta_len(DeltaOrLength)                          -> {14, <<(DeltaOrLength - 269):16>>}.

encode_payload(Payload) when is_binary(Payload) -> <<16#FF:8, Payload/binary>>;
encode_payload(?NO_PAYLOAD) -> <<>>.

%%------------------------------------------------------------------
%% decode function
decode_options_payload(Data) -> decode_options_payload(Data, 0, []).

decode_options_payload(<<>>,                              _LastOptionCode, OptionList)  -> {OptionList, ?NO_PAYLOAD};
decode_options_payload(<<16#FF, Payload/binary>>,         _LastOptionCode, OptionList)  -> {OptionList, Payload};
decode_options_payload(<<Delta:4, Length:4, Tail/binary>>, LastOptionCode, OptionList)  ->
    {RealDelta , AfterDeltaTail }              = decode_delta_len(Delta, Tail),
    {RealLength, AfterLengthTail}              = decode_delta_len(Length, AfterDeltaTail),
    <<Value:RealLength/bytes, NewTail/binary>> = AfterLengthTail,
    OptionCode                                 = LastOptionCode + RealDelta,
    decode_options_payload(NewTail, OptionCode, OptionList ++ [build_option(OptionCode, Value)]).

decode_delta_len(Delta, Tail)                              when Delta <  13 -> {Delta,             Tail};
decode_delta_len(Delta, <<DeltaTail:8,  Tail/binary>>)     when Delta == 13 -> {DeltaTail + 13,    Tail};
decode_delta_len(Delta, <<DeltaTail:16, Tail/binary>>)     when Delta == 14 -> {DeltaTail + 269,   Tail}.

get_uri_path([], Result) -> Result;
get_uri_path([#option{name = uri_path, value = Value} | Tail], Result) ->
    get_uri_path(Tail, list_to_binary([Result, "/", Value]));
get_uri_path([#option{name = _OtherOption, value = _Value} | Tail], Result) ->
    get_uri_path(Tail, Result).

get_option_param([], _Key, _DataType, Result) -> lists:reverse(Result);
get_option_param([Data | Tail], Key, DataType, Result) ->
    case check_option(Data, Key) of
        {ok, Value} -> get_option_param(Tail, Key, DataType, [trans_data_type(Value, DataType) | Result]);
        error       -> get_option_param(Tail, Key, DataType, Result)
    end.

check_option({Key, Value}, Key)         -> {ok, Value};
check_option({_OtherKey, _Value}, _Key) -> error;
check_option(#option{name = Key, value = Value}, Key)         -> {ok, Value};
check_option(#option{name = _OtherKey, value = _Value}, _Key) -> error.

trans_data_type(Data, binary) when is_binary(Data)  -> Data;
trans_data_type(Data, binary) when is_integer(Data) -> binary:encode_unsigned(Data);
trans_data_type(Data, integer) when is_integer(Data) -> Data;
trans_data_type(Data, integer) when is_binary(Data) -> binary:decode_unsigned(Data).

%%------------------------------------------------------------------
%%  build function

build_option(Option, Value) when is_record(Option, option) ->  Option#option{value = Value};
build_option(Code  , Value) when is_integer(Code) -> build_option(get_option_by_code(Code), Value).

%%------------------------------------------------------------------
%% parse function

%% code => message type
get_code_by_type(?CON)    -> 0;
get_code_by_type(?NON)    -> 1;
get_code_by_type(?ACK)    -> 2;
get_code_by_type(?RESET)  -> 3.
%% message type => code
get_type_by_code(2#00)    -> ?CON;
get_type_by_code(2#01)    -> ?NON;
get_type_by_code(2#10)    -> ?ACK;
get_type_by_code(2#11)    -> ?RESET.
%% code => method
get_method_by_code(0, 0 ) -> ?EMPTY;
get_method_by_code(0, 1 ) -> ?GET;
get_method_by_code(0, 2 ) -> ?POST;
get_method_by_code(0, 3 ) -> ?PUT;
get_method_by_code(0, 4 ) -> ?DELETE;
get_method_by_code(2, 1 ) -> ?CREATED;
get_method_by_code(2, 2 ) -> ?DELETED;
get_method_by_code(2, 3 ) -> ?VALID;
get_method_by_code(2, 4 ) -> ?CHANGED;
get_method_by_code(2, 5 ) -> ?CONTENT;
get_method_by_code(4, 0 ) -> ?BAD_REQUEST;
get_method_by_code(4, 1 ) -> ?UNAUTHORIZED;
get_method_by_code(4, 2 ) -> ?BAD_OPTION;
get_method_by_code(4, 3 ) -> ?FORBIDDEN;
get_method_by_code(4, 4 ) -> ?NOT_FOUND;
get_method_by_code(4, 5 ) -> ?METHOD_NOT_ALLOWED;
get_method_by_code(4, 6 ) -> ?NOT_ACCEPTABLE;
get_method_by_code(4, 12) -> ?PRECONDITION_FAILED;
get_method_by_code(4, 13) -> ?REQUEST_ENTITY_TOO_LARGE;
get_method_by_code(4, 15) -> ?UNSUPPORTED_CONTENT_FORMAT;
get_method_by_code(5, 0 ) -> ?INTERNAL_SERVER_ERROR;
get_method_by_code(5, 1 ) -> ?NOT_IMPLEMENTED;
get_method_by_code(5, 2 ) -> ?BAD_GATEWAY;
get_method_by_code(5, 3 ) -> ?SERVICE_UNAVAILABLE;
get_method_by_code(5, 4 ) -> ?GATEWAY_TIMEOUT;
get_method_by_code(5, 5 ) -> ?PROXYING_NOT_SUPPORTED.
%% code => option def
get_option_by_code(0 )    -> ?RESERVED;
get_option_by_code(1 )    -> ?IF_MATCH;
get_option_by_code(3 )    -> ?URI_HOST;
get_option_by_code(4 )    -> ?ETAG;
get_option_by_code(5 )    -> ?IF_NOT_MATCH;
get_option_by_code(6 )    -> ?URI_OBSERVE;
get_option_by_code(7 )    -> ?URI_PORT;
get_option_by_code(8 )    -> ?LOCATION_PATH;
get_option_by_code(11)    -> ?URI_PATH;
get_option_by_code(12)    -> ?CONTENT_FORMAT;
get_option_by_code(14)    -> ?MAX_AGE;
get_option_by_code(15)    -> ?URI_QUERY;
get_option_by_code(17)    -> ?ACCEPT;
get_option_by_code(20)    -> ?LOCATION_QUERY;
get_option_by_code(35)    -> ?PROXY_URI;
get_option_by_code(39)    -> ?PROXY_SCHEME;
get_option_by_code(60)    -> ?SIZE1;
%% other option
get_option_by_code(C ) -> #option{name = un_know, code = C}.

