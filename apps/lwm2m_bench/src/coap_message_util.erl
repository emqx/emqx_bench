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
-export([encode/1, decode/1, decode_trans/1]).
-export([get_param/3]).

-export([get_message_id_token/1,get_message_id/1, get_token/1]).

-export([build_option/2]).

-include("coap.hrl").

%%------------------------------------------------------------------
%% encode
%%------------------------------------------------------------------
encode(#coap_message{
    type    = Type,
    token   = Token,
    method  = #method{code = Code, detail = Detail},
    id      = MessageID,
    options = Options,
    payload = PayLoad}) ->
    list_to_binary(
        [<<?VERSION:2,
            (get_code_by_type(Type)):2,
            (size(Token)):4,
            Code:3, Detail:5, MessageID:16>>,
            Token,
            (encode_options(Options)),
            (encode_payload(PayLoad))]).

encode_options(Options) ->
    list_to_binary(encode_options(Options, 0, [])).
encode_options([Option = #option{code = Code} | Tail], LastOptionCode, Result) ->
    encode_options(Tail, Code, Result ++ [encode_option(LastOptionCode, Option)]);
encode_options([], _LastOptionCode, Result) ->
    Result.
encode_option(LastOptionCode, #option{value = Value} = Option) when is_integer(Value) ->
    encode_option(LastOptionCode, Option#option{value = binary:encode_unsigned(Value)});
encode_option(LastOptionCode, #option{code = Code, value = Value}) ->
    {Delta, DeltaTail}   = encode_delta_len(Code - LastOptionCode),
    {Length, LengthTail} = encode_delta_len(size(Value)),
    list_to_binary([<<Delta:4, Length:4>>, DeltaTail, LengthTail, Value]).

encode_delta_len(DeltaOrLength) when DeltaOrLength < 13  -> {DeltaOrLength, <<>>};
encode_delta_len(DeltaOrLength) when DeltaOrLength < 269 -> {13, <<(DeltaOrLength - 13):8>>};
encode_delta_len(DeltaOrLength)                          -> {14, <<(DeltaOrLength - 269):16>>}.

encode_payload(Payload) when is_binary(Payload)     -> <<16#FF:8, Payload/binary>>;
encode_payload(?NO_PAYLOAD)                         -> <<>>.

%%------------------------------------------------------------------
%% decode
%%------------------------------------------------------------------
-spec decode(binary()) -> #coap_message{}.
decode(<<Version:2, TypeCode:2, TKL:4, MethodCode:3, MethodCodeDetail:5, MessageID:16, Token:TKL/bytes, Tail/binary>>) ->
    Version = 1,
    {Options, Payload} = decode_options_payload(Tail),
    #coap_message{
        type    = get_type_by_code(TypeCode),
        method  = get_method_by_code(MethodCode, MethodCodeDetail),
        id      = MessageID,
        token   = Token,
        options = Options,
        payload = Payload
    }.

decode_options_payload(Data)-> decode_options_payload(Data, 0, []).

decode_options_payload(<<>>,                              _LastOptionCode, OptionList)  -> {OptionList, ?NO_PAYLOAD};
decode_options_payload(<<16#FF, Payload/binary>>,         _LastOptionCode, OptionList)  -> {OptionList, Payload};
decode_options_payload(<<Delta:4, Length:4, Tail/binary>>, LastOptionCode, OptionList)  ->
    {RealDelta , AfterDeltaTail }              = decode_delta_len(Delta, Tail),
    {RealLength, AfterLengthTail}              = decode_delta_len(Length, AfterDeltaTail),
    <<Value:RealLength/bytes, NewTail/binary>> = AfterLengthTail,
    OptionCode                                 = LastOptionCode + RealDelta,
    decode_options_payload(NewTail, OptionCode, OptionList ++ [build_option(get_option_by_code(OptionCode), Value)]).

decode_delta_len(Delta, Tail)                              when Delta <  13 -> {Delta,             Tail};
decode_delta_len(Delta, <<DeltaTail:8,  Tail/binary>>)     when Delta == 13 -> {DeltaTail + 13,    Tail};
decode_delta_len(Delta, <<DeltaTail:16, Tail/binary>>)     when Delta == 14 -> {DeltaTail + 269,   Tail}.

%%------------------------------------------------------------------
%% request & response
%%------------------------------------------------------------------
decode_trans(<<Version:2, TypeCode:2, TKL:4, MethodCode:3, MethodCodeDetail:5, MessageID:16, Token:TKL/bytes, Tail/binary>>)->
    Version = 1,
    Type = get_type_by_code(TypeCode),
    Method = get_method_by_code(MethodCode, MethodCodeDetail),
    {OptionList, PayLoad} = decode_options_payload(Tail),
    Trans = #coap_trans{
        method  = Method,
        path    = <<>>,
        query   = [],
        param   = [{type, Type}, {messageId, MessageID}, {token, Token}],
        body    = PayLoad
    },
    {request_or_response(Type), add_trans_params(OptionList, Trans)}.

request_or_response(?ACK) -> response;
request_or_response(_Type)-> request.

add_trans_params([],Trans) -> Trans;
add_trans_params([Option | Options],Trans) ->
    add_trans_params(Options, add_trans_param(Option,Trans)).

%% request
add_trans_param(#option{name = uri_path, value = Value},#coap_trans{path = Path} = Trans)->
    NewPath = list_to_binary([Path, "/", Value]),
    Trans#coap_trans{ path = NewPath};
add_trans_param(#option{name = location_path, value = Value},#coap_trans{path = Path} = Trans)->
    NewPath = list_to_binary([Path, "/", Value]),
    Trans#coap_trans{ path = NewPath};
add_trans_param(#option{name = uri_query, value = Value},#coap_trans{query = Query} = Trans)->
    [QueryKey,QueryValue] = binary:split(Value, [<<"=">>]),
    Trans#coap_trans{query = [{QueryKey, QueryValue} | Query]};
add_trans_param(#option{name = location_query, value = Value},#coap_trans{query = Query} = Trans)->
    [QueryKey,QueryValue] = binary:split(Value, [<<"=">>]),
    Trans#coap_trans{query = [{QueryKey, QueryValue} | Query]};
add_trans_param(#option{name = Name, value = Value},#coap_trans{param = Param} = Trans)->
    Trans#coap_trans{param = [{Name, Value} | Param]}.

%%------------------------------------------------------------------
%% request & response param
%%------------------------------------------------------------------
get_message_id_token(CoAPTrans)->{get_message_id(CoAPTrans),get_token(CoAPTrans)}.

get_message_id(#coap_message{id = MessageID}) -> MessageID;
get_message_id(#coap_trans{param = Param}) ->
    [MessageID] = get_param(Param, messageId, integer,[]),
    MessageID.

get_token(#coap_message{token = Token}) -> Token;
get_token(#coap_trans{param = Param}) ->
    [Token] = get_param(Param, token, binary,[]),
    Token.

-spec get_param(#coap_trans{} | #coap_message{}, atom(), integer | binary) -> list().
get_param(#coap_trans{param = Param} = CoAPTrans, Key, DataType) when is_record(CoAPTrans, coap_trans) ->
    get_param(Param, Key, DataType, []);
get_param(#coap_message{options = Options} = CoAPMessage, Key, DataType) when is_record(CoAPMessage, coap_message)->
    get_param(Options, Key, DataType, []).

get_param([Data | Tail], Key, DataType, Result)->
    case check_data(Data, Key) of
        {ok, Value} -> get_param(Tail, Key, DataType, [trans_data_type(Value, DataType) | Result]);
        error       -> get_param(Tail, Key, DataType, Result)
    end;
get_param([], _Key, _DataType, Result)-> lists:reverse(Result).

check_data({Key, Value}, Key)         -> {ok, Value};
check_data({_OtherKey, _Value}, _Key) -> error;
check_data(#option{name = Key, value = Value}, Key)         -> {ok, Value};
check_data(#option{name = _OtherKey, value = _Value}, _Key) -> error.

trans_data_type(Data, binary) when is_binary(Data)  -> Data;
trans_data_type(Data, binary) when is_integer(Data) -> binary:encode_unsigned(Data);
trans_data_type(Data, integer) when is_integer(Data)-> Data;
trans_data_type(Data, integer) when is_binary(Data) -> binary:decode_unsigned(Data).

%%------------------------------------------------------------------
%%  build function
%%------------------------------------------------------------------

build_option(Data, Value) -> Data#option{value = Value}.

%%------------------------------------------------------------------
%% parse function
%%------------------------------------------------------------------

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

