-module(coap_bench_message).

-export([make_register/4
    , make_deregister/2
    , make_notify/5
    , make_notify/6
    , ack_validator/2
    , location_path/1
    , make_ack/4
    , make_empty_ack/1
    , uri_path/1
    , id/1
    , token/1
    , type/1
    , incr_counter_sent/1
    , incr_counter_send_fail/1
    , incr_counter_rcvd/1
    , coap_code/1
    , find_option_value/2,
    make_response_observe_3_0/3, make_response_observe_19_0_0/3, make_notify/4, method/1]).

-include_lib("lwm2m_coap/include/coap.hrl").

-include_lib("tlv.hrl").


make_register(EndpointName, Lifetime, MsgId, ObjectLinks) ->
    Lt = bin(Lifetime),
    Query = [<<"b=U">>, <<"ep=", EndpointName/binary>>, <<"lt=", Lt/binary>>, <<"lwm2m=1.0">>],
    coap_message(con, post, ObjectLinks, MsgId, crypto:strong_rand_bytes(4),
        [{uri_path, [<<"rd">>]}, {uri_query, Query}, {content_format, <<"application/link-format">>}]).

%% response to observe 3/0
make_response_observe_3_0(MessageID, Token, UriObserveIndex) ->
    TlvBytes_0 = tlv_tool:encode(#tlv{tag = ?T_Resources_Value, id = 0, value = <<"emqx">>}),
    TlvBytes_1 = tlv_tool:encode(#tlv{tag = ?T_Resources_Value, id = 1, value = <<"lwm2m client">>}),
    TlvBytes_2 = tlv_tool:encode(#tlv{tag = ?T_Resources_Value, id = 2, value = <<"20200824">>}),
    TlvBytes_3 = tlv_tool:encode(#tlv{tag = ?T_Resources_Value, id = 3, value = <<"1.0.0">>}),
    TlvBytes = <<TlvBytes_0/bytes, TlvBytes_1/bytes, TlvBytes_2/bytes, TlvBytes_3/bytes>>,
    coap_message(ack, {ok, content}, TlvBytes, MessageID, Token, [{observe, UriObserveIndex}, {content_format, 11542}]).

make_response_observe_19_0_0(MessageID, Token, UriObserveIndex) ->
    coap_message(ack, {ok, content}, <<>>, MessageID, Token, [{observe, UriObserveIndex}, {content_format, 42}]).
%%  #coap_message{
%%    type = ack,
%%    method = content,
%%    id = MessageID,
%%    token = Token,
%%    payload = TlvBytes,
%%    options = [{observe, UriObserveIndex}, {content_format, 11542}]
%%  }.

make_deregister(Location, MsgId) when is_list(Location) ->
    coap_message(con, delete, <<>>, MsgId, crypto:strong_rand_bytes(4),
        [{uri_path, Location}]).

make_notify(MessageId, Token, ObserveIndex, PayLoad) ->
    #coap_message{
        type = non,
        method = {ok, content},
        id = MessageId,
        token = Token,
        options = [{observe, ObserveIndex}, {uri_path, [<<"19">>, <<"0">>, <<"0">>]}],
        payload = PayLoad
    }.

make_notify(Type, Token, MsgId, Body, Observe, ContentFormat) ->
    make_notify(Type, Token, MsgId, Body, [{observe, Observe}, {content_format, ContentFormat}]).

make_notify(Type, Token, MsgId, Body, Options) when is_list(Options) ->
    coap_message(Type, {ok, content}, Body, MsgId, Token, Options).

coap_message(Type, Method, Payload, MsgId, Token, Options) ->
    #coap_message{
        type = Type,
        method = Method,
        id = MsgId,
        token = Token,
        payload = Payload,
        options = Options
    }.

make_empty_ack(#coap_message{id = MsgId}) ->
    #coap_message{
        type = ack,
        id = MsgId
    }.

make_ack(#coap_message{id = MsgId, token = Token}, Code, Payload, Options) ->
    #coap_message{
        type = ack, id = MsgId,
        token = Token,
        method = Code,
        payload = Payload,
        options = Options
    }.

ack_validator(#coap_message{type = ack, id = MsgId}, MsgId) -> true;
ack_validator(_, _) -> false.

method(#coap_message{method = Method}) -> Method.

id(#coap_message{id = Id}) -> Id.

find_option_value(Key, #coap_message{options = Options}) ->
    proplists:get_value(Key, Options, []).

location_path(#coap_message{options = Options}) ->
    proplists:get_value(location_path, Options, []);
location_path(_) -> [].

uri_path(#coap_message{options = Options}) ->
    proplists:get_value(uri_path, Options, []);
uri_path(_) -> [].

token(#coap_message{token = Token}) ->
    Token;
token(_) -> undefined.

type(#coap_message{type = Type}) ->
    Type;
type(_) -> undefined.

incr_counter_sent(#coap_message{type = con}) ->
    coap_bench_metrics:incr('CON_SENT');
incr_counter_sent(#coap_message{type = non}) ->
    coap_bench_metrics:incr('NON_SENT');
incr_counter_sent(#coap_message{type = reset}) ->
    coap_bench_metrics:incr('RST_SENT');
incr_counter_sent(#coap_message{type = ack}) ->
    coap_bench_metrics:incr('ACK_SENT').

incr_counter_send_fail(#coap_message{type = con}) ->
    coap_bench_metrics:incr('CON_SEND_FAIL');
incr_counter_send_fail(#coap_message{type = non}) ->
    coap_bench_metrics:incr('NON_SEND_FAIL');
incr_counter_send_fail(#coap_message{type = reset}) ->
    coap_bench_metrics:incr('RST_SEND_FAIL');
incr_counter_send_fail(#coap_message{type = ack}) ->
    coap_bench_metrics:incr('ACK_SEND_FAIL').

incr_counter_rcvd(#coap_message{type = con}) ->
    coap_bench_metrics:incr('CON_RCVD');
incr_counter_rcvd(#coap_message{type = non}) ->
    coap_bench_metrics:incr('NON_RCVD');
incr_counter_rcvd(#coap_message{type = reset}) ->
    coap_bench_metrics:incr('RST_RCVD');
incr_counter_rcvd(#coap_message{type = ack}) ->
    coap_bench_metrics:incr('ACK_RCVD').

bin(Int) when is_integer(Int) ->
    integer_to_binary(Int);
bin(Bin) when is_binary(Bin) ->
    Bin.

coap_code(<<"created">>) -> {ok, created};
coap_code(<<"deleted">>) -> {ok, deleted};
coap_code(<<"valid">>) -> {ok, valid};
coap_code(<<"changed">>) -> {ok, changed};
coap_code(<<"content">>) -> {ok, content};
coap_code(<<"continue">>) -> {ok, continue};
coap_code(<<"bad_request">>) -> {error, bad_request};
coap_code(<<"unauthorized">>) -> {error, uauthorized};
coap_code(<<"forbidden">>) -> {error, forbidden};
coap_code(<<"not_found">>) -> {error, not_found};
coap_code(<<"method_not_allowed">>) -> {error, method_not_allowed};
coap_code(<<"request_entity_incomplete">>) -> {error, request_entity_incomplete};
coap_code(<<"precondition_failed">>) -> {error, precondition_failed};
coap_code(<<"request_entity_too_large">>) -> {error, request_entity_too_large};
coap_code(<<"unsupported_content_format">>) -> {error, unsupported_content_format};
coap_code(<<"internal_server_error">>) -> {error, internal_server_error};
coap_code(<<"not_implemented">>) -> {error, not_implemented};
coap_code(<<"bad_gateway">>) -> {error, bad_gateway};
coap_code(<<"service_unavailable">>) -> {error, service_unavailable};
coap_code(<<"gateway_timeout">>) -> {error, gateway_timeout};
coap_code(<<"proxying_not_supported">>) -> {error, proxying_not_supported}.
