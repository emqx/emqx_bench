%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 1月 2021 10:29 下午
%%%-------------------------------------------------------------------
-module(lwm2m_message_tool).
-author("DDDHuang").
-include("coap.hrl").
-include("emqx_lw_tlv.hrl").
%% API
-export([message_register/1]).
-export([message_response_auto_observe_4_0_8/2, message_response_auto_observe_3_0/3, message_response_auto_observe_19_0_0/3]).
-export([message_publish_pass_through/4, message_publish_binary/5, message_publish_json/4]).

message_register(IMEI) ->
    Options = [
        coap_message_util:build_option(?URI_PATH,  <<"rd">>),
        coap_message_util:build_option(?URI_QUERY, <<"lwm2m=1.0">>),
        coap_message_util:build_option(?URI_QUERY, <<"ep=", IMEI/binary>>),
        coap_message_util:build_option(?URI_QUERY, <<"b=U">>),
        coap_message_util:build_option(?URI_QUERY, <<"lt=300">>)
    ],
    #coap_message{
        type    = ?CON,
        method  = ?POST,
        id      = 0,
        token   = <<>>,
        options = Options,
        payload = <<"</>;rt=\"oma.lwm2m\";ct=11543,<3/0>,<19/0>">>
    }.

message_response_auto_observe_4_0_8(MessageID, Token) ->
    Options = [
        coap_message_util:build_option(?CONTENT_FORMAT, ?TEXT_PLAIN)
    ],
    #coap_message{
        type    = ?ACK,
        method  = ?CONTENT,
        id      = MessageID,
        token   = Token,
        options = Options,
        payload = ?NO_PAYLOAD
    }.
message_response_auto_observe_19_0_0(ObserverIndex, MessageID, Token) ->
    Options = [
        coap_message_util:build_option(?URI_OBSERVE, <<ObserverIndex:8>>),
        coap_message_util:build_option(?CONTENT_FORMAT, ?APPLICATION_OCTET_STREAM)
    ],
    #coap_message{
        type    = ?ACK,
        method  = ?CONTENT,
        id      = MessageID,
        token   = Token,
        options = Options,
        payload = ?NO_PAYLOAD
    }.

message_response_auto_observe_3_0(ObserverIndex, MessageID, Token) ->
    Options = [
        coap_message_util:build_option(?URI_OBSERVE, <<ObserverIndex:8>>),
        coap_message_util:build_option(?CONTENT_FORMAT, ?APPLICATION_VNDOMALWM2M_TLV)
    ],
    TLVList = [
        #tlv{type = ?VALUE, identifier = 16#0, value = <<"emqx">>},
        #tlv{type = ?VALUE, identifier = 16#1, value = <<"Lightweight M2M Client">>},
        #tlv{type = ?VALUE, identifier = 16#2, value = <<"20210107">>},
        #tlv{type = ?VALUE, identifier = 16#3, value = <<"1.0">>},
        #tlv{type = ?MULTIPLE, identifier = 16#6, value = [
            #tlv{type = ?RESOURCE,identifier = 16#0,value = binary:encode_unsigned(16#1)},
            #tlv{type = ?RESOURCE,identifier = 16#1,value = binary:encode_unsigned(16#5)}]},
        #tlv{type = ?MULTIPLE, identifier = 16#7, value = [
            #tlv{type = ?RESOURCE,identifier = 16#0,value = binary:encode_unsigned(16#0ED8)},
            #tlv{type = ?RESOURCE,identifier = 16#1,value = binary:encode_unsigned(16#1388)}]},
        #tlv{type = ?MULTIPLE, identifier = 16#8, value = [
            #tlv{type = ?RESOURCE,identifier = 16#0,value = binary:encode_unsigned(16#7D)},
            #tlv{type = ?RESOURCE,identifier = 16#1,value = binary:encode_unsigned(16#0384)}]},
        #tlv{type = ?VALUE, identifier = 16#9, value = binary:encode_unsigned(16#64)},
        #tlv{type = ?VALUE, identifier = 16#0A, value = binary:encode_unsigned(16#0F)},
        #tlv{type = ?MULTIPLE, identifier = 16#0B, value = [
            #tlv{type = ?RESOURCE,identifier = 16#0,value = binary:encode_unsigned(16#0)}]},
        #tlv{type = ?VALUE, identifier = 16#0D, value = binary:encode_unsigned(16#5182428F)},
        #tlv{type = ?VALUE, identifier = 16#0E, value = <<"+08:00">>},
        #tlv{type = ?VALUE, identifier = 16#10, value = <<"U">>}
    ],
    Payload = lw_tlv_util:encode(TLVList),
    #coap_message{
        type    = ?ACK,
        method  = ?CONTENT,
        id      = MessageID,
        token   = Token,
        options = Options,
        payload = Payload
    }.

message_publish_pass_through(MessageID, ObserverIndex, Token, Payload) ->
    message_publish(MessageID, Token, ObserverIndex, ?APPLICATION_OCTET_STREAM, Payload).

message_publish_binary(ObserverIndex, MessageID, Token, DatasetID, Payload) ->
    BuildPayload = <<2:2, DatasetID:2, (size(Payload)):2, Payload/binary>>,
    message_publish(MessageID, Token, ObserverIndex, ?APPLICATION_OCTET_STREAM, BuildPayload).

message_publish_json(ObserverIndex, MessageID, Token, Payload) ->
    message_publish(MessageID, Token, ObserverIndex, ?APPLICATION_VNDOMALWM2M_JSON, Payload).

message_publish(MessageID, Token, ObserverIndex, ContentForMate, Payload) ->
    Options = [
        coap_message_util:build_option(?URI_OBSERVE, <<ObserverIndex:8>>),
        coap_message_util:build_option(?URI_PATH, <<"19">>),
        coap_message_util:build_option(?URI_PATH, <<"0">>),
        coap_message_util:build_option(?URI_PATH, <<"0">>),
        coap_message_util:build_option(?CONTENT_FORMAT, ContentForMate)
    ],
    #coap_message{
        type    = ?CON,
        method  = ?CONTENT,
        id      = MessageID,
        token   = Token,
        options = Options,
        payload = Payload
    }.
