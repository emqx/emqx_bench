%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 1月 2021 10:29 下午
%%%-------------------------------------------------------------------
-module(lwm2m_message_util).
-author("DDDHuang").
-include("coap.hrl").
-include("emqx_lw_tlv.hrl").
%% message_function(any()) -> #coap_messahe{}
-export([
    message_register/2,
    message_fresh_register/2,
    message_deregister/2]).
-export([
    message_response_auto_observe_4_0_8/2,
    message_response_auto_observe_3_0/3,
    message_response_auto_observe_19_0_0/3]).
-export([message_publish/5]).
-export([
    message_response_command_ack/2,
    message_response_command_binary/5,
    message_response_command_json/4]).

%%-----------------------------------------------------------------
%% message build function
%%-----------------------------------------------------------------
message_register(IMEI, default) ->
    message_register(IMEI, <<"</>;rt=\"oma.lwm2m\";ct=11543,<3/0>,<19/0>">>);
message_register(IMEI, Payload) ->
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
        payload = Payload
    }.

message_fresh_register(IMEI, MessageID) ->
    Options = [
        coap_message_util:build_option(?URI_PATH,  <<"rd">>),
        coap_message_util:build_option(?URI_PATH,  IMEI),
        coap_message_util:build_option(?URI_QUERY, <<"b=U">>),
        coap_message_util:build_option(?URI_QUERY, <<"lt=300">>)
    ],
    #coap_message{
        type    = ?CON,
        method  = ?POST,
        id      = MessageID,
        token   = <<>>,
        options = Options,
        payload = ?NO_PAYLOAD
    }.

message_deregister(IMEI, MessageID)->
    Options = [
        coap_message_util:build_option(?URI_PATH,  <<"rd">>),
        coap_message_util:build_option(?URI_PATH,  IMEI)
    ],
    #coap_message{
        type    = ?CON,
        method  = ?DELETE,
        id      = MessageID,
        token   = <<>>,
        options = Options,
        payload = ?NO_PAYLOAD
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
message_publish(ProductDataType, MessageID, Token, ObserverIndex, Payload)->
    ContentForMate = case ProductDataType of
                         json            -> ?APPLICATION_VNDOMALWM2M_JSON;
                         binary          -> ?APPLICATION_OCTET_STREAM;
                         pass_through    -> ?APPLICATION_OCTET_STREAM
                     end,
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

message_response_command_ack(MessageID, Token)->
    #coap_message{
        type    = ?ACK,
        method  = ?CHANGED,
        id      = MessageID,
        token   = Token,
        options = [],
        payload = ?NO_PAYLOAD
    }.
message_response_command_binary(MessageID, Token, ObserverIndex, DatasetID, Payload)->
    BuildPayload = <<16#86:2, DatasetID:2, (size(Payload)):2, Payload/binary>>,
    message_response_command(MessageID, Token, ObserverIndex, BuildPayload).
message_response_command_json(MessageID, Token, ObserverIndex, Payload)->
    message_response_command(MessageID, Token, ObserverIndex, Payload).
message_response_command(MessageID, Token, ObserverIndex, Payload)->
    Options = [
        coap_message_util:build_option(?URI_OBSERVE, <<ObserverIndex:8>>),
        coap_message_util:build_option(?URI_PATH, <<"19">>),
        coap_message_util:build_option(?URI_PATH, <<"0">>),
        coap_message_util:build_option(?URI_PATH, <<"0">>)
    ],
    #coap_message{
        type    = ?CON,
        method  = ?CONTENT,
        id      = MessageID,
        token   = Token,
        options = Options,
        payload = Payload
    }.
