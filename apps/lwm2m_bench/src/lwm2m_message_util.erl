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
-include("tlv.hrl").
%% function(any()) -> #coap_messahe{}
-export([   bootstrap/2,
            bootstrap_sm9/3]).

-export([   register/3,
            register_static_module/3,
            fresh_register/2,
            deregister/2]).

-export([   response_auto_observe_4_0_8/2,
            response_auto_observe_3_0/2,
            response_auto_observe_19_0_0/2]).

-export([   publish/4]).

-export([   response_command_ack/2,
            response_command_binary/4,
            response_command_json/3]).

%%-----------------------------------------------------------------
%% message build function
%%-----------------------------------------------------------------

bootstrap(MessageID, IMEI)->
    Options = [
        coap_message_util:build_option(?URI_PATH,  <<"bs">>),
        coap_message_util:build_option(?URI_QUERY, <<"ep=", IMEI/binary>>)
    ],
    #coap_message{
        type    = ?CON,
        method  = ?POST,
        id      = MessageID,
        token   = <<>>,
        options = Options
    }.
bootstrap_sm9(MessageID, IMEI, PubKey)->
    Options = [
        coap_message_util:build_option(?URI_PATH,  <<"bs">>),
        coap_message_util:build_option(?URI_QUERY, <<"ktype=2">>),
        coap_message_util:build_option(?URI_QUERY, <<"ep=", IMEI/binary>>),
        coap_message_util:build_option(?URI_QUERY, <<"epub=" , PubKey/binary>>)
    ],
    #coap_message{
        type    = ?CON,
        method  = ?POST,
        id      = MessageID,
        token   = <<>>,
        options = Options
    }.

register(MessageID, IMEI, default) ->
    register(MessageID,IMEI, <<"</>;rt=\"oma.lwm2m\";ct=11543,<3/0>,<19/0>">>);
register(MessageID, IMEI, Payload) ->
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
        id      = MessageID,
        token   = <<>>,
        options = Options,
        payload = Payload
    }.
register_static_module(MessageID, IMEI, default) ->
    register_static_module(MessageID,IMEI, <<"</>;rt=\"oma.lwm2m\";ct=11543,<3/0>,<19/0>">>);
register_static_module(MessageID, IMEI, Payload) ->
    Options = [
        coap_message_util:build_option(?URI_PATH,  <<"rd">>),
        coap_message_util:build_option(?URI_QUERY, <<"lwm2m=1.0">>),
        coap_message_util:build_option(?URI_QUERY, <<"ep=", IMEI/binary>>),
        coap_message_util:build_option(?URI_QUERY, <<"b=U">>),
        coap_message_util:build_option(?URI_QUERY, <<"lt=300">>),
        coap_message_util:build_option(?URI_QUERY, <<"ctapn=Psm0.eDRX0.ctnb">>),
        coap_message_util:build_option(?URI_QUERY, <<"ctm2m=1.0">>),
        coap_message_util:build_option(?URI_QUERY, <<"imsi=e_imsi">>),
        coap_message_util:build_option(?URI_QUERY, <<"iccid=e_iccid">>),
        coap_message_util:build_option(?URI_QUERY, <<"sv=e_sv">>),
        coap_message_util:build_option(?URI_QUERY, <<"chip=e_chip">>),
        coap_message_util:build_option(?URI_QUERY, <<"module=e_module">>)
    ],
    #coap_message{
        type    = ?CON,
        method  = ?POST,
        id      = MessageID,
        token   = <<>>,
        options = Options,
        payload = Payload
    }.

fresh_register(MessageID, IMEI) ->
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

deregister(MessageID, IMEI)->
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


response_auto_observe_4_0_8(MessageID, Token) ->
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
response_auto_observe_19_0_0(MessageID, Token) ->
    Options = [
        coap_message_util:build_option(?URI_OBSERVE, <<0:8>>),
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

response_auto_observe_3_0(MessageID, Token) ->
    Options = [
        coap_message_util:build_option(?URI_OBSERVE, <<0:8>>),
        coap_message_util:build_option(?CONTENT_FORMAT, ?APPLICATION_VNDOMALWM2M_TLV)
    ],
    TLVList = [
        #tlv{type = ?VALUE, identifier = 16#0, value = <<"emqx">>},
        #tlv{type = ?VALUE, identifier = 16#1, value = <<"Lightweight M2M Client">>},
        #tlv{type = ?VALUE, identifier = 16#2, value = <<"20210107">>},
        #tlv{type = ?VALUE, identifier = 16#3, value = <<"1.0">>},
        #tlv{type = ?MULTIPLE, identifier = 16#6, value = [
            #tlv{type = ?RESOURCE, identifier = 16#0, value = binary:encode_unsigned(16#1)},
            #tlv{type = ?RESOURCE, identifier = 16#1, value = binary:encode_unsigned(16#5)}]},
        #tlv{type = ?MULTIPLE, identifier = 16#7, value = [
            #tlv{type = ?RESOURCE, identifier = 16#0, value = binary:encode_unsigned(16#0ED8)},
            #tlv{type = ?RESOURCE, identifier = 16#1, value = binary:encode_unsigned(16#1388)}]},
        #tlv{type = ?MULTIPLE, identifier = 16#8, value = [
            #tlv{type = ?RESOURCE, identifier = 16#0, value = binary:encode_unsigned(16#7D)},
            #tlv{type = ?RESOURCE, identifier = 16#1, value = binary:encode_unsigned(16#0384)}]},
        #tlv{type = ?VALUE, identifier = 16#9, value = binary:encode_unsigned(16#64)},
        #tlv{type = ?VALUE, identifier = 16#0A, value = binary:encode_unsigned(16#0F)},
        #tlv{type = ?MULTIPLE, identifier = 16#0B, value = [
            #tlv{type = ?RESOURCE, identifier = 16#0, value = binary:encode_unsigned(16#0)}]},
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
publish(ProductDataType, MessageID, Token, Payload)->
    ContentForMate = case ProductDataType of
        json            -> ?APPLICATION_VNDOMALWM2M_JSON;
        binary          -> ?APPLICATION_OCTET_STREAM;
        pass_through    -> ?APPLICATION_OCTET_STREAM
    end,
    Options = [
        coap_message_util:build_option(?URI_OBSERVE, <<0:8>>),
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

response_command_ack(MessageID, Token)->
    #coap_message{
        type    = ?ACK,
        method  = ?CHANGED,
        id      = MessageID,
        token   = Token,
        options = [],
        payload = ?NO_PAYLOAD
    }.
response_command_binary(MessageID, Token, DatasetID, Payload)->
    BuildPayload = <<16#86:2, DatasetID:2, (size(Payload)):2, Payload/binary>>,
    response_command(MessageID, Token, BuildPayload).
response_command_json(MessageID, Token, Payload)->
    response_command(MessageID, Token, Payload).
response_command(MessageID, Token, Payload)->
    Options = [
        coap_message_util:build_option(?URI_OBSERVE, <<0:8>>),
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
