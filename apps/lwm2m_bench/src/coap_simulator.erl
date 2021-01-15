%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 1月 2021 10:45 上午
%%%-------------------------------------------------------------------
-module(coap_simulator).
-author("DDDHuang").

-behaviour(gen_statem).
-include("coap.hrl").
-include("emqx_lw_tlv.hrl").
-export([start_link/3]).

-export([init/1, terminate/3, code_change/4, callback_mode/0]).

-export([working/3]).

-define(SERVER, ?MODULE).

-record(coap_state, {
    socket,
    host,
    port,
    uri_observe = 0,
    imei,
    messageId,
    token_19_0_0 = un_defined,
    task_list
}).

start_link(Host, Port, IMEI) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Host, Port, IMEI], []).

init([Host, Port, IMEI]) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    {ok, working, #coap_state{socket = Socket, host = Host, port = Port,imei = IMEI},
        [{next_event, internal, continue_workflow}]}.

callback_mode() ->
    state_functions.

working(internal, continue_workflow, #coap_state{imei = IMEI} = State) ->
    CoAPMessage = register(IMEI),
    {next_state, working, send(State, CoAPMessage)};
working(internal, continue_workflow, #coap_state{imei = IMEI} = State) ->
    CoAPMessage = register(IMEI),
    {next_state, working, send(State, CoAPMessage)};
working(info, {udp, _Sock, _PeerIP, _PeerPortNo, Packet}, State) ->
    {ReqOrResp, CoAPMessageTrans} = coap_message_util:decode_trans(Packet),
    NewState = fresh_coap_state(State, CoAPMessageTrans),
    io:format("Receive coap message:~p~n",[CoAPMessageTrans]),
    handle_message(NewState, {ReqOrResp, CoAPMessageTrans}).

terminate(_Reason, _StateName, _State = #coap_state{}) ->
    ok.

code_change(_OldVsn, StateName, State = #coap_state{}, _Extra) ->
    {ok, StateName, State}.


%%--------------------------------------------------------------------------------
%%  fresh message data
%%--------------------------------------------------------------------------------
fresh_coap_state(State, CoAPMessageOrCoAPTrans)->
    NewState = next_message_id(State, CoAPMessageOrCoAPTrans),
    next_uri_observe(NewState, CoAPMessageOrCoAPTrans).

%%--------------------------------------------------------------------------------
%%  fresh message ID
%%--------------------------------------------------------------------------------
next_message_id(State, CoAPMessageOrCoAPTrans)
    when is_record(CoAPMessageOrCoAPTrans, coap_trans) or is_record(CoAPMessageOrCoAPTrans, coap_message) ->
    MessageID = coap_message_util:get_message_id(CoAPMessageOrCoAPTrans), next_message_id(State,MessageID);
next_message_id(State,[MessageID] = MessageIDList) when is_list(MessageIDList)-> next_message_id(State,MessageID);
next_message_id(State,[] = MessageIDList) when is_list(MessageIDList) -> State;
next_message_id(State,#coap_message{id = MessageID} = CoAPMessage) when is_record(CoAPMessage, coap_message) ->
    next_message_id(State,MessageID);
next_message_id(State,MessageID) when is_integer(MessageID) -> State#coap_state{messageId = MessageID+1}.
%%--------------------------------------------------------------------------------
%%  fresh uri observe index
%%--------------------------------------------------------------------------------
next_uri_observe(State, CoAPTrans) when is_record(CoAPTrans, coap_trans) ->
    ObserverIndex = coap_message_util:get_param(CoAPTrans, uri_observe, integer),
    next_uri_observe(State,ObserverIndex);
next_uri_observe(State, CoAPMessage) when is_record(CoAPMessage, coap_message) ->
    ObserverIndex = coap_message_util:get_param(CoAPMessage, uri_observe, integer),
    next_uri_observe(State,ObserverIndex);
next_uri_observe(State, ObserverIndex) when is_integer(ObserverIndex) ->
    State#coap_state{uri_observe = ObserverIndex + 1};
next_uri_observe(State, [ObserverIndex] = ObserverIndexList) when is_list(ObserverIndexList) ->
    next_uri_observe(State,ObserverIndex);
next_uri_observe(State, [] = ObserverIndexList) when is_list(ObserverIndexList) -> State.

%%--------------------------------------------------------------------------------
%%  handle message
%%--------------------------------------------------------------------------------
handle_message(State, {request, #coap_trans{path = <<"/4/0/8">>} = CoAPTrans}) ->
    {MessageID, Token} = coap_message_util:get_message_id_token(CoAPTrans),
    CoAPMessage = response_auto_observe_4_0_8(MessageID, Token),
    {next_state, working, send(State, CoAPMessage)};
handle_message(#coap_state{uri_observe = ObserverIndex}= State,
    {request, #coap_trans{path = <<"/3/0">>} = CoAPTrans}) ->
    {MessageID, Token} = coap_message_util:get_message_id_token(CoAPTrans),
    CoAPMessage = response_auto_observe_3_0(ObserverIndex + 1, MessageID, Token),
    {next_state, working, send(State, CoAPMessage)};
handle_message(#coap_state{uri_observe = ObserverIndex} = State,
    {request, #coap_trans{path = <<"/19/0/0">>} = CoAPTrans}) ->
    {MessageID, Token} = coap_message_util:get_message_id_token(CoAPTrans),
    CoAPMessage = response_auto_observe_19_0_0(ObserverIndex, MessageID, Token),
    {next_state, working, send(State#coap_state{token_19_0_0 = Token}, CoAPMessage)};
handle_message(_State, {request, _CoAPTrans}) ->
    io:format("get a ? request ~n"),
    keep_state_and_data;
handle_message(_State, {response, CoAPTrans}) ->
    io:format("get a resposne ~p~n", [CoAPTrans]),
    keep_state_and_data;
handle_message(_State, UnKnowData) ->
    io:format("unknow data ? ~p~n",[UnKnowData]),
    keep_state_and_data.

%%--------------------------------------------------------------------------------
%%  coap message
%%--------------------------------------------------------------------------------
register(IMEI) ->
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
response_auto_observe_19_0_0(ObserverIndex, MessageID, Token) ->
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

response_auto_observe_3_0(ObserverIndex, MessageID, Token) ->
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

%%--------------------------------------------------------------------------------
%%  udp api
%%--------------------------------------------------------------------------------
%% will fresh message id and uri observe before send.
-spec send(#coap_state{}, #coap_message{}) -> #coap_state{}.
send(#coap_state{socket = Socket,host = Host, port = Port} = State,CoAPMessage) ->
    NewState = fresh_coap_state(State, CoAPMessage),
    io:format("Send Message :~p~n",[CoAPMessage]),
    gen_udp:send(Socket, Host, Port, coap_message_util:encode(CoAPMessage)),
    NewState.
