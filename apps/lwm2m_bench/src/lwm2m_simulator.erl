%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 1月 2021 10:45 上午
%%%-------------------------------------------------------------------
-module(lwm2m_simulator).
-author("DDDHuang").

-behaviour(gen_statem).
-include("coap.hrl").
-include("emqx_lw_tlv.hrl").
-export([start_link/2]).

-export([init/1, terminate/3, code_change/4, callback_mode/0]).

-export([working/3]).

-export([aep_register/1, aep_publish/3]).

-define(SERVER, ?MODULE).

-record(coap_state, {
    socket                      :: reference(),
    host                        :: binary() | tuple,
    port                        :: integer(),
    %% ct iot aep state
    register                    :: unfinished | finished,
    observe_1900                :: unfinished | finished,
    uri_observe_index = 0       :: integer(),
    imei                        :: binary(),
    message_id_index            :: integer(),
    token_19_0_0 = un_defined   :: un_defined | binary()
}).

start_link(Host, Port) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Host, Port], []).

init([Host, Port]) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    {ok, working, #coap_state{socket = Socket, host = Host, port = Port}}.

callback_mode() ->
    state_functions.

working(info, {udp, _Sock, _PeerIP, _PeerPortNo, Packet}, State) ->
    {ReqOrResp, CoAPMessageTrans} = coap_message_util:decode_trans(Packet),
    NewState = fresh_coap_state(State, CoAPMessageTrans),
    io:format("Receive coap message:~p~n",[CoAPMessageTrans]),
    handle_message(NewState, {ReqOrResp, CoAPMessageTrans});
%% ct iot aep action
working(aep, register, #coap_state{imei = IMEI}=State)->
    CoAPMessage = lwm2m_message_tool:message_register(IMEI),
    {next_state, working, send(State, CoAPMessage)};
working(aep, {publish, pass_through, Payload},
    #coap_state{uri_observe_index = ObserverIndex, message_id_index = MessageID, token_19_0_0 = Token}=State)->
    CoAPMessage = lwm2m_message_tool:message_publish_pass_through(MessageID, ObserverIndex, Token, Payload),
    {next_state, working, send(State, CoAPMessage)};
working(aep, {publish, binary, DatasetID, Payload},
    #coap_state{uri_observe_index = ObserverIndex,message_id_index = MessageID,token_19_0_0 = Token}=State)->
    CoAPMessage = lwm2m_message_tool:message_publish_binary(ObserverIndex, MessageID, Token, DatasetID, Payload),
    {next_state, working, send(State, CoAPMessage)};
working(aep, {publish, json, Payload},
    #coap_state{uri_observe_index = ObserverIndex,message_id_index = MessageID,token_19_0_0 = Token}=State)->
    CoAPMessage = lwm2m_message_tool:message_publish_json(ObserverIndex, MessageID, Token, Payload),
    {next_state, working, send(State, CoAPMessage)}.


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
    MessageID = coap_message_util:get_message_id(CoAPMessageOrCoAPTrans),
    next_message_id(State,MessageID);
next_message_id(State,[MessageID] = MessageIDList) when is_list(MessageIDList)-> next_message_id(State,MessageID);
next_message_id(State,[] = MessageIDList) when is_list(MessageIDList) -> State;
next_message_id(State,#coap_message{id = MessageID} = CoAPMessage) when is_record(CoAPMessage, coap_message) ->
    next_message_id(State,MessageID);
next_message_id(State,MessageID) when is_integer(MessageID) -> State#coap_state{message_id_index = MessageID + 1}.
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
    State#coap_state{uri_observe_index = ObserverIndex + 1};
next_uri_observe(State, [ObserverIndex] = ObserverIndexList) when is_list(ObserverIndexList) ->
    next_uri_observe(State,ObserverIndex);
next_uri_observe(State, [] = ObserverIndexList) when is_list(ObserverIndexList) -> State.

%%--------------------------------------------------------------------------------
%%  handle message
%%--------------------------------------------------------------------------------
%%--------------------------------------------------------------------------------
%%  some aep lwm2m function
%%--------------------------------------------------------------------------------

aep_register(Pid) ->
    gen_statem:call(Pid, {aep, register}).
aep_publish(Pid, ProductDataType ,PublishData) ->
    gen_statem:call(Pid, {aep, {publish, ProductDataType, PublishData}}).


%% response
handle_message(_State, {response, _CoAPTrans}) ->
    keep_state_and_data;
%% request
handle_message(State, {request, #coap_trans{path = <<"/4/0/8">>} = CoAPTrans}) ->
    {MessageID, Token} = coap_message_util:get_message_id_token(CoAPTrans),
    CoAPMessage = lwm2m_message_tool:message_response_auto_observe_4_0_8(MessageID, Token),
    {next_state, working, send(State, CoAPMessage)};
handle_message(#coap_state{uri_observe_index = ObserverIndex}= State,
    {request, #coap_trans{path = <<"/3/0">>} = CoAPTrans}) ->
    {MessageID, Token} = coap_message_util:get_message_id_token(CoAPTrans),
    CoAPMessage = lwm2m_message_tool:message_response_auto_observe_3_0(ObserverIndex + 1, MessageID, Token),
    {next_state, working, send(State, CoAPMessage)};
handle_message(#coap_state{uri_observe_index = ObserverIndex} = State,
    {request, #coap_trans{path = <<"/19/0/0">>} = CoAPTrans}) ->
    {MessageID, Token} = coap_message_util:get_message_id_token(CoAPTrans),
    CoAPMessage = lwm2m_message_tool:message_response_auto_observe_19_0_0(ObserverIndex, MessageID, Token),
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
%%  some aep lwm2m function over
%%--------------------------------------------------------------------------------

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
