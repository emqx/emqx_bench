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

-export([start_link/3]).
-export([init/1, terminate/3, code_change/4, callback_mode/0]).

-export([working/3]).
-export([register/1, register/2, de_register/1, fresh_register/1, publish/3]).

-define(SERVER, ?MODULE).

-record(coap_state, {
    socket                              :: gen_udp:socket(),
    host                                :: binary() | tuple | inet:ip_address(),
    port                                :: integer(),
    imei                                :: binary(),
    register                            :: unfinished | finished,
    observe_1900                        :: unfinished | finished,
    uri_observe_index   = 0             :: integer(),
    message_id_index    = 0             :: integer(),
    token_19_0_0        = un_defined    :: un_defined | binary()
}).

start_link(IMEI, Host, Port) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [IMEI, Host, Port], []).

init([IMEI, Host, Port]) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    {ok, working, #coap_state{imei = IMEI, socket = Socket, host = Host, port = Port}}.

callback_mode() ->
    state_functions.

working(info, {udp, _Sock, _PeerIP, _PeerPortNo, Packet}, State) ->
    CoapMessage = coap_message_util:decode(Packet),
    %% fresh new message id and if message has uri observe
    NewState = fresh_coap_state(CoapMessage, State),
    io:format("Receive coap message:~p~n", [CoapMessage]),
    handle_message(CoapMessage, NewState);

working({call, From}, Command, State) ->
    {keep_state, execute(Command, State), {reply, From, ok}};
working(Any, Data, State) ->
    io:format("====> event type ~p~n, Data ~p~n,State ~p~n ", [Any, Data, State]),
    keep_state_and_data.

terminate(_Reason, _StateName, _State = #coap_state{}) -> ok.
code_change(_OldVsn, StateName, State = #coap_state{}, _Extra) -> {ok, StateName, State}.

%% RegisterPayload should be like <<"</>;rt=\"oma.lwm2m\";ct=11543,<3/0>,<19/0>">>
execute({register, RegisterPayload}, #coap_state{imei = IMEI} = State) ->
    send(lwm2m_message_util:message_register(IMEI, RegisterPayload), State);
execute({fresh_register}, #coap_state{imei = IMEI, message_id_index = MessageID} = State) ->
    send(lwm2m_message_util:message_fresh_register(IMEI, MessageID), State);
execute({de_register}, #coap_state{imei = IMEI, message_id_index = MessageID} = State) ->
    send(lwm2m_message_util:message_deregister(IMEI, MessageID), State);
execute({publish, ProductDataType, Payload},
    #coap_state{message_id_index = MessageID, uri_observe_index = ObserverIndex, token_19_0_0 = Token} = State) ->
    send(lwm2m_message_util:message_publish(ProductDataType, MessageID, Token, ObserverIndex, Payload), State).


-spec register(pid()) -> any().
register(Pid) ->
    gen_statem:call(Pid, {register, default}).

-spec register(pid(), binary()) -> any().
register(Pid, RegisterPayload) ->
    gen_statem:call(Pid, {register, RegisterPayload}).

-spec fresh_register(pid()) -> any().
fresh_register(Pid) ->
    gen_statem:call(Pid, {fresh_register}).

-spec de_register(pid()) -> any().
de_register(Pid) ->
    gen_statem:call(Pid, {de_register}).
%%   binary payload, BuildPayload = <<16#02:2, DatasetID:2, (size(Payload)):2, Payload/binary>>,
-spec publish(pid(), json | binary | pass_through, binary()) -> any().
publish(Pid, ProductDataType, PublishData) ->
    gen_statem:call(Pid, {publish, ProductDataType, PublishData}).

%%--------------------------------------------------------------------------------
%%  fresh message data
%%  fresh message ,then fresh uri observe (if have)
%%--------------------------------------------------------------------------------
fresh_coap_state(CoAPMessage, State) ->
    NewState = next_message_id(CoAPMessage, State),
    next_uri_observe(CoAPMessage, NewState).
%%--------------------------------------------------------------------------------
%%  fresh message ID
%%  new message id >= now ,use new message id +1
%%  else, use message id now +1
%%--------------------------------------------------------------------------------
next_message_id(#coap_message{id = MessageID} = CoAPMessage, State) when is_record(CoAPMessage, coap_message) ->
    next_message_id(MessageID, State);
next_message_id(MessageID, #coap_state{message_id_index = IndexNow} = State) when IndexNow >= MessageID ->
    State#coap_state{message_id_index = IndexNow + 1};
next_message_id(MessageID, State) -> State#coap_state{message_id_index = MessageID + 1}.
%%--------------------------------------------------------------------------------
%%  fresh uri observe index
%%--------------------------------------------------------------------------------
next_uri_observe(CoAPMessage, State) when is_record(CoAPMessage, coap_message) ->
    ObserverIndex = coap_message_util:get_param(CoAPMessage, uri_observe, integer),
    %% coap_message_util:get_param/3 return type is list()
    next_uri_observe(ObserverIndex, State);
next_uri_observe([ObserverIndex] = ObserverIndexList, State) when is_list(ObserverIndexList) ->
    next_uri_observe(ObserverIndex, State);
next_uri_observe(ObserverIndex, State) when is_integer(ObserverIndex) ->
    State#coap_state{uri_observe_index = ObserverIndex + 1};
%% if no uri observe
next_uri_observe([] = ObserverIndexList, State) when is_list(ObserverIndexList) -> State.

%%--------------------------------------------------------------------------------
%%  handle message
%%--------------------------------------------------------------------------------
%%--------------------------------------------------------------------------------
handle_message(#coap_message{type = Type} = CoapMessage, State) ->
    case Type of
        ?ACK -> handle_ack(CoapMessage, State);
        ?CON -> handle_con(CoapMessage, State);
        ?NON -> handle_non(CoapMessage, State);
        ?RESET -> handle_reset(CoapMessage, State)
    end.

handle_ack(#coap_message{} = _CoapMessage, _State) ->
    keep_state_and_data.
handle_con(#coap_message{method = Method} = CoapMessage, State) ->
    case Method of
        ?EMPTY  -> keep_state_and_data;
        ?GET    -> handle_get(CoapMessage, State);
        ?PUT    -> handle_put(CoapMessage,State);
        ?POST   -> handle_post(CoapMessage, State);
        ?DELETE -> handle_delete(CoapMessage, State)
    end.
handle_non(#coap_message{} = _CoapMessage, _State) ->
    keep_state_and_data.
handle_reset(#coap_message{} = _CoapMessage, _State) ->
    keep_state_and_data.

handle_get(#coap_message{id = MessageID, token = Token} = CoapMessage, #coap_state{uri_observe_index = UriObserveIndex} = State) ->
    Path = coap_message_util:get_uri_path(CoapMessage),
    case Path of
        <<"/3/0">> ->
            {keep_state, send(lwm2m_message_util:message_response_auto_observe_4_0_8(MessageID, Token), State)};
        <<"/19/0/0">> ->
            {keep_state,
                send(lwm2m_message_util:message_response_auto_observe_19_0_0(UriObserveIndex, MessageID, Token), State#coap_state{token_19_0_0 = Token})};
        <<"/4/0/8">> ->
            {keep_state, send(lwm2m_message_util:message_response_auto_observe_4_0_8(MessageID, Token), State)};
        _ -> keep_state_and_data
    end.
handle_post(#coap_message{} = _CoapMessage, _State) ->
    keep_state_and_data.
handle_put(#coap_message{} = _CoapMessage, _State) ->
    keep_state_and_data.
handle_delete(#coap_message{} = _CoapMessage, _State) ->
    keep_state_and_data.
%%--------------------------------------------------------------------------------
%%  some lwm2m function over
%%--------------------------------------------------------------------------------

%%--------------------------------------------------------------------------------
%%  udp api
%%--------------------------------------------------------------------------------
%% will fresh message id and uri observe before send.
-spec send(#coap_message{}, #coap_state{}) -> #coap_state{}.
send(CoAPMessage, #coap_state{socket = Socket, host = Host, port = Port} = State) ->
    gen_udp:send(Socket, Host, Port, coap_message_util:encode(CoAPMessage)),
    io:format("Send Message :~p~n", [CoAPMessage]),
    fresh_coap_state(CoAPMessage, State).
