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

-export([start_link/1]).
-export([init/1, terminate/3, code_change/4, callback_mode/0]).

-export([working/3, wait_response/3]).
-export([register/1, register/2, de_register/1, fresh_register/1, publish/3]).

-define(SERVER, ?MODULE).

-define(ACK_OR_DIE, ack_or_die).
-define(SIMPLE_CON, simple_con).

-record(coap_state, {
    socket                              :: gen_udp:socket(),
    host                                :: binary() | tuple | inet:ip_address(),
    port                                :: integer(),
    sampler                             :: function(),
    sampler_arg                         :: any(),
    imei                                :: binary(),
    observe_1900                        :: unfinished | finished,
    message_id_index    = 0             :: integer(),
    token_19_0_0        = un_defined    :: un_defined | binary()
}).

start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, []).

init(Args) ->
    {ok, working, do_init(Args, #coap_state{})}.

do_init([{imei, IMEI}, Args], State) -> do_init(Args, State#coap_state{imei = IMEI});
do_init([{host, Host}, Args], State) -> do_init(Args, State#coap_state{host = Host});
do_init([{port, Port}, Args], State) -> do_init(Args, State#coap_state{port = Port});
do_init([{_, _}, Args], State)       -> do_init(Args, State);
do_init([], State) ->
%%   {ok, Sock} = gen_udp:open(0, [{ip, {192,168,1,120}}, binary, {active, false}, {reuseaddr, false}]),
    {ok, Socket} = gen_udp:open(0, [binary]),
    State#coap_state{socket = Socket}.



callback_mode() ->
    state_functions.

working(info, {udp, _Sock, _PeerIP, _PeerPortNo, Packet}, State) ->
    CoAPMessage = coap_message_util:decode(Packet),
    %% fresh new message id and if message has uri observe
    NewState = fresh_coap_state(CoAPMessage, State),
    io:format("Receive coap message:~p~n", [CoAPMessage]),
    handle_message(CoAPMessage, NewState);

working(cast, Command, State) ->
    execute(Command, State);
working(Any, Data, State) ->
    io:format("working，event type ~p~n, Data ~p~n,State ~p~n ", [Any, Data, State]),
    keep_state_and_data.

%% RequestStyle :: ack_or_die | simple_con
%% ack_or_die -> timeout -> shutdown
%% simple_con -> timeout -> ignore
wait_response(state_timeout, {RequestStyle, AckTimeout, LastTimes, CoAPMessage}, State) when LastTimes > 1 ->
    {keep_state, send(CoAPMessage, State),
        [{state_timeout, AckTimeout, {RequestStyle, AckTimeout * (?MAX_RETRANSMIT - LastTimes + 1), LastTimes - 1, CoAPMessage}}]};
wait_response(state_timeout, {?ACK_OR_DIE, _AckTimeout, LastTimes, CoAPMessage}, State) when LastTimes =:= 1 ->
    {stop, {shutdown, {ack_time_out, CoAPMessage}}, State};
wait_response(state_timeout, {?SIMPLE_CON, _AckTimeout, LastTimes, _CoAPMessage}, _State) when LastTimes =:= 1 ->
    keep_state_and_data;
wait_response(info, {udp, _Sock, _PeerIP, _PeerPortNo, Packet}, #coap_state{sampler = Sampler,sampler_arg = SamplerArg} = State)->
    CoAPMessage = coap_message_util:decode(Packet),
    io:format("waitting response ~p~n",[CoAPMessage]),
    case Sampler(CoAPMessage, SamplerArg) of
        ok              -> {next_state, working, fresh_coap_state(CoAPMessage, State),[{timeout, cancel}]};
        {error, Reason} -> {stop, {shutdown, {action_failed , Reason}}, State};
        ignore          -> {keep_state, fresh_coap_state(CoAPMessage, State)}
    end;
wait_response(Any, Data, State) ->
    io:format("wait message, event type ~p~n, Data ~p~n,State ~p~n ", [Any, Data, State]),
    keep_state_and_data.

terminate(_Reason, _StateName, _State = #coap_state{socket = Socket}) ->
    gen_udp:close(Socket).
code_change(_OldVsn, StateName, State = #coap_state{}, _Extra) -> {ok, StateName, State}.

%%--------------------------------------------------------------------------------
%% execute function
%%--------------------------------------------------------------------------------
add_sampler_arg(State, Sampler, SamplerArgs)->
    State#coap_state{sampler = Sampler,sampler_arg = SamplerArgs}.
execute({register, RegisterPayload}, #coap_state{message_id_index = MessageID, imei = IMEI} = State) ->
    %% RegisterPayload should be like <<"</>;rt=\"oma.lwm2m\";ct=11543,<3/0>,<19/0>">>
    Sampler = fun
                  (#coap_message{type = ?ACK, method = ?CREATED, id = AckMessageID}, AckMessageID) -> ok;
                  (#coap_message{type = ?ACK, method = Method,   id = AckMessageID}, AckMessageID) -> {fail, Method};
                  (_CoAPMessage, _MessageID) -> ignore
              end,
    CoAPMessage = lwm2m_message_util:register(MessageID, IMEI, RegisterPayload),
    send_request(?ACK_OR_DIE, CoAPMessage, add_sampler_arg(State, Sampler, MessageID));
execute({register_standard_module, RegisterPayload}, #coap_state{message_id_index = MessageID, imei = IMEI} = State) ->
    %% RegisterPayload should be like <<"</>;rt=\"oma.lwm2m\";ct=11543,<3/0>,<19/0>">>
    Sampler = fun
                  (#coap_message{type = ?ACK, method = ?CREATED, id = AckMessageID}, AckMessageID) -> ok;
                  (#coap_message{type = ?ACK, method = Method,   id = AckMessageID}, AckMessageID) -> {error, Method};
                  (_CoAPMessage, _MessageID) -> ignore
              end,
    CoAPMessage = lwm2m_message_util:register_standard_module(MessageID, IMEI, RegisterPayload),
    send_request(?ACK_OR_DIE, CoAPMessage, add_sampler_arg(State, Sampler, MessageID));
execute({fresh_register}, #coap_state{imei = IMEI, message_id_index = MessageID} = State) ->
    Sampler = fun
                  (#coap_message{type = ?ACK, method = ?CHANGED, id = AckMessageID}, AckMessageID) -> ok;
                  (#coap_message{type = ?ACK, method = Method,   id = AckMessageID}, AckMessageID) -> {error, Method};
                  (_CoAPMessage, _MessageID) -> ignore
              end,
    CoAPMessage = lwm2m_message_util:fresh_register(MessageID, IMEI),
    send_request(?ACK_OR_DIE, CoAPMessage, add_sampler_arg(State, Sampler, MessageID));
execute({de_register}, #coap_state{imei = IMEI, message_id_index = MessageID} = State) ->
    Sampler = fun
                  (#coap_message{type = ?ACK, method = ?DELETED, id = AckMessageID}, AckMessageID) -> ok;
                  (#coap_message{type = ?ACK, method = Method,   id = AckMessageID}, AckMessageID) -> {error, Method};
                  (_CoAPMessage, _MessageID) -> ignore
              end,
    CoAPMessage = lwm2m_message_util:fresh_register(MessageID, IMEI),
    send_request(?ACK_OR_DIE, CoAPMessage, add_sampler_arg(State, Sampler, MessageID));
execute({publish, ProductDataType, Payload},
    #coap_state{message_id_index = MessageID, token_19_0_0 = Token} = State) ->
    Sampler = fun
                  (#coap_message{type = ?ACK, id = AckMessageID}, AckMessageID) -> ok;
                  (#coap_message{type = ?ACK, method = Method, id = AckMessageID}, AckMessageID) -> {error, Method};
                  (_CoAPMessage, _MessageID) -> ignore
              end,
    CoAPMessage = lwm2m_message_util:publish(ProductDataType, MessageID, Token, Payload),
    send_request(?SIMPLE_CON,CoAPMessage, add_sampler_arg(State, Sampler, MessageID)).


-spec register(pid()) -> any().
register(Pid) ->
    gen_statem:cast(Pid, {register, default}).

-spec register(pid(), binary()) -> any().
register(Pid, RegisterPayload) ->
    gen_statem:cast(Pid, {register, RegisterPayload}).

-spec fresh_register(pid()) -> any().
fresh_register(Pid) ->
    gen_statem:cast(Pid, {fresh_register}).

-spec de_register(pid()) -> any().
de_register(Pid) ->
    gen_statem:cast(Pid, {de_register}).
%%   binary payload, BuildPayload = <<16#02:2, DatasetID:2, (size(Payload)):2, Payload/binary>>,
-spec publish(pid(), json | binary | pass_through, binary()) -> any().
publish(Pid, ProductDataType, PublishData) ->
    gen_statem:cast(Pid, {publish, ProductDataType, PublishData}).

%%--------------------------------------------------------------------------------
%%  fresh message data
%%  fresh message ,then fresh uri observe (if have)
%%--------------------------------------------------------------------------------
fresh_coap_state(CoAPMessage, State) ->
    next_message_id(CoAPMessage, State).
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

handle_get(#coap_message{id = MessageID, token = Token} = CoapMessage, #coap_state{} = State) ->
    Path = coap_message_util:get_uri_path(CoapMessage),
    case Path of
        <<"/3/0">> ->
            {keep_state, send(lwm2m_message_util:response_auto_observe_3_0(MessageID, Token), State)};
        <<"/19/0/0">> ->
            {keep_state,
                send(lwm2m_message_util:response_auto_observe_19_0_0(MessageID, Token), State#coap_state{token_19_0_0 = Token})};
        <<"/4/0/8">> ->
            {keep_state, send(lwm2m_message_util:response_auto_observe_4_0_8(MessageID, Token), State)};
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

%% RequestStyle :: ack_or_die | simple_con
%% ack_or_die -> timeout -> shutdown
%% simple_con -> timeout -> ignore
send_request(RequestStyle, CoAPMessage, State) ->
    {next_state, wait_response, send(CoAPMessage, State),
        [{state_timeout, ?ACK_TIMEOUT, {RequestStyle, ?ACK_TIMEOUT, ?MAX_RETRANSMIT, CoAPMessage}}]
    }.

%%--------------------------------------------------------------------------------
%%  udp api
%%--------------------------------------------------------------------------------
%% will fresh message id and uri observe before send.
-spec send(#coap_message{}, #coap_state{}) -> #coap_state{}.
send(CoAPMessage, #coap_state{socket = Socket, host = Host, port = Port} = State) ->
    gen_udp:send(Socket, Host, Port, coap_message_util:encode(CoAPMessage)),
    io:format("Send Message :~p~n", [CoAPMessage]),
    fresh_coap_state(CoAPMessage, State).
