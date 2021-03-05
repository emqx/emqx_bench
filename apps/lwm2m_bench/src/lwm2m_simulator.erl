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
-export([register/1, de_register/1, update_register/1, publish/2]).

-define(SERVER, ?MODULE).

-define(MSG_TIMEOUT, message_time_out).

-record(coap_state, {
    socket                              :: gen_udp:socket(),
    host                                :: binary()     | tuple     | inet:ip_address(),
    port                                :: integer(),
    imei                                :: binary(),
    sampler                             :: function(),
    sampler_arg                         :: any(),
    data_type           = json          :: pass_through | json      | binary,
    lifetime            = 300           :: integer(),
    register_payload    = <<"</>;rt=\"oma.lwm2m\";ct=11543,<3/0>,<19/0>">>,
    message_id_index    = 0             :: integer(),
    token_19_0_0        = un_defined    :: un_defined   | binary(),
    task_list           = []            :: list()
}).

start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, []).

callback_mode() -> state_functions.
terminate(_Reason, _StateName, _State = #coap_state{socket = Socket}) -> gen_udp:close(Socket).
code_change(_OldVsn, StateName, State = #coap_state{}, _Extra) -> {ok, StateName, State}.

init(Args) ->
    {ok, working, do_init(Args, #coap_state{})}.

do_init([{imei, IMEI} | Args], State) -> do_init(Args, State#coap_state{imei = IMEI});
do_init([{host, Host} | Args], State) -> do_init(Args, State#coap_state{host = Host});
do_init([{port, Port} | Args], State) -> do_init(Args, State#coap_state{port = Port});
do_init([{register_payload, Payload} | Args], State) -> do_init(Args, State#coap_state{register_payload = Payload});
do_init([{lifetime, Lifetime} | Args], State) -> do_init(Args, State#coap_state{lifetime = Lifetime});
do_init([{data_type, pass_through} | Args], State) -> do_init(Args, State#coap_state{data_type = pass_through});
do_init([{data_type, json} | Args], State) -> do_init(Args, State#coap_state{data_type = json});
do_init([{data_type, binary} | Args], State) -> do_init(Args, State#coap_state{data_type = binary});
do_init([{data_type, _} | Args], State) -> do_init(Args, State#coap_state{data_type = pass_through});
do_init([{_, _} | Args], State) -> do_init(Args, State);
do_init([], State) ->
%%   {ok, Sock} = gen_udp:open(0, [{ip, {192,168,1,120}}, binary, {active, false}, {reuseaddr, false}]),
    {ok, Socket} = gen_udp:open(0, [binary]),
    State#coap_state{socket = Socket}.

working(info, {udp, _Sock, _PeerIP, _PeerPortNo, Packet}, State) ->
    {ok, CoAPMessage} = coap_message_util:decode(Packet),
    %% fresh new message id and if message has uri observe
    NewState = fresh_coap_state(CoAPMessage, State),
    io:format("Receive coap message:~0p~n", [CoAPMessage]),
    handle_message(CoAPMessage, NewState);

working(cast, Command, State) ->
    io:format("receive command ~0p~n", [Command]),
    execute(Command, State);
working(Any, Data, State) ->
    io:format("working，event type ~0p~n, Data ~0p~n,State ~0p~n ", [Any, Data, State]),
    keep_state_and_data.

%% RequestStyle :: ack_or_die | simple_con
%% ack_or_die -> timeout -> shutdown
%% simple_con -> timeout -> ignore
wait_response(state_timeout, {RequestStyle, AckTimeout, LastTimes, CoAPMessage}, State) when LastTimes > 1 ->
    {keep_state, send(CoAPMessage, State),
        [{state_timeout, AckTimeout,
            {RequestStyle, AckTimeout * (?MAX_RETRANSMIT - LastTimes + 1), LastTimes - 1, CoAPMessage}}]};
wait_response(state_timeout, {_AckTimeout, LastTimes, CoAPMessage},
    #coap_state{sampler = Sampler, sampler_arg = Arg} = State) when LastTimes =:= 1 ->
    case Sampler(?MSG_TIMEOUT, Arg) of
        {ok, _}         -> {next_state, working, fresh_coap_state(CoAPMessage, State),[{timeout, cancel}]};
        {error, Reason} -> {stop, {shutdown, {action_failed , Reason}}, State};
        ignore          -> {keep_state, fresh_coap_state(CoAPMessage, State)}
    end,
    {stop, {shutdown, {ack_time_out, CoAPMessage}}, State};
wait_response(state_timeout, {_AckTimeout, LastTimes, _CoAPMessage}, _State) when LastTimes =:= 1 ->
    keep_state_and_data;
wait_response(info, {udp, _Sock, _PeerIP, _PeerPortNo, Packet},
    #coap_state{sampler = Sampler,sampler_arg = SamplerArg} = State)->
    {ok, CoAPMessage} = coap_message_util:decode(Packet),
    io:format("waitting response ~0p~n",[CoAPMessage]),
    case Sampler(CoAPMessage, SamplerArg) of
        {ok, _}         -> {next_state, working, fresh_coap_state(CoAPMessage, State),[{timeout, cancel}]};
        {error, Reason} -> {stop, {shutdown, {action_failed , Reason}}, State};
        ignore          -> {keep_state, fresh_coap_state(CoAPMessage, State)}
    end;
wait_response(Any, Data, State) ->
    io:format("wait message, event type ~0p~n, Data ~0p~n,State ~0p~n ", [Any, Data, State]),
    keep_state_and_data.

%%--------------------------------------------------------------------------------
%% execute function
%%--------------------------------------------------------------------------------

-define(REGISTER_SAMPLER, fun
                              (?MSG_TIMEOUT, _Arg) -> {};
                              (#coap_message{type = ?ACK, method = ?CREATED, id = AckMessageID}, AckMessageID) ->
                                  {ok, register_success};
                              (#coap_message{type = ?ACK, method = Method,   id = AckMessageID}, AckMessageID) ->
                                  {fail, Method};
                              (_CoAPMessage, _MessageID) -> ignore
                          end).

-define(UPDATE_SAMPLER, fun
                              (?MSG_TIMEOUT, _Arg) -> {};
                              (#coap_message{type = ?ACK, method = ?CHANGED, id = AckMessageID}, AckMessageID) ->
                                  {ok, update_success};
                              (#coap_message{type = ?ACK, method = Method,   id = AckMessageID}, AckMessageID) ->
                                  {error, Method};
                              (_CoAPMessage, _MessageID) -> ignore
                          end).
-define(DEREGISTER_SAMPLER, fun
                                (?MSG_TIMEOUT, _Arg) -> {};
                                (#coap_message{type = ?ACK, method = ?CHANGED, id = AckMessageID}, AckMessageID) ->
                                    {ok, deregister_success};
                                (#coap_message{type = ?ACK, method = Method,   id = AckMessageID}, AckMessageID) ->
                                    {error, Method};
                                (_CoAPMessage, _MessageID) -> ignore
                            end).
-define(PUBLISH_SAMPLER, fun
                             (?MSG_TIMEOUT, _Arg) -> {};
                             (#coap_message{type = ?ACK, id = AckMessageID}, AckMessageID) -> ok;
                             (#coap_message{type = ?ACK, method = Method, id = AckMessageID}, AckMessageID) ->
                                 {error, Method};
                             (_CoAPMessage, _MessageID) -> ignore
                         end).

add_sampler_arg(State, Sampler, SamplerArgs)->
    State#coap_state{sampler = Sampler,sampler_arg = SamplerArgs}.

execute({register},
    #coap_state{message_id_index = MessageID, lifetime = LifeTime,
        imei = IMEI, register_payload = RegisterPayload} = State) ->
    CoAPMessage = lwm2m_message_util:register(MessageID, LifeTime, IMEI, RegisterPayload),
    send_request(CoAPMessage, add_sampler_arg(State, ?REGISTER_SAMPLER, MessageID));
execute({register_standard_module},
    #coap_state{message_id_index = MessageID, lifetime = LifeTime,
        imei = IMEI, register_payload = RegisterPayload} = State) ->
    CoAPMessage = lwm2m_message_util:register_standard_module(MessageID, LifeTime, IMEI, RegisterPayload),
    send_request(CoAPMessage, add_sampler_arg(State, ?REGISTER_SAMPLER, MessageID));
execute({update_register}, #coap_state{lifetime = LifeTime, imei = IMEI, message_id_index = MessageID} = State) ->
    CoAPMessage = lwm2m_message_util:update_register(MessageID, LifeTime, IMEI),
    send_request(CoAPMessage, add_sampler_arg(State, ?UPDATE_SAMPLER, MessageID));
execute({de_register}, #coap_state{imei = IMEI, message_id_index = MessageID} = State) ->
    CoAPMessage = lwm2m_message_util:deregister(MessageID, IMEI),
    send_request(CoAPMessage, add_sampler_arg(State, ?DEREGISTER_SAMPLER, MessageID));
execute({publish, Payload},
    #coap_state{message_id_index = MessageID, token_19_0_0 = Token, data_type = DataType} = State) ->
    CoAPMessage = lwm2m_message_util:publish(DataType, MessageID, Token, Payload),
    send_request(CoAPMessage, add_sampler_arg(State, ?PUBLISH_SAMPLER, MessageID)).

%%--------------------------------------------------------------------------------
%%  simulator action
%%--------------------------------------------------------------------------------
-spec register(pid()) -> any().
register(Pid) ->
    gen_statem:cast(Pid, {register}).

-spec update_register(pid()) -> any().
update_register(Pid) ->
    gen_statem:cast(Pid, {update_register}).

-spec de_register(pid()) -> any().
de_register(Pid) ->
    gen_statem:cast(Pid, {de_register}).

%%   binary payload, BuildPayload = <<16#02:2, DatasetID:2, (size(Payload)):2, Payload/binary>>,
-spec publish(pid(), binary()) -> any().
publish(Pid, PublishData) ->
    gen_statem:cast(Pid, {publish, PublishData}).

%%--------------------------------------------------------------------------------
%%  fresh message data
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
    {ok, Path} = coap_message_util:get_uri_path(CoapMessage),
    case Path of
        <<"/3/0">> ->
            {keep_state, send(lwm2m_message_util:response_auto_observe_3_0(MessageID, Token), State)};
        <<"/19/0/0">> ->
            {keep_state,
                send(lwm2m_message_util:response_auto_observe_19_0_0(MessageID, Token),
                    State#coap_state{token_19_0_0 = Token})};
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

send_request(CoAPMessage, State) ->
    {next_state, wait_response, send(CoAPMessage, State),
        [{state_timeout, ?ACK_TIMEOUT, {?ACK_TIMEOUT, ?MAX_RETRANSMIT, CoAPMessage}}]}.

%%--------------------------------------------------------------------------------
%%  udp api
%%--------------------------------------------------------------------------------
%% will fresh message id and uri observe before send.
-spec send(#coap_message{}, #coap_state{}) -> #coap_state{}.
send(CoAPMessage, #coap_state{socket = Socket, host = Host, port = Port} = State) ->
    {ok, Package} = coap_message_util:encode(CoAPMessage),
    gen_udp:send(Socket, Host, Port, Package),
    io:format("Send Message :~0p~n", [CoAPMessage]),
    fresh_coap_state(CoAPMessage, State).
