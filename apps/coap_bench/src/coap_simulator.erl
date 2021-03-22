%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 3月 2021 11:26 上午
%%%-------------------------------------------------------------------
-module(coap_simulator).
-author("DDDHuang").

-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, terminate/3,
    code_change/4, callback_mode/0]).

-export([working/3, waiting/3]).

-export([request/2, request/3, send/2, wait/3, wait/4, new_callback/2]).

-include("coap.hrl").

-define(SERVER, ?MODULE).

-record(coap_state, {
    socket                              :: gen_udp:socket() | undefined,
    host                = {127, 0, 0, 1}:: binary() | inet:ip_address(),
    port                = 5683          :: integer(),
    message_id_index    = 0             :: integer(),
    task_list           = []            :: list(),
%%    module must have callback message_build(Args, Loop) -> {ok, Message, NewLoop} | any()
%%    and handle_message(CoAPMessage, Loop) -> {ok, NewLoop} | Ignore :: any()
    callback_module                     :: term(),
    callback_loop                       :: term(),
%%    map #{ RequestMessageID => From }
    request = maps:new()                :: term()
}).

start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, []).

init(Args) ->
    {ok, working, do_init(Args, #coap_state{})}.

do_init([], State) -> State;
do_init([{host, Host} | Args], State) -> do_init(Args, State#coap_state{host = Host});
do_init([{port, Port} | Args], State) -> do_init(Args, State#coap_state{port = Port});
do_init([{task_list, TaskList} | Args], State) -> do_init(Args, State#coap_state{task_list = TaskList});
do_init([{callback_module, CallBack} | Args], State) -> do_init(Args, State#coap_state{callback_module = CallBack});
do_init([{callback_loop, Loop} | Args], State) -> do_init(Args, State#coap_state{callback_loop = Loop});
do_init([{socket, new} | Args], State) -> do_init(Args, State#coap_state{socket = new_socket()});
do_init([{socket, keep} | Args], State) -> do_init(Args, State);
do_init([{socket, Socket} | Args], State) when is_port(Socket) -> do_init(Args, State#coap_state{socket = Socket});
do_init([_ | Args], State) -> do_init(Args, State).


new_socket() -> {ok, Socket} = gen_udp:open(0, [binary]), Socket.

callback_mode() -> [state_functions].
format_status(_Opt, [_PDict, _StateName, _State]) ->
    Status = some_term,
    Status.


working(cast, {send, Message}, State) ->
    do_send(Message, State),
    keep_state_and_data;
working(cast, Command, State) -> cast_command(Command, State);
working({call, From}, Command, State) -> call_command(From, Command, State);
working(Event, Content, State) -> hand_event(Event, Content, State).

waiting(state_timeout, {request_timeout, CoAPMessage, Time, Retry}, State) when Retry < (?MAX_RETRANSMIT - 1) ->
    do_send(CoAPMessage, State),
    NextTimeout = Time * (Retry + 1),
    {keep_state, State, [{state_timeout, NextTimeout, {request_timeout, CoAPMessage, NextTimeout, Retry + 1}}]};
waiting(state_timeout, {request_timeout, #coap_message{id = ID} = RequestCoAPMessage, _Time, _Retry},
    #coap_state{request = RequestMap} = State) ->
    From = maps:get(ID, RequestMap),
    erase({request, ID}),
    {next_state, working, State, [{reply, From, {request_timeout, RequestCoAPMessage}}]};
waiting({call, From}, {request, _CoAPMessage}, State) ->
    {keep_state, State, [{reply, From, {error, last_requesting}}]};
waiting({call, From}, Message, State) -> call_command(From, Message, State);
waiting(Event, Content, State) -> hand_event(Event, Content, State).

hand_event(info, {udp, Sock, PeerIP, PeerPortNo, Packet}, State) ->
    udp_message(Sock, PeerIP, PeerPortNo, Packet, State);
hand_event(_Event, _Content, _State)-> keep_state_and_data.

%%--------------------------------------------------------------------------------
%%  gen_statem function
%%--------------------------------------------------------------------------------
terminate(_Reason, _StateName, _State = #coap_state{socket = Socket}) ->
    try gen_udp:close(Socket)
    catch _:_ -> ok
    end.

code_change(_OldVsn, StateName, State = #coap_state{}, _Extra) ->
    {ok, StateName, State}.

cast_command({send, CoapMessage}, State) ->
    do_send(CoapMessage, State),
    keep_state_and_data;
cast_command(_, _State) ->
    keep_state_and_data.


call_command(From, {request, build_message, Args}, #coap_state{callback_module = Mod, callback_loop = Loop} = State)->
    erlang:apply(Mod, build_message, [Args, Loop]),
    try erlang:apply(Mod, build_message, [Args, Loop]) of
        {ok, CoAPMessage, NewLoop} ->
            call_command(From, {request, CoAPMessage},
                State#coap_state{callback_loop = NewLoop});
        _ -> {keep_state, State, [{reply, From, {fail, {message_callback_error, build_message_bad_return}}}]}
    catch E:R -> {keep_state, State, [{reply, From, {fail, {E,R}}}]}
    end;
call_command(From, {request, CoAPMessage}, State)->
    do_request(CoAPMessage, From, State);
call_command(From, _, State)->
    {keep_state, State, [{reply, From, un_support_call}]}.

udp_message(_Sock, _PeerIP, _PeerPortNo, Packet, State) ->
    udp_message(Packet, State).
udp_message(Packet, #coap_state{request = RequestMap, callback_module = Mod, callback_loop = Loop} = State) ->
    try coap_message_util:decode(Packet) of
        {ok, #coap_message{id = ID} = CoAPMessage} ->
            case maps:find(ID, RequestMap) of
                {ok, From} ->
                    NewMap = maps:remove(ID, RequestMap),
                    {next_state, working, State#coap_state{request = NewMap},
                        [{reply, From, {ok, CoAPMessage}}, {state_timeout, cancel}]};
                _ ->
                    NewState = synchronize_state_message_id(CoAPMessage, State),
                    NewLoop = apply_callback_handle_message(Mod, CoAPMessage, Loop),
                    {keep_state, NewState#coap_state{callback_loop = NewLoop}}
            end
    catch _:_  -> keep_state_and_data end.

apply_callback_handle_message(undefined, _, Loop)-> Loop;
apply_callback_handle_message(Mod, CoAPMessage, Loop)->
    try Mod:handle_message(CoAPMessage, Loop) of
        {ok, NewLoop} -> NewLoop;
        _ -> Loop
    catch _:_ -> {ok, Loop}
    end.


%%--------------------------------------------------------------------------------
%%  api
%%--------------------------------------------------------------------------------
request(Pid, CoAPMessage) ->
    try gen_statem:call(Pid, {request, CoAPMessage}, cal_max_timeout())
    catch _:_ -> {fail, timeout} end.
request(Pid, build_message, CallbackArgs) ->
    try gen_statem:call(Pid, {request, build_message, CallbackArgs}, cal_max_timeout())
    catch _:_ -> {fail, timeout} end.

send(Pid, CoAPMessage) ->
    gen_statem:cast(Pid, {send, CoAPMessage}).

wait(Pid, MessageSampler, SamplerArgs)->
    wait(Pid, MessageSampler, SamplerArgs, 5000).
wait(Pid, MessageSampler, SamplerArgs, Timeout)->
    try gen_statem:call(Pid, {wait, MessageSampler, SamplerArgs, Timeout}, Timeout)
    catch _:_ -> {fail, timeout} end.

new_callback(Pid, Callback)->
    try  gen_statem:call(Pid, {new_callback, Callback}, 200)
    catch _:_ -> {fail, timeout} end.


%%--------------------------------------------------------------------------------
%%  internal function
%%--------------------------------------------------------------------------------
cal_max_timeout()->
    cal_max_timeout(?ACK_TIMEOUT, 0).
cal_max_timeout(Timeout, MaxRetry) when MaxRetry >= ?MAX_RETRANSMIT->
    Timeout;
cal_max_timeout(Timeout, Retry) ->
    cal_max_timeout(Timeout + (Timeout * (Retry + 1)), (Retry + 1)).
%%--------------------------------------------------------------------------------
%%  send request ,will synchronize message id
%%--------------------------------------------------------------------------------
do_request(CoAPMessage, From, #coap_state{request = RequestMap} = State) ->
    {#coap_message{id = ID} = NewMessage, NewState} = synchronize_message_id(CoAPMessage, State),
    NewMap = maps:put(ID, From, RequestMap),
    do_send(NewMessage, NewState),
    {next_state, waiting, NewState#coap_state{request = NewMap},
        [{state_timeout, ?ACK_TIMEOUT, {request_timeout, CoAPMessage, ?ACK_TIMEOUT, 0}}]}.

%%--------------------------------------------------------------------------------
%%  udp send api,will not synchronize message id
%%--------------------------------------------------------------------------------
do_send(CoAPMessage, #coap_state{socket = Socket, host = Host, port = Port} = State) ->
    io:format("send  >>> ~0p~n", [CoAPMessage]),
    {ok, Package} = coap_message_util:encode(CoAPMessage),
    gen_udp:send(Socket, Host, Port, Package),
    {keep_state, State}.

%%--------------------------------------------------------------------------------
%%  synchronize message id
%%--------------------------------------------------------------------------------
synchronize_message_id(#coap_message{id = MessageID} = CoAPMessage,#coap_state{message_id_index = Index} = State)
    when MessageID =:= -1 ->
    {CoAPMessage#coap_message{id = Index}, State#coap_state{message_id_index = Index + 1}};
synchronize_message_id(#coap_message{id = MessageID} = CoAPMessage,#coap_state{message_id_index = Index} = State)
    when MessageID >= Index ->
    {CoAPMessage, State#coap_state{message_id_index = MessageID + 1}};
synchronize_message_id(#coap_message{id = MessageID} = CoAPMessage,#coap_state{message_id_index = Index} = State)
    when MessageID =< Index ->
    {CoAPMessage#coap_message{id = Index}, State#coap_state{message_id_index = Index + 1}}.
%% only up state to last message id
synchronize_state_message_id(#coap_message{id = MessageID},#coap_state{message_id_index = Index} = State)
    when MessageID >= Index ->
    State#coap_state{message_id_index = MessageID + 1};
synchronize_state_message_id(_, State) ->
    State.


