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
-export([init/1, terminate/3,
    code_change/4, callback_mode/0]).

-export([working/3, waiting/3]).

-export([request/2, request/3, send/2]).

-include("coap.hrl").
-include_lib("emqx_bench/include/emqx_bench.hrl").

-define(SERVER, ?MODULE).

-record(coap_state, {
    socket                      :: gen_udp:socket() | undefined,
    host = {127, 0, 0, 1}       :: binary() | inet:ip_address(),
    port = 5683                 :: integer(),
    message_id_index = 0        :: integer(),
    task_list = []              :: list(),
%%    module must have callback message_build(Args, Loop) -> {ok, Message, NewLoop} | any()
%%    and handle_message(CoAPMessage, Loop) -> {ok, NewLoop} | Ignore :: any()
    callback_module             :: term(),
    callback_loop               :: term(),
%%    map #{ RequestMessageID => RequestCallback }
    request = maps:new()        :: term()
}).

start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, []).

request(Pid, CoAPMessage) ->
    try gen_statem:call(Pid, {request, CoAPMessage}, cal_max_timeout())
    catch _:_ -> {fail, timeout} end.

request(Pid, build_message, CallbackArgs) ->
    try gen_statem:call(Pid, {request, build_message, CallbackArgs}, cal_max_timeout())
    catch _:_ -> {fail, timeout} end.

send(Pid, CoAPMessage) ->
    gen_statem:cast(Pid, {send, CoAPMessage}).
%%--------------------------------------------------------------------------------
%%  gen_statem function
%%--------------------------------------------------------------------------------
init(Args) ->
    State = lists:foldl(fun(Arg, State) -> do_init(Arg, State) end, #coap_state{}, Args),
    {ok, working, State}.

do_init({host, Host}, State)            -> State#coap_state{host = Host};
do_init({port, Port}, State)            -> State#coap_state{port = Port};
do_init({socket, new}, State)           -> State#coap_state{socket = new_socket()};
do_init({socket, keep}, State)          -> State;
do_init({socket, Socket}, State) when is_port(Socket) -> State#coap_state{socket = Socket};
do_init({callback, Module, InitArgs}, State) ->
    try Module:init(InitArgs) of
        CallbackLoop -> State#coap_state{callback_module = Module, callback_loop = CallbackLoop}
    catch _:_ -> State end;
do_init(_, State) -> State.

new_socket() -> {ok, Socket} = gen_udp:open(0, [binary]), Socket.

callback_mode() -> [state_functions].
terminate(_Reason, _StateName, _State = #coap_state{socket = Socket}) ->
    try gen_udp:close(Socket)
    catch _:_ -> ok
    end.

code_change(_OldVsn, StateName, State = #coap_state{}, _Extra) ->
    {ok, StateName, State}.
%%--------------------------------------------------------------------------------
%%  state function
%%--------------------------------------------------------------------------------
working(cast, {send, Message}, State) ->
    do_send(Message, State),
    keep_state_and_data;
working(internal, execute_task, State) -> execute_task(State);
working(cast, Command, State) -> cast_command(Command, State);
working({call, From}, Command, State) -> call_command(From, Command, State);
working(Event, Content, State) -> hand_event(Event, Content, State).

waiting(state_timeout, {request_timeout, CoAPMessage, Time, Retry}, State) when Retry < (?MAX_RETRANSMIT - 1) ->
    do_send(CoAPMessage, State),
    NextTimeout = Time * (Retry + 1),
    {keep_state, State, [{state_timeout, NextTimeout, {request_timeout, CoAPMessage, NextTimeout, Retry + 1}}]};
waiting(state_timeout, {request_timeout, RequestCoAPMessage, _Time, _Retry}, State) ->
    apply_callback({request_time_out, RequestCoAPMessage}, State),
    {next_state, working, State};

waiting({call, From}, {request, _CoAPMessage}, State) ->
    {keep_state, State, [{reply, From, {error, last_requesting}}]};
waiting({call, From}, Message, State) -> call_command(From, Message, State);
waiting(Event, Content, State) -> hand_event(Event, Content, State).

hand_event(info, {udp, Sock, PeerIP, PeerPortNo, Packet}, State) ->
    udp_message(Sock, PeerIP, PeerPortNo, Packet, State);
hand_event(_Event, _Content, _State) -> keep_state_and_data.

%%--------------------------------------------------------------------------------
%%  internal function
%%--------------------------------------------------------------------------------
cast_command({send, CoapMessage}, State) ->
    do_send(CoapMessage, State),
    keep_state_and_data;
cast_command(_, _State) ->
    keep_state_and_data.

call_command(From, {new_task, Task}, State) when is_record(Task, task) ->
    call_command(From, {new_task, [Task]}, State);
call_command(From, {new_task, Tasks}, #coap_state{task_list = TaskList} = State) when is_list(Tasks) ->
    {keep_state, State#coap_state{task_list = TaskList ++ Tasks}, [{reply, From, ok}, {next_event, internal, execute_task}]};

call_command(From, {request, build_message, Args}, #coap_state{callback_module = Mod, callback_loop = Loop} = State) ->
    try erlang:apply(Mod, build_message, [Args, Loop]) of
        {ok, CoAPMessage, NewLoop} ->
            call_command(From, {request, CoAPMessage},
                State#coap_state{callback_loop = NewLoop});
        _ -> {keep_state, State, [{reply, From, {fail, {message_callback_error, build_message_bad_return}}}]}
    catch E:R -> {keep_state, State, [{reply, From, {fail, {E, R}}}]}
    end;
call_command(From, {request, CoAPMessage}, State) ->
    Fun = fun(ResponseCoapMessage, no_arg, CurrentState) ->
            gen_statem:reply(From, ResponseCoapMessage),
            {next_state, working, CurrentState}
          end,
    do_request(CoAPMessage, {Fun, no_arg}, State);
call_command(From, _, State) ->
    {keep_state, State, [{reply, From, un_support_call}]}.

udp_message(_Sock, _PeerIP, _PeerPortNo, Packet, State) ->
    udp_message(Packet, State).
udp_message(Packet, State) ->
    try coap_message_util:decode(Packet) of
        {ok, CoAPMessage} ->
            apply_callback(CoAPMessage, State);
        _ ->
            keep_state_and_data
    catch _:_ ->
        keep_state_and_data
    end.

%% Request call back
%%  1 From -> reply From, ResponseMessage |  {request_timeout, RequestMessage}
%%  2 task -> callback , callback loop , ResponseMessage, RequestArgs
%%  param will be find in State
apply_callback({request_timeout, #coap_message{id = ID} = RequestMessage},
    #coap_state{request = RequestMap} = State) ->
    {Fun, Args} = maps:get(ID, RequestMap),
    try Fun({request_timeout, RequestMessage}, Args, State)
    catch _:_ ->
        {next_state, working,
            State#coap_state{request = maps:remove(ID, RequestMap)},
            [{state_timeout, cancel}, {next_event, internal, execute_task}]}
    end;
apply_callback(#coap_message{id = ID} = ResponseMessage,
    #coap_state{request = RequestMap} = State) ->
    case maps:find(ID, RequestMap) of
        {ok, {Fun, Args}} ->
            try Fun(ResponseMessage, Args, State)
            catch _:_ -> {next_state, working,
                State#coap_state{request = maps:remove(ID, RequestMap)},
                [{state_timeout, cancel}, {next_event, internal, execute_task}]}
            end;
        _ ->
            %% request map no found
            apply_callback_handle_message(ResponseMessage, State)
    end.

apply_callback_handle_message(CoAPMessage,
    #coap_state{callback_module = CallbackModule, callback_loop = CallbackLoop} = State)->
    NewState = synchronize_state_message_id(CoAPMessage, State),
    try erlang:apply(CallbackModule, handle_message, [CoAPMessage, CallbackLoop]) of
        {ok, NewLoop} ->
            {keep_state, NewState#coap_state{callback_loop = NewLoop}};
        _ ->
            {keep_state, NewState}
    catch
        _:_  ->
            {keep_state, NewState}
    end.

cal_max_timeout() ->
    cal_max_timeout(?ACK_TIMEOUT, 0).
cal_max_timeout(Timeout, MaxRetry) when MaxRetry >= ?MAX_RETRANSMIT ->
    Timeout;
cal_max_timeout(Timeout, Retry) ->
    cal_max_timeout(Timeout + (Timeout * (Retry + 1)), (Retry + 1)).

execute_task(#coap_state{task_list = []}) -> keep_state_and_date;
execute_task(#coap_state{task_list = [#task{action = request, args = Args} = Task | Tasks],
    callback_module = Mod, callback_loop = Loop} = State) ->
    try erlang:apply(Mod, build_message, [Args, Loop]) of
        {ok, CoAPMessage, NewLoop} ->
            Fun = fun(Response ,Task, CurrentState) ->
%%                   todo  response sample and task callback
                    {next_state, working, CurrentState}
                  end,
            do_request(CoAPMessage, {Fun, Task}, State#coap_state{callback_loop = NewLoop, task_list = Tasks});
        _ -> {keep_state, State, [{next_event, internal, execute_task}]}
    catch _:_ ->
        Mod:task_execute(error, message_build_error),
        {keep_state, State#coap_state{task_list = Tasks}, [{next_event, internal, execute_task}]}
    end;
execute_task(#coap_state{task_list = [_ | Tasks]} = State) ->
    {keep_state, State#coap_state{task_list = Tasks}, [{next_event, internal, execute_task}]}.

%%--------------------------------------------------------------------------------
%%  send request ,will synchronize message id
%%--------------------------------------------------------------------------------
do_request(CoAPMessage, RequestCallback, #coap_state{request = RequestMap} = State) ->
    {#coap_message{id = ID} = NewMessage, NewState} = synchronize_message_id(CoAPMessage, State),
    NewMap = maps:put(ID, RequestCallback, RequestMap),
    do_send(NewMessage, NewState),
    {next_state, waiting, NewState#coap_state{request = NewMap},
        [{state_timeout, ?ACK_TIMEOUT, {request_timeout, CoAPMessage, ?ACK_TIMEOUT, 0}}]}.

%%--------------------------------------------------------------------------------
%%  udp send api,will not synchronize message id
%%--------------------------------------------------------------------------------
do_send(CoAPMessage, #coap_state{socket = Socket, host = Host, port = Port} = State) ->
    io:format(">>>>~n  ~0p~n", [CoAPMessage]),
    {ok, Package} = coap_message_util:encode(CoAPMessage),
    gen_udp:send(Socket, Host, Port, Package),
    {keep_state, State}.

%%--------------------------------------------------------------------------------
%%  synchronize message id
%%--------------------------------------------------------------------------------
synchronize_message_id(#coap_message{id = MessageID} = CoAPMessage, #coap_state{message_id_index = Index} = State)
    when MessageID =:= -1 ->
    {CoAPMessage#coap_message{id = Index}, State#coap_state{message_id_index = Index + 1}};
synchronize_message_id(#coap_message{id = MessageID} = CoAPMessage, #coap_state{message_id_index = Index} = State)
    when MessageID >= Index ->
    {CoAPMessage, State#coap_state{message_id_index = MessageID + 1}};
synchronize_message_id(#coap_message{id = MessageID} = CoAPMessage, #coap_state{message_id_index = Index} = State)
    when MessageID =< Index ->
    {CoAPMessage#coap_message{id = Index}, State#coap_state{message_id_index = Index + 1}}.
%% only up state to last message id
synchronize_state_message_id(#coap_message{id = MessageID}, #coap_state{message_id_index = Index} = State)
    when MessageID >= Index ->
    State#coap_state{message_id_index = MessageID + 1};
synchronize_state_message_id(_, State) ->
    State.


