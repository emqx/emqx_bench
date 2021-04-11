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
    tasks = []                  :: list(),
    current_task                :: {ID::integer(), Task :: #task{}} | {waiting, Task :: #task{}},
    %% module must have callback message_build(Args, Loop) -> {ok, Message, NewLoop} | any()
    %% and handle_message(CoAPMessage, Loop) -> {ok, NewLoop} | Ignore :: any()
    callback_module             :: term(),
    callback_loop               :: term()
}).

start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, []).

request(Pid, CoAPMessage) ->
    try 
        Task = #task{action = request, args = CoAPMessage},
        gen_statem:call(Pid, {new_task, Task}, cal_max_timeout())
    catch _:_ -> 
        {fail, timeout}
    end.

request(Pid, build_message, CallbackArgs) ->
    try 
        Task = #task{action = {request, build_message}, args = CallbackArgs},
        gen_statem:call(Pid, {new_task, Task}, cal_max_timeout())
    catch _:_ -> 
        {fail, timeout}
    end.

send(Pid, CoAPMessage) ->
    Task = #task{action = send, args = CoAPMessage},
    gen_statem:cast(Pid, {new_task, Task}).
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
    try 
        Module:init(InitArgs)
    of
        CallbackLoop -> State#coap_state{callback_module = Module, callback_loop = CallbackLoop}
    catch _:_ -> 
        State
    end;
do_init(_, State) -> State.

new_socket() -> {ok, Socket} = gen_udp:open(0, [binary]), Socket.

callback_mode() -> [state_functions].
terminate(_Reason, _StateName, _State = #coap_state{socket = Socket}) ->
    gen_udp:close(Socket).

code_change(_OldVsn, StateName, State = #coap_state{}, _Extra) ->
    {ok, StateName, State}.
%%--------------------------------------------------------------------------------
%%  state function
%%--------------------------------------------------------------------------------
working(internal, execute_task, State) -> 
    execute_task(State);
working(Event, Content, State) -> 
    hand_event(Event, Content, State).

waiting(state_timeout, {CoAPMessage, Time, Retry}, State) when Retry < (?MAX_RETRANSMIT - 1) ->
    do_send(CoAPMessage, State),
    NextTimeout = Time * (Retry + 1),
    {keep_state, State, [{state_timeout, NextTimeout, {CoAPMessage, NextTimeout, Retry + 1}}]};
waiting(state_timeout, {Message, _Time, _Retry}, State) ->
    apply_callback({timeout, Message}, State);
waiting(internal, execute_task, _State) -> 
    keep_state_adn_data;
waiting(Event, Content, State) -> 
    hand_event(Event, Content, State).

%%--------------------------------------------------------------------------------
%%  internal function
%%--------------------------------------------------------------------------------
hand_event(info, {udp, Sock, PeerIP, PeerPortNo, Packet}, State) ->
    udp_message(Sock, PeerIP, PeerPortNo, Packet, State);
hand_event(cast, Command, State) -> 
    do_cast(Command, State);
hand_event({call, From}, Command, State) -> 
    do_call(From, Command, State);
hand_event(_Event, _Content, _State) -> 
    keep_state_and_data.

do_cast({new_task, Tasks}, #coap_state{tasks = TaskList} = State) when is_list(Tasks) ->
    {keep_state, State#coap_state{tasks = TaskList ++ Tasks}, [{next_event, internal, execute_task}]};
do_cast(_, _State) ->
    keep_state_and_data.

do_call(From, {new_task, Task}, State) when is_record(Task, task) ->
    Tasks = add_task_from([Task], From, []),
    do_call(From, {new_tasks, Tasks}, State);
do_call(From, {new_tasks, Tasks}, #coap_state{tasks = TaskList} = State) when is_list(Tasks) ->
    Tasks1 = add_task_from(Tasks, From, []),
    {keep_state, State#coap_state{tasks = TaskList ++ Tasks1}, [{reply, From, ok}, {next_event, internal, execute_task}]};
do_call(From, _, State) ->
    {keep_state, State, [{reply, From, un_support}]}.

udp_message(_Sock, _PeerIP, _PeerPortNo, Packet, State) ->
    udp_message(Packet, State).
udp_message(Packet, State) ->
    try 
        coap_message_util:parse(Packet)
    of
        {ok, CoAPMessage} ->
            io:format("<<<<~0p~n", [CoAPMessage]),
            apply_callback(CoAPMessage, State);
        _ ->
            keep_state_and_data
    catch _:_ ->
        keep_state_and_data
    end.

%%--------------------------------------------------------------------------------
%% callback
%%--------------------------------------------------------------------------------
apply_callback(Message, State) ->
    NewState = synchronize_state_message_id(Message, State),
    apply_callback_(Message, NewState).

apply_callback_({timeout, CoAPMessage}, State) when is_record(CoAPMessage, coap_message) ->
    reply_task_call(CoAPMessage, State);
apply_callback_({timeout, Task}, _State) when is_record(Task, task) ->
    %% TODO: apply callback
    apply_waiting_task;
apply_callback_(Message, #coap_state{current_task = CurrentTask} = State) ->
    #coap_message{id = ID} = Message,
    case        
        CurrentTask
    of
        {waiting, _Task} -> 
            task_wating(Message, State);
        {ID, _Task} -> 
            reply_task_execute_result(Message, State);    
        undefined -> 
            apply_cllback(Message, handle_message, State)
    end.

reply_task_call(Message, #coap_state{current_task = {_, #task{from = From}}} = State) ->
    gen_statem:reply(From, Message),
    {next_state, working, State, [{next_event, internal, execute_task}, {state_timeout, cancel}]}.

task_wating(Message, #coap_state{current_task = {_, Task}, callback_module = CallbackModule, callback_loop = CallbackLoop} = State) ->
    try 
        erlang:apply(CallbackModule, task_result, [Message, Task] ++ [CallbackLoop])
    of 
        {ok, NewLoop} ->
            {next_state, working, State#coap_state{callback_loop = NewLoop, current_task = undefined},
            [{next_event, internal, execute_task}, {state_timeout, cancel}]};
        {ignore, NewLoop} ->
            {keep_state, State#coap_state{callback_loop = NewLoop}};
        {stop, NewLoop} ->
            {next_state, working, State#coap_state{callback_loop = NewLoop}, [{state_timeout, cancel}]}
    catch _:_ ->
        {next_state, working, State}
    end.

reply_task_execute_result(Message, #coap_state{current_task = {_, #task{from = From} = Task}} = State) ->
    case 
        From
    of
        undefined -> 
            apply_cllback({Task, Message}, task_result, State);
        From -> 
            gen_statem:reply(From, {Task, Message})
    end,
    {next_state, working, State#coap_state{current_task = undefined}, [{next_event, internal, execute_task}, {state_timeout, cancel}]}.

apply_cllback(Message, Function, #coap_state{callback_module = CallbackModule, callback_loop = CallbackLoop} = State) ->
    {ok, NewLoop} = erlang:apply(CallbackModule, Function, [Message] ++ [CallbackLoop]),
    try 
        {ok, NewLoop} = erlang:apply(CallbackModule, Function, [Message] ++ [CallbackLoop]),
        {next_state, working, State#coap_state{callback_loop = NewLoop}}
    catch _:_ ->
        {next_state, working, State}
    end.

cal_max_timeout() ->
    cal_max_timeout(?ACK_TIMEOUT, 0).
cal_max_timeout(Timeout, MaxRetry) when MaxRetry >= ?MAX_RETRANSMIT ->
    Timeout;
cal_max_timeout(Timeout, Retry) ->
    cal_max_timeout(Timeout + (Timeout * (Retry + 1)), (Retry + 1)).

execute_task(#coap_state{tasks = []}) -> keep_state_and_data;
execute_task(#coap_state{tasks = [#task{action = {request, build_message}, args = Args} = Task | Tasks],
    callback_module = Mod, callback_loop = Loop} = State) ->
    try 
        erlang:apply(Mod, build_message, [Args, Loop])
    of
        {ok, CoAPMessage, NewLoop} ->
            do_request(CoAPMessage, Task, State#coap_state{callback_loop = NewLoop, tasks = Tasks})
    catch _:_ ->
        apply_cllback([Task, {fail, message_build_error}], task_result, State#coap_state{tasks = Tasks})
    end;
execute_task(#coap_state{tasks = [#task{action = request, args = CoAPMessage} = Task | Tasks]} = State) ->
    do_request(CoAPMessage, Task, State#coap_state{tasks = Tasks});
execute_task(#coap_state{tasks = [#task{action = send, args = CoAPMessage} = Task | Tasks]} = State) ->
    %% TODO: task callback task callback send success
    do_send(CoAPMessage, State#coap_state{tasks = Tasks, current_task = undefined});
execute_task(#coap_state{tasks = [#task{action = waiting, args = Args} = Task | Tasks]} = State) ->
    Timeout = find_args(timeout, Args, ?ACK_TIMEOUT),
    TimeoutMessage = waiting_timeout,
    {next_state, waiting, State#coap_state{tasks = Tasks, current_task = {waiting, Task}},
        [{state_timeout, Timeout, TimeoutMessage}]};
execute_task(#coap_state{tasks = [_ | Tasks]} = State) ->
    {keep_state, State#coap_state{tasks = Tasks}, [{next_event, internal, execute_task}]}.

%%--------------------------------------------------------------------------------
%%  send request ,will synchronize message id
%%--------------------------------------------------------------------------------
do_request(CoAPMessage, Task, State) ->
    {#coap_message{id = ID} = NewMessage, NewState} = synchronize_message_id(CoAPMessage, State),
    do_send(NewMessage, NewState),
    TimeoutMessage = {CoAPMessage, ?ACK_TIMEOUT, 0},
    {next_state, waiting, NewState#coap_state{current_task = {ID, Task}},
        [{state_timeout, ?ACK_TIMEOUT, TimeoutMessage}]}.

%%--------------------------------------------------------------------------------
%%  udp send api,will not synchronize message id
%%--------------------------------------------------------------------------------
do_send(CoAPMessage, #coap_state{socket = Socket, host = Host, port = Port} = State) ->
    io:format(">>>>~n  ~0p~n", [CoAPMessage]),
    {ok, Package} = coap_message_util:serialize(CoAPMessage),
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

%%--------------------------------------------------------------------------------
%%  finde args, from map or properlist
%%--------------------------------------------------------------------------------
find_args(Key, Args, Default) when is_list(Args) ->
    proplists:get_value(Key, Args, Default);
find_args(Key, Args, Default) when is_map(Args) ->
    maps:get(Key, Args, Default);
find_args(_Key, _Args, Default) ->
    Default.

add_task_from([], _From, Result)-> 
    lists:reverse(Result);
add_task_from([Task = #task{from = undefined} | Tasks], From, Result) -> 
    add_task_from(Tasks, From, Result ++ [Task#task{from = From}]);
add_task_from([Task | Tasks], From, Result) -> 
    add_task_from(Tasks, From, Result ++ [Task#task{from = From}]).


