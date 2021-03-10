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
-include_lib("emqx_bench/include/emqx_bench.hrl").

-export([start_link/1]).
-export([init/1, terminate/3, code_change/4, callback_mode/0]).

-export([working/3, wait_message/3]).
-export([register/1, de_register/1, update_register/1, publish/2, close/1, new_imei/3]).

-record(coap_state, {
    socket                              :: gen_udp:socket(),
    host                                :: binary()     | tuple     | inet:ip_address(),
    port                                :: integer(),
    imei                                :: binary(),
    sampler                             :: function(),
    current_request_id                  :: term(), %% request message
    data_type           = json          :: pass_through | json      | binary,
    lifetime            = <<"300">>     :: binary(),
    register_payload    = <<"</>;rt=\"oma.lwm2m\";ct=11543,<3/0>,<19/0>">>,
    message_id_index    = 0             :: integer(),
    token_19_0_0        = undefined     :: undefined   | binary(),
    task_list           = []            :: list(),
    %% task_callback :: {
    %%                      fun( Task :: #task{},Result :: success | {fail, Reason}, CallBackArgs :: any()),
    %%                      CallBackArgs :: any()
    %%                  }
    task_callback       = undefined    :: term()
}).

start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, []).

callback_mode() -> [state_functions].
terminate(_Reason, _StateName, _State = #coap_state{socket = Socket}) -> gen_udp:close(Socket).
code_change(_OldVsn, StateName, State = #coap_state{}, _Extra) -> {ok, StateName, State}.

init(Args) ->
    {ok, working, do_init(Args, #coap_state{}), [{next_event, internal, start}]}.
do_init([], State) -> State;
do_init([{imei, IMEI} | Args], State) -> do_init(Args, State#coap_state{imei = IMEI});
do_init([{host, Host} | Args], State) -> do_init(Args, State#coap_state{host = Host});
do_init([{port, Port} | Args], State) -> do_init(Args, State#coap_state{port = Port});
do_init([{register_payload, Payload} | Args], State) -> do_init(Args, State#coap_state{register_payload = Payload});
do_init([{lifetime, Lifetime} | Args], State) ->
    do_init(Args, State#coap_state{lifetime = list_to_binary(integer_to_list(Lifetime))});

do_init([{data_type, pass_through} | Args], State) -> do_init(Args, State#coap_state{data_type = pass_through});
do_init([{data_type, json} | Args], State) -> do_init(Args, State#coap_state{data_type = json});
do_init([{data_type, binary} | Args], State) -> do_init(Args, State#coap_state{data_type = binary});
do_init([{data_type, _} | Args], State) -> do_init(Args, State#coap_state{data_type = pass_through});

do_init([{task_list, TaskList} | Args], State) -> do_init(Args, State#coap_state{task_list = TaskList});
do_init([{task_callback, CallBack} | Args], State) -> do_init(Args, State#coap_state{task_callback = CallBack});

do_init([{socket, keep} | Args], State) -> do_init(Args, State);
do_init([{socket, new} | Args], State) ->
%%   {ok, Sock} = gen_udp:open(0, [{ip, {192,168,1,120}}, binary, {active, false}, {reuseaddr, false}]),
    {ok, Socket} = gen_udp:open(0, [binary]),
    do_init(Args, State#coap_state{socket = Socket});
do_init([{socket, Socket} | Args], State) when is_port(Socket) -> do_init(Args, State#coap_state{socket = Socket});

do_init([{_, _} | Args], State) -> do_init(Args, State).
%%--------------------------------------------------------------------------------
%% state function
%%--------------------------------------------------------------------------------
working(internal, start, #coap_state{task_list = []} = State) ->
    task_callback(State, task_list_over, execute),
    {next_state, working, State};
working(internal, start, #coap_state{task_list = [Task | TaskList]} = State) ->
    execute_task(Task, State#coap_state{task_list = TaskList});
working(Event, EventContext, State) -> handle_event(Event, EventContext, State).

wait_message(state_timeout, {AckTimeout, LastTimes, CoAPMessage}, State) when LastTimes > 1 ->
    {keep_state, send(CoAPMessage, State),
        [{state_timeout, AckTimeout,
            {AckTimeout * (?MAX_RETRANSMIT - LastTimes + 1), LastTimes - 1, CoAPMessage}}]};
wait_message(state_timeout, {_AckTimeout, LastTimes, _CoAPMessage},
    #coap_state{sampler = Sampler} = State) when LastTimes =:= 1 ->
    Sampler(message_time_out, State);
wait_message(info, {udp, _Sock, _PeerIP, _PeerPortNo, Packet}, #coap_state{sampler = Sampler} = State) ->
    {CoAPMessage, NewMessageIDState} = udp_message(Packet, State),
    Sampler(CoAPMessage, NewMessageIDState);
wait_message(Event, EventContext, State) -> handle_event(Event, EventContext, State).

handle_event(cast, Command, State) -> cast_command(Command, State);
handle_event(info, {udp, _Sock, _PeerIP, _PeerPortNo, Packet}, State) ->
    %% if no auto observe task, simulator will handle auto observe message.
    {CoAPMessage, NewMessageIDState} = udp_message(Packet, State),
    handle_message(CoAPMessage, NewMessageIDState);
handle_event(_Event, _EventContext, _State) -> keep_state_and_data.

%%--------------------------------------------------------------------------------
%% execute function
%%--------------------------------------------------------------------------------
%% from gen_statem:cast(Pid, Command)
cast_command({new_task, Task}, #coap_state{task_list = TaskList} = State) ->
    {next_state, working, State#coap_state{task_list = lists:append(TaskList, [Task])},
        [{next_event, internal, start}]};
cast_command(_UnKnowCommand, _State) -> keep_state_and_date.

udp_message(Packet, State) ->
    try coap_message_util:decode(Packet) of
        {ok, CoAPMessage} ->
            io:format("receive: <<<<<< ~0p~n~n", [CoAPMessage]),
            {CoAPMessage, fresh_coap_state(CoAPMessage, State)};
        {error, _} -> {#coap_message{}, State}
    catch _:_  -> {#coap_message{}, State}
    end.

%%--------------------------------------------------------------------------------
%% execute task
%%--------------------------------------------------------------------------------
execute_task(#task{action = close} = Task, State) ->
    task_callback(State, Task, execute),
    task_callback(State, Task, success),
    {stop, {shutdown, command}, State};
execute_task(#task{action = new_mei, args = Args}, State) ->
    {next_state, working, do_init(Args, State), [{next_event, internal, start}]};
execute_task(#task{action = auto_observe} = Task, State) ->
    task_callback(State, Task, execute),
    {next_state, wait_message, State#coap_state{sampler = auto_observe_sampler(Task)},
        [{state_timeout, ?ACK_TIMEOUT, {?ACK_TIMEOUT, 1, no_request}}]};
execute_task(#task{action = publish} = Task, #coap_state{token_19_0_0 = undefined} = State) ->
    task_callback(State, Task, {fail, auto_observe_19_0_0_fail}),
    {next_state, working, State};
execute_task(Task, State) ->
    task_callback(State, Task, execute),
    CoAPMessage = build_message(Task, State),
    Sampler = find_sampler(Task),
    send_request(CoAPMessage, State#coap_state{sampler = Sampler}).

-spec task_callback(#coap_state{}, #task{}, execute | success | {fail, Reason :: term()})-> ignore.
task_callback(#coap_state{task_callback = {Function, Args}}, Task, Result) ->
    try Function(Task, Result, Args), ignore
    catch _:_  -> ignore
    end;
task_callback(#coap_state{task_callback = undefined}, _Task, _Result) -> ignore;
task_callback(#coap_state{task_callback = _}, _Task, _Result) -> ignore.


find_sampler(#task{action = register} = Task)    -> method_sampler(Task, ?CREATED);
find_sampler(#task{action = de_register} = Task) -> method_sampler(Task, ?DELETED);
find_sampler(#task{action = publish} = Task)     -> method_sampler(Task, ?EMPTY).

method_sampler(Task, Method)->
    fun
        (#coap_message{id = MsgID, method = AckMethod}, #coap_state{current_request_id = MsgID} = State) ->
            case AckMethod =:= Method of
                true ->
                    task_callback(State, Task, success),
                    {next_state, working, State, [{state_timeout, cancel}, {next_event, internal, start}]};
                false ->
                    task_callback(State, Task, {fail, AckMethod}),
                    {next_state, working, State, [{state_timeout, cancel}]}
            end;
        (CoAPMessage, _State) when is_record(CoAPMessage, coap_message) -> keep_state_and_data;
        (message_time_out, State) ->
            task_callback(State, Task, {fail, message_time_out}),
            {next_state, working, State, [{state_timeout, cancel}]}
    end.

auto_observe_sampler(Task) ->
    fun
        (#coap_message{type = ?CON, method = ?GET} = CoAPMessage, State) ->
            case handle_auto_observe(CoAPMessage, State) of
                {ok, NewState} ->
                    task_callback(State, Task, success),
                    {next_state, working, NewState, [{state_timeout, cancel}, {next_event, internal, start}]};
                ignore -> keep_state_and_data
            end;
        (message_time_out, State) ->
            task_callback(State, Task, {fail, auto_observe_timeout}),
            {next_state, working, State, [{state_timeout, cancel}]};
        (_, _) -> keep_state_and_data
    end.

%%--------------------------------------------------------------------------------
%%  simulator action
%%--------------------------------------------------------------------------------
-spec register(pid()) -> any().
register(Pid) ->
    gen_statem:cast(Pid, {new_task, #task{action = register}}).

-spec update_register(pid()) -> any().
update_register(Pid) ->
    gen_statem:cast(Pid, {new_task, #task{action = update_register}}).

-spec de_register(pid()) -> any().
de_register(Pid) ->
    gen_statem:cast(Pid, {new_task, #task{action = de_register}}).

%%   binary payload, BuildPayload = <<16#02:2, DatasetID:2, (size(Payload)):2, Payload/binary>>,
-spec publish(pid(), binary()) -> any().
publish(Pid, PublishData) ->
    gen_statem:cast(Pid, {new_task, #task{action = publish, args = PublishData}}).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_statem:cast(Pid, {new_task, #task{action = close}}).

new_imei(Pid, IMEI, TaskList)->
    gen_statem:cast(Pid, {new_task,
        #task{action = new_mei, args = [{imei, IMEI}, {socket, keep}, {task_list, TaskList}]}
    }).


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
handle_message(#coap_message{type = ?CON, method = ?GET} = CoAPMessage, State) ->
    case handle_auto_observe(CoAPMessage, State) of
        {ok, NewState} -> {keep_state, NewState};
        _ -> keep_state_and_data
    end;
handle_message(#coap_message{type = ?RESET}, _State) ->
    %% todo
    all_fail,
    keep_state_and_data;
handle_message(_CoAPMessage, _State) -> keep_state_and_data.

handle_auto_observe(#coap_message{id = MessageID, token = Token} = CoAPMessage, State) ->
    {ok, Path} = coap_message_util:get_uri_path(CoAPMessage),
    case Path of
        <<"/3/0">> ->
            {ok, send(lwm2m_message_util:response_auto_observe_3_0(MessageID, Token), State)};
        <<"/19/0/0">> ->
            {ok, send(lwm2m_message_util:response_auto_observe_19_0_0(MessageID, Token),
                State#coap_state{token_19_0_0 = Token})};
        <<"/4/0/8">> ->
            {ok, send(lwm2m_message_util:response_auto_observe_4_0_8(MessageID, Token), State)};
        _ -> ignore
    end.

%%--------------------------------------------------------------------------------
%%  build message
%%--------------------------------------------------------------------------------
build_message(#task{action = Action, args = Args}, State) ->
    build_message(Action, Args, State).
build_message(register, _Args,
    #coap_state{
        message_id_index = MessageID,
        lifetime = LifeTime,
        imei = IMEI,
        register_payload = RegisterPayload}) ->
    lwm2m_message_util:register(MessageID, IMEI, LifeTime, RegisterPayload);
build_message(register_standard_module, _Args,
    #coap_state{
        message_id_index = MessageID,
        lifetime = LifeTime,
        imei = IMEI,
        register_payload = RegisterPayload}) ->
    lwm2m_message_util:register_standard_module(MessageID, IMEI, LifeTime, RegisterPayload);
build_message(de_register, _Args,
    #coap_state{message_id_index = MessageID, imei = IMEI}) ->
    lwm2m_message_util:deregister(MessageID, IMEI);
build_message(publish, Payload,
    #coap_state{message_id_index = MessageID, data_type = ProductDataType,  token_19_0_0 = Token}) ->
    lwm2m_message_util:publish(ProductDataType, MessageID, Token, Payload).

%%--------------------------------------------------------------------------------
%%  send request
%%--------------------------------------------------------------------------------
send_request(#coap_message{id = RequestID} = CoAPMessage, State) ->
    NewMessageIDState = fresh_coap_state(CoAPMessage, State),
    {next_state, wait_message,
        send(CoAPMessage, NewMessageIDState#coap_state{current_request_id = RequestID}),
        [{state_timeout, ?ACK_TIMEOUT, {?ACK_TIMEOUT, ?MAX_RETRANSMIT, CoAPMessage}}]}.

%%--------------------------------------------------------------------------------
%%  udp api
%%--------------------------------------------------------------------------------
%% will fresh message id and uri observe before send.
-spec send(#coap_message{}, #coap_state{}) -> #coap_state{}.
send(CoAPMessage, #coap_state{socket = Socket, host = Host, port = Port} = State) ->
    io:format("send: >>>>>> ~0p~n~n", [CoAPMessage]),
    {ok, Package} = coap_message_util:encode(CoAPMessage),
    gen_udp:send(Socket, Host, Port, Package),
    fresh_coap_state(CoAPMessage, State).
