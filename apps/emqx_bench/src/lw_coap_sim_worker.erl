-module(lw_coap_sim_worker).
%%create time :20.07.27 10:49
-behaviour(gen_statem).

-export([callback_mode/0, init/1, terminate/3]).

-export([start_link/2]).

-export([working/3, waiting_message/3, sleeping/3]).

-record(lw_coap_data,
{
    wf_id,
    socket,
    server_ip,
    server_port,
    client_info,
    tasks_list,
    mid = 0,
    uri_observe_index = 0,
    location,
    token_1900,
%%  validator should fresh_message_id()  because all message received should pass validator
    validator :: fun((term()) -> {ok, #lw_coap_data{}} | {error, term()} | {ignore, term()}),
    tasks_status_map,
    task_now_index,
    task_list_size
}).

-include_lib("lwm2m_coap/include/coap.hrl").

%% task status: un_executed success fail
-define(UN_EXECUTE, un_executed).
-define(SUCCESS, success).
-define(TIMEOUT, timeout).
-define(FAIL, fail).

start_link(ClientInfo, Workflow) ->
    gen_statem:start_link(?MODULE,
        [ClientInfo, Workflow],
        []).

init([ClientInfo, Workflow]) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    {ok, working,
        #lw_coap_data{
            wf_id = maps:get(<<"workflow_id">>, Workflow),
            client_info = ClientInfo,
            socket = Socket,
            server_ip = maps:get(<<"server_ip">>, Workflow),
            server_port = maps:get(<<"server_port">>, Workflow),
            tasks_list = maps:get(<<"work_flow">>, Workflow),
            tasks_status_map = maps:new(),
            task_list_size = maps:get(task_list_size, Workflow)
        },
        [{next_event, internal, continue_workflow}]}.

callback_mode() -> state_functions.


terminate(_Reason, State, #lw_coap_data{tasks_status_map = Maps, wf_id = WorkFlowID, task_list_size = TasksSize}) ->
    workflow_sup:count(WorkFlowID, Maps, TasksSize).

working(internal, continue_workflow, #lw_coap_data{tasks_list = TasksList} = Data) ->
    case TasksList of
        [] ->
            {stop, {shutdown, Data}};
        [Task | Tail] ->
            process_task(Task, Data#lw_coap_data{tasks_list = Tail})
    end;

working(info, {udp, _Sock, _PeerIP, _PeerPortNo, Packet}, Data) ->
    try
        lwm2m_coap_message_parser:decode(Packet)
    of
        CoapMsg ->
            MessageId = coap_bench_message:id(CoapMsg),
            {keep_state, fresh_message_id(MessageId, Data)}
    catch
        _:_ ->
            logger:error("~p received unknown udp message : ~p", [?MODULE, Packet]),
            keep_state_and_data
    end.


waiting_message(info, {udp, _Sock, _PeerIP, _PeerPortNo, Packet}, #lw_coap_data{validator = Validator} = Data) when is_function(Validator) ->
    try
        lwm2m_coap_message_parser:decode(Packet)
    of
        CoapMsg ->
            case Validator(CoapMsg, Data) of
                {ok, NewData} ->
                    {next_state, working,
                        task_success(NewData#lw_coap_data{validator = undefined}), [{next_event, internal, continue_workflow}]};
                {ignore, _} ->
                    keep_state_and_data;
                {error, Reason} ->
                    logger:error("~p response ERROR ~p", [?MODULE, Reason]),
                    {stop, {shutdown, error_message}, task_fail(Data)}
            end
    catch
        _:_ ->
            logger:error("~p received unknown udp message : ~p", [?MODULE, Packet]),
            keep_state_and_data
    end;

waiting_message(timeout, message_timeout, #lw_coap_data{client_info = ClientInfo} = Data) ->
    {stop, {shutdown, message_timeout}, task_timeout(Data)}.


sleeping(state_timeout, wakeup, Data) ->
    {next_state, working, task_success(Data), [{next_event, internal, continue_workflow}]};
sleeping(_Event, _EventContent, _Data) ->
    keep_state_and_data.


process_task(#{<<"task">> := TaskName} = Task,
    #lw_coap_data{socket = Socket, client_info = Ep, server_ip = Host, server_port = Port, mid = MsgId} = StartData) ->
    Data = StartData#lw_coap_data{task_now_index = maps:get(index, Task)},
    case TaskName of
        <<"register">> ->
            LifeTime = maps:get(<<"lifetime">>, Task),
            ObjectLinks = maps:get(<<"object_links">>, Task),
            TimeOut = maps:get(<<"timeout">>, Task),
            Validator = fun
                            (ReceivedMsg, StateData) ->
                                case {coap_bench_message:ack_validator(ReceivedMsg, MsgId),
                                    coap_bench_message:method(ReceivedMsg),
                                    coap_bench_message:location_path(ReceivedMsg)} of
                                    {false, _, _} ->
                                        {ignore, not_care_message};
                                    {true, {ok, created}, LocationPath} ->
                                        {ok, StateData#lw_coap_data{location = LocationPath}};
                                    {true, _, _} -> {error, register_fail}
                                end
                        end,
            RegisterMessage_Coap = coap_bench_message:make_register(list_to_binary(Ep), LifeTime, MsgId, ObjectLinks),
            send_lw_request(Socket, Host, Port, RegisterMessage_Coap),
            {next_state, waiting_message, fresh_message_id(Data#lw_coap_data{validator = Validator}), [{timeout, TimeOut, message_timeout}]};
        <<"wait_observe">> ->
            Validator_30 = fun(ValidatorMsg, ValidatorData) -> handle_observe_30(ValidatorMsg, ValidatorData) end,
            Validator_1900 = fun(ValidatorMsg, ValidatorData) -> handle_observe_1900(ValidatorMsg, ValidatorData) end,
            TimeOut = maps:get(<<"timeout">>, Task),
            case maps:get(<<"path">>, Task) of
                <<"/3/0">> ->
                    {next_state, waiting_message, Data#lw_coap_data{validator = Validator_30}, [{timeout, TimeOut, message_timeout}]};
                <<"19/0/0">> ->
                    {next_state, waiting_message, Data#lw_coap_data{validator = Validator_1900}, [{timeout, TimeOut, message_timeout}]}
            end;
        <<"notify">> ->
            NotifyToken = Data#lw_coap_data.token_1900,
            ObserveIndex = ((Data#lw_coap_data.uri_observe_index) + 1),
            case maps:find(<<"body">>, Task) of
                {ok, PayLoad} ->
                    Notify = coap_bench_message:make_notify(MsgId, NotifyToken, ObserveIndex, PayLoad),
                    send_lw_request(Socket, Host, Port, Notify),
                    {next_state, working, task_success(fresh_message_id(Data#lw_coap_data{uri_observe_index = ObserveIndex})), [{next_event, internal, continue_workflow}]};
                _error ->
                    logger:error("~p notify fail param not found ~p", [?MODULE, Ep]),
                    {stop, {shutdown, wrong_task}, task_fail(Data)}
            end;
        <<"sleep">> ->
            {ok, TimeOut} = maps:find(<<"interval">>, Task),
            {next_state, sleeping, Data, [{state_timeout, TimeOut, wakeup}]};
        <<"deregister">> ->
            Validator = fun
                            (ReceivedMsg, StateData) ->
                                case coap_bench_message:ack_validator(ReceivedMsg, MsgId) of
                                    true ->
                                        {ok, fresh_message_id(StateData#lw_coap_data{validator = undefined})};
                                    false ->
                                        {ignore, not_care_message}
                                end
                        end,
            TimeOut = maps:get(<<"timeout">>, Task),
            Location = Data#lw_coap_data.location,
            LogoutResponse = coap_bench_message:make_deregister(Location, MsgId),
            send_lw_request(Socket, Host, Port, LogoutResponse),
            {next_state, waiting_message, fresh_message_id(Data#lw_coap_data{validator = Validator}), [{timeout, TimeOut, message_timeout}]};
        _ ->
            logger:error("~p ignore task ~p", [?MODULE, TaskName]),
            {next_state, working, Data, [{next_event, internal, continue_workflow}]}
    end.

fresh_message_id(#lw_coap_data{mid = MessageId} = Data) ->
    fresh_message_id(MessageId + 1, Data).

fresh_message_id(NewMid, #lw_coap_data{} = Data) ->
    Data#lw_coap_data{mid = NewMid}.


handle_observe_30(ReceivedMsg, #lw_coap_data{socket = Socket, server_ip = Host, server_port = Port} = StateData) ->
    MessageID = coap_bench_message:id(ReceivedMsg),
    Token = coap_bench_message:token(ReceivedMsg),
    UriObserveIndex = coap_bench_message:find_option_value(observe, ReceivedMsg) + 1,
    case list_to_binary(coap_bench_message:uri_path(ReceivedMsg)) of
        <<"30">> ->
            Response = coap_bench_message:make_response_observe_3_0(MessageID, Token, UriObserveIndex),
            send_lw_request(Socket, Host, Port, Response),
            {ok, fresh_message_id(StateData#lw_coap_data{validator = undefined, uri_observe_index = UriObserveIndex})};
        _ ->
            {ignore, wrong_message}
    end.


handle_observe_1900(ReceivedMsg, #lw_coap_data{socket = Socket, server_ip = Host, server_port = Port} = StateData) ->
    MessageID = coap_bench_message:id(ReceivedMsg),
    Token = coap_bench_message:token(ReceivedMsg),
    UriObserveIndex = coap_bench_message:find_option_value(observe, ReceivedMsg) + 1,
    case list_to_binary(coap_bench_message:uri_path(ReceivedMsg)) of
        <<"1900">> ->
            Response = coap_bench_message:make_response_observe_19_0_0(MessageID, Token, UriObserveIndex),
            send_lw_request(Socket, Host, Port, Response),
            {ok, fresh_message_id(StateData#lw_coap_data{validator = undefined, uri_observe_index = UriObserveIndex, token_1900 = Token})};
        _ ->
            {ignore, wrong_message}
    end.


%% -------------------------------------------
%% simulator SDK
%% -------------------------------------------


send_lw_request(Socket, Host, Port, CoapMessage) when is_binary(Host) ->
    send_lw_request(Socket, binary_to_list(Host), Port, CoapMessage);
send_lw_request(Socket, Host, Port, CoapMessage) when is_list(Host) ->
    LWMessage = lwm2m_coap_message_parser:encode(CoapMessage),
    gen_udp:send(Socket, Host, Port, LWMessage).

%% -------------------------------------------
%% count tool
%% -------------------------------------------


task_success(Counter) ->
    task_status(?SUCCESS, Counter).
task_fail(Counter) ->
    task_status(?FAIL, Counter).
task_timeout(Counter) ->
    task_status(?TIMEOUT, Counter).

task_status(TaskStatus, #lw_coap_data{task_now_index = TaskIndex, tasks_status_map = Map} = Counter) ->
    Counter#lw_coap_data{tasks_status_map = maps:put(TaskIndex, TaskStatus, Map)}.




