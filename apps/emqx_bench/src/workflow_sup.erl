-module(workflow_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

-export([init/1]).

-export([count/3, status/1, tasks_list/1]).

-export([start_sims/1]).

start_link(WorkflowID) ->
    supervisor:start_link({local, binary_to_atom(WorkflowID)}, ?MODULE, WorkflowID).

-define(Protocol2SimworkerMap,
    #{<<"lwm2m">> => lw_coap_sim_worker,
        <<"mqtt">> => mqtt_sim_worker}).

%% task status: un_executed success timeout fail
-define(UN_EXECUTE, un_executed).
-define(SUCCESS, success).
-define(TIMEOUT, timeout).
-define(FAIL, fail).


init(WorkflowID) ->
    Workflow = properties_manager:get_workflow(WorkflowID),
    {ok, Protocol} = maps:find(<<"protocol">>, Workflow),
    {ok, SimWorker} = maps:find(Protocol, ?Protocol2SimworkerMap),
    SupervisorSpecification =
        #{strategy => simple_one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
            intensity => 10,
            period => 60},
    ChildSpecifications = [
        #{id => SimWorker,
            start => {SimWorker, start_link, []},
            restart =>
            temporary, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [SimWorker]}],
    {ok, {SupervisorSpecification, ChildSpecifications}}.


start_sims(WorkFlowID) ->
    Workflow = prepare_workflow(WorkFlowID),
    ClientInfoList = properties_manager:get_wf_client_list(WorkFlowID),
    WorkFlowID_pid = binary_to_atom(WorkFlowID),
    init_counter(Workflow, WorkFlowID),
    spawn(fun() -> start_sim(WorkFlowID_pid, ClientInfoList, Workflow) end).


start_sim(WorkFlowID, ClientInfoList, Workflow) ->
    case ClientInfoList of
        [] -> ok;
        [ClientInfo | Tail] ->
            case supervisor:start_child(WorkFlowID, [ClientInfo, Workflow]) of
                {ok, _} ->
                    logger:info("~p start sim success: ~p", [?MODULE, ClientInfo]),
                    start_sim(WorkFlowID, Tail, Workflow);
                Reason ->
                    logger:error("~p start sim fail: ~p in workflow ~p ~p", [?MODULE, ClientInfo, WorkFlowID, Reason]),
                    start_sim(WorkFlowID, Tail, Workflow)
            end
    end.

init_counter(Workflow, WorkFlowID) ->
    Len = 4 * erlang:length(maps:get(<<"work_flow">>, Workflow)),
    CounterRef = counters:new(Len, [write_concurrency]),
    persistent_term:put(WorkFlowID, CounterRef).

count(WorkFlowID, TaskStatusMap, TaskExecutedIndex) ->
    CounterRef = persistent_term:get(WorkFlowID),
    count_all(CounterRef, TaskStatusMap, TaskExecutedIndex).

count_all(CounterRef, TaskStatusMap, TasksListSize) when TasksListSize > 0 ->
    case maps:find(TasksListSize, TaskStatusMap) of
        {ok, ?UN_EXECUTE} ->
            counters:add(CounterRef, ((TasksListSize - 1) * 4 + 1), 1);
        {ok, ?SUCCESS} ->
            counters:add(CounterRef, ((TasksListSize - 1) * 4 + 2), 1);
        {ok, ?TIMEOUT} ->
            counters:add(CounterRef, ((TasksListSize - 1) * 4 + 3), 1);
        {ok, ?FAIL} ->
            counters:add(CounterRef, ((TasksListSize - 1) * 4 + 4), 1);
        _ -> counters:add(CounterRef, ((TasksListSize - 1) * 4 + 1), 1)
    end,
    count_all(CounterRef, TaskStatusMap, TasksListSize - 1);
count_all(_CounterRef, _TaskStatusMap, TasksListSize) when TasksListSize =:= 0 ->
    ok.

tasks_list(WorkFlowID) ->
    maps:get(<<"work_flow">>, properties_manager:get_workflow(WorkFlowID)).

status(WorkFlowID) ->
    persistent_term:get(WorkFlowID).

prepare_workflow(WorkFlowID) ->
    WorkflowLoading = properties_manager:get_workflow(WorkFlowID),
    TasksList = add_tasks_list_index(maps:get(<<"work_flow">>, WorkflowLoading)),
    maps:put(task_list_size, tasks_list_size(TasksList), maps:put(<<"work_flow">>, TasksList, WorkflowLoading)).

add_tasks_list_index(TasksList) ->
    add_tasks_list_index(TasksList, 1).

add_tasks_list_index(TasksList, TaskIndex) ->
    case TasksList of
        [Task | Tail] -> [maps:put(index, TaskIndex, Task) | add_tasks_list_index(Tail, TaskIndex + 1)];
        [] -> []
    end.

tasks_list_size(TasksList) ->
    list_size(TasksList).


list_size(List) ->
    list_size(List, 0).
list_size(List, SizeCount) ->
    case List of
        [] -> SizeCount;
        [_ | Tail] -> list_size(Tail, SizeCount + 1)
    end.
