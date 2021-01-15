-module(workflow_manager_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([init/1]).

-export([start_workflow/1, stop_workflow/1, work_status/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification = #{strategy =>
    simple_one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 10, period => 60},
    ChildSpecifications = [#{id => workflow_sup,
        start => {workflow_sup, start_link, []},
        restart =>
        permanent, % permanent | transient | temporary
        shutdown => 2000,
        type => worker, % worker | supervisor
        modules => [workflow_sup]}],
    {ok, {SupervisorSpecification, ChildSpecifications}}.

start_workflow(all) ->
    start_workflow(properties_manager:get_workflow_id_list());

start_workflow(WorkFlowIDList) when is_list(WorkFlowIDList) ->
    [start_workflow(WorkFlowID) || WorkFlowID <- WorkFlowIDList];

start_workflow(WorkFlowID) when is_binary(WorkFlowID) ->
    case supervisor:start_child(?MODULE, [WorkFlowID]) of
        {ok, _} ->
            logger:debug("~p start workflow sup success : ~p", [?MODULE, WorkFlowID]),
            workflow_sup:start_sims(WorkFlowID);
        _ ->
            logger:error("[~p] start workflow sup ~p fial", [?MODULE, WorkFlowID])
    end.

stop_workflow(all) -> ok;

stop_workflow(WorkFlowList) when is_list(WorkFlowList) ->
    [stop_workflow(WorkFlow) || WorkFlow <- WorkFlowList];

stop_workflow(WorkFlowId) when is_binary(WorkFlowId) ->
    ok.

work_status(all) ->
    work_status(properties_manager:get_workflow_id_list());
work_status(WorkFlowIdList) when is_list(WorkFlowIdList) ->
    [work_status(WorkFlowId) || WorkFlowId <- WorkFlowIdList];
work_status(WorkFlowId) when is_binary(WorkFlowId) ->
    {}.


%%taskslist_status(all) ->
%%    taskslist_status(properties_manager:get_workflow_id_list());
%%taskslist_status(WorkFlowIdList) when is_list(WorkFlowIdList) ->
%%    [taskslist_status(WorkFlowId) || {WorkFlowId} <- WorkFlowIdList];
%%taskslist_status(WorkFlowId) when is_binary(WorkFlowId) ->
%%    {workflow_sup:tasks_list(WorkFlowId), workflow_sup:status(WorkFlowId)}.


