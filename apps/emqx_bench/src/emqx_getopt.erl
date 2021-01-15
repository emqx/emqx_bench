-module(emqx_getopt).

-export([command/1]).

-define(RUN_OPTS,
    [{help, $h, "help", boolean, "help information"},
        {workflowid,
            $w,
            "workflowid",
            string,
            "Chose workflow by woriflow_id (define "
            "in workflow file ,josn key [ workflow_id "
            "]). if not specified ,run all workflow "
            ""}]).

-define(CLEAR_OPTS,
    [{help, undefined, "help", boolean, "help information"},
        {workflowid,
            $w,
            "workflowid",
            string,
            "Stop workflow by woriflow_id (define "
            "in workflow file ,josn key [ workflow_id "
            "]). if not specified ,stop all workflow "
            ""}]).

-define(STATUS_OPTS,
    [{help, undefined, "help", boolean, "help information"},
        {workflowid,
            $w,
            "workflowid",
            string,
            "Show workflow status by woriflow_id "
            "(define in workflow file ,josn key [ "
            "workflow_id ]). if not specified ,list "
            "all workflow "}]).

-define(TAB, ?MODULE).

-define(IDX_SENT, 1).

-define(IDX_RECV, 2).

command(["load", WorkflowFile]) ->
    properties_manager:load_properties({fromfile, WorkflowFile}),
    io:format("~nLoading profiles into memory...~p~n",
        [WorkflowFile]);
command(["load" | _]) ->
    io:format("Usage: ./emqx_bench load <workflow>.json~n");
command(["run" | Argv]) ->
    {ok, {Opts, _Args}} = getopt:parse(?RUN_OPTS, Argv),
    ok = maybe_help(run, Opts),
    case properties_manager:is_ready() of
        true ->
            WorkFlowId = proplists:get_value(workflowid, Opts, all),
            io:format("Running......"),
            workflow_manager_sup:start_workflow(WorkFlowId);
        false ->
            io:format("Not initialized. Please do 'emqx_bench "
            "load <workflow-file>.json' first!~n")
    end;
%%command(["resume" | _]) ->
%%    % todo  protocol support ?
%%    workflow_manager_sup:resume_sim_groups();
command(["clear" | Argv]) ->
    {ok, {Opts, _Args}} = getopt:parse(?CLEAR_OPTS, Argv),
    ok = maybe_help(clear, Opts),
    WorkFlowId = proplists:get_value(workflowid, Opts, all),
    io:format("~nStop workflow ~p~n", [WorkFlowId]),
    workflow_manager_sup:stop_workflow(WorkFlowId);
command(["status" | Argv]) ->
    {ok, {Opts, _Args}} = getopt:parse(?STATUS_OPTS, Argv),
    ok = maybe_help(status, Opts),
    WorkFlowId = proplists:get_value(workflowid, Opts, all),
    print_table_title(WorkFlowId).

print_table_title(all) ->
    WorkFlowIdList = properties_manager:get_workflow_id_list(),
    print_table_title(WorkFlowIdList);
print_table_title(WorkFlowIdList) when is_list(WorkFlowIdList) ->
    io:format("~60..=s~n", [""]),
    io:format("~-20.. s ~s~n", ["workflowd ID", "client count"]),
    io:format("~60..-s~n", [""]),
    [begin
         ClientSum = properties_manager:count_client(WorkFlowId),
         io:format("~s~-20.. s ~s~p~n", ["\e[0;34m", WorkFlowId, "\e[0;38m", ClientSum])
     end
        || WorkFlowId <- WorkFlowIdList],
    io:format("~60..=s~n", [""]),
    io:format("~-15.. s~-12.. s ~-12.. s ~-12.. s ~s~-12.. s~s~n", ["task", "unexecuted", "timeout", "fail", "\e[0;32m", "success", "\e[0;38m"]),
    [begin
         io:format("~s~-40.. s~n~s", ["\e[0;34m", WorkFlowId, "\e[0;38m"]),
         TasksList = workflow_sup:tasks_list(WorkFlowId),
         StatusCounterRef = workflow_sup:status(WorkFlowId),
         ClientSum = properties_manager:count_client(WorkFlowId),
         print_task_detail(TasksList, StatusCounterRef, ClientSum)
     end || WorkFlowId <- WorkFlowIdList],
    ok.

print_task_detail(TasksList, StatusCounterRef, ClientSum) ->
    print_task_detail(TasksList, StatusCounterRef, 1, ClientSum).
print_task_detail(TasksList, StatusCounterRef, TaskAccIndex, ClientSum) ->
    case TasksList of
        [] -> ok;
        [Task | Tail] ->
            UnExecute = counters:get(StatusCounterRef, TaskAccIndex),
            Success = counters:get(StatusCounterRef, TaskAccIndex + 1),
            TimeOut = counters:get(StatusCounterRef, TaskAccIndex + 2),
            Fail = counters:get(StatusCounterRef, TaskAccIndex + 3),
            TaskName = maps:get(<<"task">>, Task),
            io:format("~-15.. s", [TaskName]),

            print_task_count(UnExecute),
            print_task_count(TimeOut),
            print_task_count(Fail),
            io:format("~s~-12.. w~s~n", ["\e[0;32m", Success, "\e[0;38m"]),
            io:format("~8.. s", [""]),
            print_task_count_percent(UnExecute, ClientSum),
            print_task_count_percent(TimeOut, ClientSum),
            print_task_count_percent(Fail, ClientSum),
            print_task_count_percent_green(Success, ClientSum),
            io:format("~n~-60..-s~n", [""]),
            print_task_detail(Tail, StatusCounterRef, TaskAccIndex + 4, ClientSum)
    end.


print_task_count(Count) when Count > 0 ->
    io:format("~s~-12.. w~s ", ["\e[0;31m", Count, "\e[0;38m"]);
print_task_count(Count) ->
    io:format("~-12.. w ", [Count]).

print_task_count_percent_green(TaskCount, ClientSum) when (TaskCount * 100 / ClientSum) < 10.0 ->
    io:format("~s~11.2. f% ~s", ["\e[0;32m", TaskCount * 100 / ClientSum, "\e[0;38m"]);
print_task_count_percent_green(TaskCount, ClientSum) when (TaskCount * 100 / ClientSum) >= 10.0 ->
    io:format("~s~12.2. f%~s", ["\e[0;32m", TaskCount * 100 / ClientSum, "\e[0;38m"]).

print_task_count_percent(TaskCount, ClientSum) when (TaskCount * 100 / ClientSum) < 10.0 ->
    io:format("~11.2. f% ", [TaskCount * 100 / ClientSum]);
print_task_count_percent(TaskCount, ClientSum) when (TaskCount * 100 / ClientSum) >= 10.0 ->
    io:format("~s~12.2. f%~s", ["\e[0;31m", TaskCount * 100 / ClientSum, "\e[0;38m"]).



maybe_help(PubSub, Opts) ->
    case proplists:get_value(help, Opts) of
        true ->
            usage(PubSub),
            halt(0);
        _ -> ok
    end.

usage(PubSub) ->
    getopt:usage(
        case PubSub of
            run -> ?RUN_OPTS;
            clear -> ?CLEAR_OPTS;
            status -> ?STATUS_OPTS
        end, "./emqx_bench " ++ atom_to_list(PubSub)).
