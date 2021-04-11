%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 3月 2021 1:44 下午
%%%-------------------------------------------------------------------
-module(workflow_read).
-author("DDDHuang").

%% API
-export([get/1]).
-include("emqx_bench.hrl").


test() ->
    Path = "/Users/huangdi/Documents/code/EMQ/emqx_bench/data/workflow_lw.json",
    WorkflowJson = iolist_to_binary(read(Path)),
    Data = jsx:decode(WorkflowJson, [return_maps]),
    io:format("~p ~n----------------------", [Data]),
    WorkFlow = trans(Data),
    io:format("trans ~n ~p~n~n", [WorkFlow]).

get(Path)->
    WorkflowJson = iolist_to_binary(read(Path)),
    Data = jsx:decode(WorkflowJson, [return_maps]),
    trans(Data).

read(Path) ->
    {ok, FileRef} = file:open(Path, [read]),
    read_line(FileRef).
read_line(FileRef) ->
    case file:read_line(FileRef) of
        eof -> [];
        {ok, Line} ->
            Line ++ read_line(FileRef)
    end.

trans(WorkflowMap) ->
    Keys = maps:keys(WorkflowMap),
    trans(Keys, WorkflowMap, #work_flow{}).
trans([<<"test_id">> = Key | Keys], WorkflowMap, Workflow) ->
    trans(Keys, WorkflowMap, Workflow#work_flow{test_id = maps:get(Key, WorkflowMap)});
trans([<<"protocol">> = Key | Keys], WorkflowMap, Workflow) ->
    Protocol = binary_to_atom(maps:get(Key, WorkflowMap), utf8),
    trans(Keys, WorkflowMap, Workflow#work_flow{protocol = Protocol});
trans([<<"simulator_config">> = Key | Keys], WorkflowMap, Workflow) ->
    SimulatorConfigMap = maps:get(Key, WorkflowMap),
    trans(Keys, WorkflowMap, Workflow#work_flow{simulator_config = trans_to_atom_map(SimulatorConfigMap)});
trans([<<"task_list">> = Key | Keys], WorkflowMap, Workflow) ->
    TaskMapList = maps:get(Key, WorkflowMap),
    trans(Keys, WorkflowMap, Workflow#work_flow{task_list = trans_task(TaskMapList)});
trans([_ | Keys], WorkflowMap, Workflow) ->
    trans(Keys, WorkflowMap, Workflow);
trans([], _WorkflowMap, Workflow) -> Workflow.

trans_task(TaskMapList) when is_list(TaskMapList) ->
    [trans_task(TaskMap) || TaskMap <- TaskMapList];
trans_task(TaskMap) when is_map(TaskMap) ->
    Keys = maps:keys(TaskMap),
    trans_task(Keys, TaskMap, #task{}).
trans_task([<<"action">> = Key | Keys], TaskMap, Task) ->
    Value = binary_to_atom(maps:get(Key, TaskMap), utf8),
    trans_task(Keys, TaskMap, Task#task{action = Value});
trans_task([<<"args">> = Key | Keys], TaskMap, Task) ->
    Value = maps:get(Key, TaskMap),
    trans_task(Keys, TaskMap, Task#task{args = Value});
trans_task([_ | Keys], TaskMap, Task) ->
    trans_task(Keys, TaskMap, Task);
trans_task([], _TaskMap, Task) -> Task.



trans_to_atom_map(Map) ->
    Result = maps:new(),
    Keys = maps:keys(Map),
    trans_to_atom_map(Keys, Map, Result).
trans_to_atom_map([Key | Keys], Map, Result) ->
    trans_to_atom_map(Keys, Map, maps:put(binary_to_atom(Key, utf8), maps:get(Key, Map), Result));
trans_to_atom_map([], _Map, Result) -> Result.
