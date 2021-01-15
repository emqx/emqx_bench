-module(properties_manager).

-export([get_wf_client_list/1,
    get_workflow/1,
    get_workflow_by_workflowId_atom/1,
    load_properties/1]).

-export([get_workflow_id_list/0, count_client/1]).

-export([init/0, is_ready/0, stop/0]).

-define(WorkFlowTable, workflowid_workflow).

-define(ClientListTable, workflowid_clientsinfo).


init() ->
    ets:new(?WorkFlowTable, [set, public, named_table]),
    ets:new(?ClientListTable, [set, public, named_table]),
    ok.

stop() ->
    ets:delete(?WorkFlowTable),
    ets:delete(?ClientListTable),
    ok.

is_ready() -> ets:info(?WorkFlowTable, size) > 0.

% Properties = {fromfile,FilePath} | {fromapi,ApiArgs}
load_properties(Properties) ->
    {WorkFlowLoadStatus, WorkFlowList} =
        load_workflow_list(Properties),
    store_workflow(WorkFlowList),
    {WorkFlowLoadStatus,
        store_client_list_for_each_workflow(WorkFlowList)}.

store_workflow(WorkFlowList) ->
    case WorkFlowList of
        [] -> ok;
        [WorkFlow | Tail] ->
            WorkFlowId = maps:get(<<"workflow_id">>, WorkFlow),
            ets:insert_new(?WorkFlowTable, {WorkFlowId, WorkFlow}),
            store_workflow(Tail)
    end.

load_workflow_list(LoadProperties) ->
    case LoadProperties of
        {fromfile, WorkflowFilePath} ->
            {FileLoadStatus, WorkFlowFileLinesLoadResult} = load_file_lines(WorkflowFilePath),
            case FileLoadStatus of
                ok ->
                    WorkflowList = jsx:decode(iolist_to_binary(WorkFlowFileLinesLoadResult), [return_maps]),
                    logger:debug("~p :load workflow from file [ ~p ] success", [?MODULE, WorkflowFilePath]),
                    {ok, WorkflowList};
                error ->
                    logger:error("~p :load workflow from file [ ~p ] fail [ ~p ]", [?MODULE, WorkflowFilePath, WorkFlowFileLinesLoadResult]),
                    {FileLoadStatus, WorkFlowFileLinesLoadResult}
            end;
        {fromapi, _ApiArgs} ->
            % todo :  this should Match customer API
            % DataBase ,HTTP/s API ...
            {error, noreply}
    end.

get_workflow_id_list() -> get_ets_keys(?WorkFlowTable).

get_workflow_by_workflowId_atom(WorkFlowIDAtom) ->
    [{_, Workflow}] = ets:lookup(?WorkFlowTable, atom_to_binary(WorkFlowIDAtom, utf8)),
    Workflow.

get_workflow(WorkFlowID) ->
    [{_, Workflow}] = ets:lookup(?WorkFlowTable, WorkFlowID),
    Workflow.

count_client(WorkflowId) ->
    count_list(get_wf_client_list(WorkflowId)).

get_wf_client_list(WorkflowId) ->
    [{_, ClientList} | _] = ets:lookup(?ClientListTable, WorkflowId),
    ClientList.

store_client_list_for_each_workflow(WorkflowList) ->
    case WorkflowList of
        [] -> ok;
        [Workflow | Tail] ->
            {_, WFClientList} = load_client_list(Workflow),
            ets:insert_new(?ClientListTable, {maps:get(<<"workflow_id">>, Workflow), WFClientList}),
            store_client_list_for_each_workflow(Tail)
    end.


load_client_list(Workflow) ->
    ClientsInfo = maps:get(<<"clients_info">>, Workflow),
    WorkflowId = maps:get(<<"workflow_id">>, Workflow),
    logger:debug("~p :workflow  ~p  clients info  ~p ", [?MODULE, WorkflowId, maps:get(<<"data_acquisition_mode">>, ClientsInfo)]),
    case maps:get(<<"data_acquisition_mode">>, ClientsInfo) of
        <<"localfile">> ->
            FilePath = maps:get(<<"file_path">>, ClientsInfo),
            logger:debug("~p :workflow  ~p  load client info from file  ~p ", [?MODULE, WorkflowId, FilePath]),
            load_file_lines(FilePath);
        _ -> {error, noreply}
    end.

load_file_lines(FilePath) ->
    case file:open(FilePath, [read]) of
        {ok, FileIo} ->
            FileLines = try
                            load_lines(FileIo) after
                            file:close(FileIo)
                        end,
            {ok, FileLines};
        {error, Reason} ->
            logger:error("[~p] load file [ ~p ] fail :~p", [?MODULE, FilePath, Reason]),
            {error, Reason}
    end.

load_lines(FileIO) -> load_lines(FileIO, []).

load_lines(FileIO, Lines) ->
    case file:read_line(FileIO) of
        eof -> Lines;
        {ok, ReadLine} ->
            case ommit_comments(ommit_comments(ReadLine, "#"), "//")
            of
                [] -> load_lines(FileIO, Lines);
                Line -> load_lines(FileIO, Lines ++ [Line])
            end;
        {error, Reason} ->
            logger:error("[~p] load file [ ~p ] fail :~p", [?MODULE, Reason]),
            Lines
    end.

%% comments are lines begin with "//" or "#" (single line comments) or
%% the texts after "//" or "#" (inline comments)
ommit_comments(Line, CommentToken) ->
    case string:split(Line, CommentToken) of
        [Literals, _Comments] -> string:trim(Literals);
        [Literals] -> string:trim(Literals)
    end.

get_ets_keys(Table) ->
    get_ets_keys(Table, ets:first(Table)).

get_ets_keys(Table, Key) ->
    case Key of
        '$end_of_table' -> [];
        _ -> [Key] ++ get_ets_keys(Table, ets:next(Table, Key))
    end.

count_list(AnyList) ->
    count_list(AnyList, 0).

count_list(AnyList, Counter) ->
    case AnyList of
        [_AnyAtom | Tail] ->
            count_list(Tail, Counter + 1);
        [] -> Counter
    end.