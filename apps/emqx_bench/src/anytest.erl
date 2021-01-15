-module(anytest).

-export([start/0]).

-export([testwork6/1, testwork7/0]).

-define(WorkFlowTable, workflowid_workflow).

-define(ClientListTable, workflowid_clientsinfo).

start() ->
    % io:format("test 1  result: ~p ~n", [testwork1("bbbb")]),
    start_map_speed_test().

start_map_speed_test() ->
    StartTime = os:timestamp(),
    Map = #{<<"taskA">> => 1, <<"taskB">> => 2, <<"taskC">> => 3, <<"taskD">> => 4, <<"taskE">> => 5},
    maps_speed_test(Map, 100),
    EndTime = os:timestamp(),
    timer:sleep(2000),
    io:format("~p   ~p", [StartTime, EndTime]).

maps_speed_test(Map, Index) when Index > 60 ->
    io:format("~p~n", [Index]),
    spawn(fun() -> get_map_group(<<"taskA">>, Map, 100) end),
    maps_speed_test(Map, Index - 1);
maps_speed_test(Map, Index) when Index > 80 ->
    io:format("~p~n", [Index]),
    spawn(fun() -> get_map_group(<<"taskB">>, Map, 100) end),
    maps_speed_test(Map, Index - 1);
maps_speed_test(Map, Index) when Index > 0 ->
    io:format("~p~n", [Index]),
    spawn(fun() -> get_map_group(<<"taskC">>, Map, 100) end),
    maps_speed_test(Map, Index - 1);
maps_speed_test(Map, Index) when Index =:= 0 ->
    ok.
get_map_group(Key, Map, Index) when Index > 0 ->
    io:format("~p ==> ~p~n", [Key, Index]),
    spawn(fun() -> get_from_map(Key, Map) end),
    get_map_group(Key, Map, Index - 1);
get_map_group(Key, Map, Index) when Index =:= 0 ->
    ok.

get_from_map(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            Value;
        _ -> 0
    end.

testwork_byte_size() ->
    Ep = <<"002052905290529">>,
    BS = byte_size(Ep),
    io:format("Ep is ~p~n", [BS]),
    ok.

testwork_strprepare() ->
    Arg0 = #{<<"task">> => <<"binary key 0 vlaue">>},
    Arg1 = #{<<"task">> => <<"binary key vlaue">>},
    Arg2 = #{task => atom_key_vlaue},
    testwork_prepare(Arg0),
    testwork_prepare(Arg1),
    testwork_prepare(Arg2).

testwork_prepare(#{<<"task">> := <<"binary key 0 vlaue">>}) ->
    io:format("Key is str   value is   binary key vlaue ~n");
testwork_prepare(#{<<"task">> := Value}) ->
    io:format("key is str and v is ~p~n", [Value]);
testwork_prepare(#{task := MyArg}) ->
    io:format("Key is term ~p~n", [MyArg]);
testwork_prepare(KV) ->
    io:format("i don't care ~p~n", KV).

testwork1() ->
    properties_manager:init(),
    Result = properties_manager:load_properties({fromfile,
        "workflow_mqtt_1.json"}),
    io:format("test work 1 load properties result ~n "
    "~p~n",
        [Result]),
    WorkFlowList =
        properties_manager:get_workflow_id_list(),
    io:format("test work1 workflow id list is ~p~n",
        [WorkFlowList]),
    Workflow1 =
        properties_manager:get_workflow(<<"connection_only">>),
    io:format("test work 1 get workflow 1 result   "
    "~n ~p~n",
        [Workflow1]),
    Workflow2 =
        properties_manager:get_workflow(<<"connection_and_publish">>),
    io:format("test work 1 get workflow 2 result   "
    "~n ~p~n",
        [Workflow2]),
    ClientList =
        properties_manager:get_wf_client_list(<<"connection_only">>),
    io:format("test work 1 wf1 client list ~n ~p~n",
        [ClientList]),
    ClientList2 =
        properties_manager:get_wf_client_list(<<"connection_and_publish">>),
    io:format("test work 1 wf2 client list ~n ~p~n",
        [ClientList2]),
    ok.

% testwork2() ->
% OptSpecList = [
%     {help,          $h, "help",         boolean,    "help information"},
%     {workflowid,    $w, "workflowid",   string,     "Chose workflow by woriflow_id (define in workflow file ,josn key [ workflow_id ]). if not specified ,run all workflow "}
%     ],
% Argv = "-w connection_only",
% {ok, {Opts, _Args}} = getopt:parse(OptSpecList, Argv),
% Result = proplists:get_value(workflowid, Opts, all),
% io:format("this maybe the workflow ID ~p~n~n", [Result]),
% ok.

% testwork3() ->
%     ets:new(?WorkFlowTable, [set, protected, named_table]),
%     ets:insert(?WorkFlowTable,
%                [{<<"test key">>, 1}, {k2, 2}]),
%     % ets:insert(?WorkFlowTable, {k2, 2}),
%     ets:insert(?WorkFlowTable, {k3, 3}),
%     ets:insert(?WorkFlowTable, {k4, 4}),
%     Key1 = ets:first(?WorkFlowTable),
%     io:format("this is keys list ~p~n",
%               [get_ets_keys(?WorkFlowTable, Key1)]),
%     io:format("get k2 test ~p~n",
%               [ets:lookup(?WorkFlowTable, k2)]),
%     ets:delete(?WorkFlowTable),
%     ok.

% get_ets_keys(Table, Key) ->
%     case Key of
%         '$end_of_table' -> [];
%         _ -> [Key] ++ get_ets_keys(Table, ets:next(Table, Key))
%     end.

% show_next_kv(Table, Key) ->
%     case Key of
%         '$end_of_table' -> io:format("end of table");
%         _ ->
%             io:format("get key 1 ~p value is ~p ~n~n",
%                       [Key, ets:lookup(mytable, Key)]),
%             show_next_kv(Table, ets:next(Table, Key))
%     end.

testwork4() ->
    Mymap = #{a => 123, b => 456},
    io:format("take c ~p", [maps:find(a, Mymap)]),
    io:format("self pid ?~p  ", [self()]),
    ok.

testwork5() -> spawn(fun() -> startserver() end).

startserver() ->
    {ok, Socket} = gen_udp:open(3700, [binary]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp, Socket, Host, Port, Bin} = Msg ->
            io:format("Msg  is ~p ~n", [Msg]),
            io:format("message is ~p~n", [Bin]),
            gen_udp:send(Socket, Host, Port, "hello")
    end,
    loop(Socket).

testwork6(SendMsg) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    Host = {127, 0, 0, 1},
    Port = 3700,
    ok = gen_udp:send(Socket,
        Host,
        Port,
        atom_to_binary(SendMsg)),
    receive
        {udp, Socket, _, _, _Bin} = Msg ->
            io:format("client receive ~p ~n", [Msg])
    after 2000 -> io:format("timeout~n")
    end,
    gen_udp:close(Socket).

testwork7() ->
    Bytes_r = <<68, 2, 0, 15, 67, 91, 82, 20, (-78), 114,
        100, 73, 108, 119, 109, 50, 109, 61, 49, 46, 48, 13, 5,
        101, 112, 61, 48, 48, 50, 48, 53, 50, 57, 48, 53, 50,
        57, 48, 53, 50, 57, 3, 98, 61, 85, 6, 108, 116, 61, 51,
        48, 48, (-1), 60, 47, 62, 59, 114, 116, 61, 34, 111,
        109, 97, 46, 108, 119, 109, 50, 109, 34, 59, 99, 116,
        61, 49, 49, 53, 52, 51, 44, 60, 47, 51, 47, 48, 62, 44,
        60, 47, 53, 47, 48, 62, 44, 60, 47, 49, 57, 47, 48, 62,
        44, 60, 47, 49, 57, 47, 49, 62>>,
    Bytes_r2 = <<68, 2, 0, 0, 85, 180, 111, 62, 178, 114,
        100, 17, 40, 51, 98, 61, 85, 13, 5, 101, 112, 61, 48,
        48, 50, 48, 53, 50, 57, 48, 53, 50, 57, 48, 53, 50, 57,
        5, 108, 116, 61, 51, 48, 9, 108, 119, 109, 50, 109, 61,
        49, 46, 48, 255, 60, 47, 62, 59, 114, 116, 61, 34, 111,
        109, 97, 46, 108, 119, 109, 50, 109, 34, 44, 60, 47, 51,
        47, 48, 62, 44, 60, 47, 49, 57, 47, 48, 62>>,
    {ok, Socket} = gen_udp:open(0, [binary]),
    Host = "221.229.214.202",
    Port = 5683,
    ok = gen_udp:send(Socket, Host, Port, Bytes_r2),
    receive
        {udp, Socket, _, _, _Bin} = Msg ->
            io:format("client receive ~p ~n", [Msg])
    after 2000 -> io:format("timeout~n")
    end,
    gen_udp:close(Socket).
