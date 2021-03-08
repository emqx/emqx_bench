%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 3月 2021 2:15 下午
%%%-------------------------------------------------------------------
-module(lwm2m_test_SUITE).
-author("DDDHuang").

%% API
-compile(nowarn_export_all).

-define(SERVER, lwm2m_bench_sup).

-include_lib("emqx_bench/include/emqx_bench.hrl").

all() -> [simple_test].

simple_test() ->
    lwm2m_bench_app:start(a, a),
    Host = "221.229.214.202",
%%    Host = "221.229.214.201",
    Port = 5683,
    IMEI = <<"202002261804000">>,
    %%                      fun( Task :: #task{},Result :: success | {fail, Reason}, CallBackArgs :: any()),
    CallBackFun = fun(Task, Result, _) -> io:format("~0p --------------> ~0p~n", [Task, Result]) end,
    CallBackArg = ignore,
    RegisterTask = #task{action = register},
    AutoObserveTask2 = #task{index = 2, action = auto_observe, args = 5000},
    AutoObserveTask3 = #task{index = 3, action = auto_observe, args = 5000},
    AutoObserveTask4 = #task{index = 4, action = auto_observe, args = 5000},
    PublishTask = #task{action = publish, args = <<"">>},
    DeRegisterTask = #task{action = de_register},
    CloseTask = #task{action = close},
    TaskList = [
        RegisterTask,
        AutoObserveTask2,
        AutoObserveTask3,
        AutoObserveTask4,
        PublishTask,
        DeRegisterTask,
        CloseTask
    ],
    Args =
        [{imei, IMEI},
            {host, Host},
            {port, Port},
            {task_callback, {CallBackFun, CallBackArg}},
            {task_list, TaskList}
        ],
    supervisor:start_child(?SERVER, [Args]),
    ?assertEqual(a,a).