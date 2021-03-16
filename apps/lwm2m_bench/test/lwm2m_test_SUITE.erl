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

bs_sm2_test() ->
    lwm2m_bench_app:start(a, a),
%%    Host = "221.229.214.202",
    Host = "180.107.140.103",
    Port = 8683,
    IMEI = <<"622600100112345">>,
    CallBackFun = fun(Task, Result, _) -> io:format("~0p --------------> ~0p~n", [Task, Result]) end,
    CallBackArg = ignore,
    PubKeyHex = "11017D0156325BF2B9D905FDF099A8EA3C3804B521030B70C546C7E5A1C4B8AA0244529A3DCAEBCF73E7B70E9E71E45E49C12EC7E213D3D3E37888D99FECDC059DABE19F144044AEC35D7A4AC9FA3A1307F49F9B71DE6C99E63C949EE682FA33E8866D1600000119CE9B304DB773B3DBA5D817F61B31A4B7A00D9492DDAE9E44810FC631DD6B63C2319968B7B189BE91D467866BF66F8D2A7529AAE8010FD925A25CFE57538FDEEEE688668C2EC84235363F59FC2ACC32DE402AB99353C9FC2F2811E409925CAEE592B6DEA832B1BD93AD512EE95175B68788F023AE15DE10C0CB570F63771EC65714F9162863E8E9533EFAEFCF38959838C84AFAC962B1665479C5C3D4ADE65CD7B5DB9AEE637D4CBDC65E240FF93D9714CFD771DCA0DBA0C3081B1AB228C91E35744AB697954A6EB2A38D39D5B0584DAF77907605757D1082107187733F95D49D57BA529CDB49AA6D002582BE72FC115EAFD8B503391C95D40C574C3CAFEBDAAED2A5C47AE05033188F897D60CD6B6244FBFE4D353B3395724C",
    BigInteger = list_to_integer(PubKeyHex, 16),
    PubKey = binary:encode_unsigned(BigInteger),
    BootstrapTask = #task{action = bootstrap_sm2, args = PubKey},
    TaskList = [BootstrapTask],
    Args =
        [
            {socket, new},
            {imei, IMEI},
            {host, Host},
            {port, Port},
            {task_callback, {CallBackFun, CallBackArg}},
            {task_list, TaskList}
        ],
    supervisor:start_child(?SERVER, [Args]).

simple_test() ->
    lwm2m_bench_app:start(a, a),
%%    Host = "221.229.214.202",
%%    Host = "180.101.145.55",
    Host = "180.107.140.103",
    Port = 5683,
    IMEI = <<"622600100112345">>,
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
        [
            {socket, new},
            {imei, IMEI},
            {host, Host},
            {port, Port},
            {task_callback, {CallBackFun, CallBackArg}},
            {task_list, TaskList}
        ],
    supervisor:start_child(?SERVER, [Args]).