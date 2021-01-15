%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 9月 2020 3:04 下午
%%%-------------------------------------------------------------------
-module(work_station).
-author("DDDHuang").

%% API
-export([start/0]).


start() ->
    spawn(fun() -> test() end).
test() ->
    UnExecute = 1,
    Success = 7,
    TimeOut = 0,
    Fail = 8,
    TaskName = <<"register">>,
    io:format("~-15.. s~-12.. s ~-12.. s ~-12.. s ~-12.. s~n", ["task", "un executed", "success", "timeout", "fail"]),
    io:format("~-15.. s", [TaskName]),
    io:format("~-12.. w ~-12.. w ~-12.. w ~-12.. w~n", [UnExecute, Success, TimeOut, Fail]),

    ClientSum = 8,
    UnExecutePercent = UnExecute * 100 / ClientSum,
    SuccessPercent = Success * 100 / ClientSum,
    TimeOutPercent = TimeOut * 100 / ClientSum,
    FailPercent = Fail * 100 / ClientSum,

    io:format("~8.. s", [""]),
    print_task_count_percent(UnExecute, ClientSum),
    print_task_count_percent(Success, ClientSum),
    print_task_count_percent(TimeOut, ClientSum),
    print_task_count_percent(Fail, ClientSum),
    io:format("~n"),

    io:format("~s~s~s~n", ["\e[0;31m", debug, "\e[0;38m"]),
    ok.


print_task_count_percent(TaskCount, ClientSum) when (TaskCount * 100 / ClientSum) =:= 0.0 ->
    io:format("0%~11.. s", [""]);
print_task_count_percent(TaskCount, ClientSum) when (TaskCount * 100 / ClientSum) < 10.0 ->
    io:format("~11.2. f% ", [TaskCount * 100 / ClientSum]);
print_task_count_percent(TaskCount, ClientSum) when (TaskCount * 100 / ClientSum) >= 10.0 ->
    io:format("~12.2. f%", [TaskCount * 100 / ClientSum]).
