%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 3月 2021 6:18 下午
%%%-------------------------------------------------------------------
-module(work_execute).
-author("DDDHuang").

-export([]).

-include_lib("emqx_bench/include/emqx_bench.hrl").

execute_task_list(SimulatorPid, Module, TaskList) ->
    ok.

execute(SimulatorPid, Module, [#task{action = Fun, args = Args}]) ->
    case erlang:apply(Module, Fun, [SimulatorPid, Args]) of
        ok -> ok;
        _ -> error
    end.
