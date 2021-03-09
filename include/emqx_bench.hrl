%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 12月 2020 3:41 下午
%%%-------------------------------------------------------------------
-author("DDDHuang").

-record(work_flow,
{
    test_id             :: atom(),
    protocol            :: atom(),
    task_list           :: list(),
    simulator_config    :: term()
}
).

-record(task,
{
    index               :: integer(),
    action              :: function(),
    args                :: any()
}).
