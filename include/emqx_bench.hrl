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
    simulator_info      ,  %% #simulator_info{}
    task_list           :: list()
}
).

-record(simulator_info,
{
    csv_file_path       :: string(),
    start_index         :: integer(),
    end_index           :: integer()
}).

-record(task,
{
    index               :: integer(),
    task_ref            :: atom(),
    task_info           :: any()
}).

-record(task_executed_status,
{
    status              :: success | fail,
    reason              :: any(),
    count_ref              %% count ref
}).
