%%%-------------------------------------------------------------------
%% @doc lwm2m_bench top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lwm2m_bench_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).
-export([simple_test/0]).

-include("coap.hrl").
-include_lib("emqx_bench/include/emqx_bench.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    {ok, {SupFlags, child_spec()}}.

%% internal functions
child_spec() ->
    [#{ id          => lwm2m_simulator,
        start       => {lwm2m_simulator, start_link, []},
        restart     => temporary,
        shutdown    => brutal_kill,
        type        => worker}].

simple_test() ->
    lwm2m_bench_app:start(a, a),
    Host = "221.229.214.202",
%%    Host = "221.229.214.201",
    Port = 5683,
    IMEI = <<"202002261804000">>,
    %%                      fun( Task :: #task{},Result :: success | {fail, Reason}, CallBackArgs :: any()),
    CallBackFun = fun(Task, Result, _) -> io:format("~0p --------------> ~0p~n", [Result, Task]) end,
    CallBackArg = ignore,
    RegisterTask = #task{action = register},
    AutoObserveTask2 = #task{index = 30, action = auto_observe, args = 5000},
    AutoObserveTask3 = #task{index = 1900, action = auto_observe, args = 5000},
    AutoObserveTask4 = #task{index = 408, action = auto_observe, args = 5000},
    PublishTask = #task{action = publish, args = <<"{\"str_data\":\"112233\"}">>},
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
