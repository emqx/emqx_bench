%%%-------------------------------------------------------------------
%% @doc lwm2m_bench top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lwm2m_bench_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-export([start_workflow/2]).

-include("coap.hrl").
-include_lib("emqx_bench/include/emqx_bench.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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

start_workflow(Workflow, ClientInfoList) ->
%%        IMEI = <<"202002261804000">>,
    start_all_simulator(Workflow,ClientInfoList).

start_all_simulator( #work_flow{simulator_config = SimulatorConfig, task_list = TaskList} = Workflow,
    [ClientInfo | ClientInfoList])->
    CallBackFun = call_back(),
    CallBackArg = ignore,
    StartArgs = [
        {task_callback, {CallBackFun, CallBackArg}},
        {task_list, TaskList},
        {socket, new},
        {imei, ClientInfo}
        ],
    supervisor:start_child(?SERVER, [lists:append(SimulatorConfig, StartArgs)]),
    start_all_simulator(Workflow, ClientInfoList).


call_back()->
%%     todo counter
    fun
        (#task{action = Action,index = _Index, args = _Args}, Result, _CallBackArgs) ->
            io:format("~0p ~0p~n", [Action, Result]);
        (task_list_over, _, _) ->
            io:format("~0p~n", [task_list_over]);
        (Task, Result, Args) ->
            io:format("~0p ~0p ~0p~n", [Task, Result, Args])
    end.