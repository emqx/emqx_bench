%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 3月 2021 3:33 下午
%%%-------------------------------------------------------------------
-module(simulator_sup).
-author("DDDHuang").

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-export([start/1]).

-export([execute/4]).

-export([test/0, echo/1]).

-define(SERVER, ?MODULE).

-include_lib("emqx_bench/include/emqx_bench.hrl").

start_link(#work_flow{test_id = ID} = WorkFlow) ->
    supervisor:start_link({local, ID}, ?MODULE, [WorkFlow]).

init(#work_flow{protocol = Protocol, task_list = TaskList}) ->
    SupFlags = #{strategy => simple_one_for_one,
        intensity => 0,
        period => 1},
    {ok, {SupFlags, child_spec(protocol_bench(Protocol), sort_task_list(TaskList))}}.

sort_task_list(TaskList)->
    SortFun = fun(#task{index = IndexA}, #task{index = IndexB}) -> IndexA < IndexB end,
    lists:sort(SortFun,TaskList).

start(#work_flow{simulator_info = InfoList} = WorkFlow) ->
    {ok, SupervisorPID} = supervisor:start_link(?MODULE, WorkFlow),
    start_all_simulator(SupervisorPID, InfoList).
start_all_simulator(ID, [Info | InfoList]) ->
    io:format("id  ~0p  info ~0p ~n",[ID,Info]),
    supervisor:start_child(ID, [Info]),
    start_all_simulator(ID, InfoList);
start_all_simulator(ID, []) ->
    {ok, ID}.


test() ->
    Host = "221.229.214.202",
    Port = 5683,
    DeviceInfo1 = [{imei, <<"202002261804000">>}, {host, Host}, {port, Port}],
    Info = [DeviceInfo1],
    TaskReg = #task{index = 0, action = register},
    TaskList = [TaskReg],
    WorkFlow = #work_flow{test_id = test1, protocol = lwm2m, simulator_info = Info, task_list = TaskList},
    lwm2m_bench_app:start(a,b),
    start(WorkFlow).

echo(M) -> io:format("~0p~n",[M]).

execute(SimulatorPid, Module, Function, Args)->
    erlang:apply(Module, Function, [SimulatorPid, Args]).
%%%===================================================================
%%% Internal functions
%%%===================================================================
child_spec(Bench, TaskList) ->
    [#{ id          => Bench,
        start       => {Bench, start_link, [TaskList]},
        restart     => temporary,
        shutdown    => brutal_kill,
        type        => worker}].

protocol_bench(Protocol) when is_binary(Protocol) ->
    protocol_bench(binary_to_atom(Protocol, utf8));
protocol_bench(mqtt) -> mqtt_bench;
protocol_bench(lwm2m) -> lwm2m_simulator;
protocol_bench(tcp) -> tcp_bench;
protocol_bench(_) -> mqtt_bench.


