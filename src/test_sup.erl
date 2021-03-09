%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 12月 2020 10:52 上午
%%%-------------------------------------------------------------------
-module(test_sup).
-author("DDDHuang").

-behaviour(supervisor).
-include("emqx_bench.hrl").
%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Config]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([Config = #work_flow{task_list = TaskList, protocol = Protocol,
    simulator_config = SimulatorConfig}]) ->
    CounterRef = counters:new(length(TaskList), [write_concurrency]),
    persistent_term:put(?MODULE, CounterRef),
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = #{strategy => simple_one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts},
    start_bench_app(Protocol, permanent, SimulatorConfig),
    AChild = simulator_child_spec(Config),
    {ok, {SupFlags, [AChild]}}.

start_bench_app(Protocol, StartType, _StartArgs) ->
    BenchApp = find_bench(Protocol),
    ok = application:ensure_started(BenchApp, StartType).

%%%===================================================================
%%% Internal functions
%%%===================================================================
simulator_child_spec(Config = #work_flow{protocol = Protocol}) ->
    #{
        start => {Protocol, start_link, [Config]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker
    }.

find_bench(lwm2m) -> lwm2m_bench_app;
find_bench(mqtt) -> mqtt_bench_app;
find_bench(tcp) -> tcp_bench_app.

