%%%-------------------------------------------------------------------
%% @doc lwm2m_bench top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lwm2m_bench_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-export([start_workflow/2]).

-export([simple_test/0]).
-export([bs_sm2_test/0]).

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
    [#{id => lwm2m_simulator,
        start => {lwm2m_simulator, start_link, []},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker}].

start_workflow(Workflow, ClientInfoList) ->
    start_all_simulator(Workflow, ClientInfoList).

start_all_simulator(#work_flow{simulator_config = SimulatorConfig, task_list = TaskList} = Workflow,
    [ClientInfo | ClientInfoList]) ->
%%    CallBackArg = ignore,
    StartArgs = [
        {task_list, TaskList},
        {socket, new},
        {imei, ClientInfo}
    ],
    supervisor:start_child(?SERVER, [lists:append(SimulatorConfig, StartArgs)]),
    start_all_simulator(Workflow, ClientInfoList).


simple_test() ->
    application:start(lwm2m_bench),
    Port = 5683,
    Host = "221.229.214.202",
    IMEI = <<"202103201518000">>,
%%    Host = "180.101.145.55",
%%    IMEI = <<"202103161001999">>,

    Args =
        [
            {imei, IMEI},
            {host, Host},
            {port, Port},
            {socket, new},
            {data_type, json},
            {lifetime, 10}
        ],
    {ok, Pid} = supervisor:start_child(?SERVER, [Args]),
    erlang:register(lw, Pid),
    Result = lwm2m_simulator:register(lw),
    io:format("~0p~n", [Result]),
    sleep(10),
    io:format("logout ~0p~n", [lwm2m_simulator:deregister(lw)]).

sleep(Time) when Time > 0 ->
    timer:sleep(1 * 1000),
    io:format("beep ~0p~n", [Time]),
    sleep(Time - 1);
sleep(_Time) ->
    ok.


bs_sm2_test() ->
    lwm2m_bench_app:start(a, a),
%%    Host = "221.229.214.202",
%%    Host = "180.107.140.103",
%%    Host = "180.101.145.52",
    Host = "180.106.148.146",
    Port = 5683,
    IMEI = <<"622200000012345">>,
%%    IMEI = <<"622210000012345">>,
    PubKeyHex = "11017D016532DFFE2C69188E391E6362EC1499CD96A064E28F46D287BBC415D60DDF638D3E7E0309744849C0B87EB08E520E7F9951EDC4ED51FF8B6AD09D990CACF3589A690AE37E1FDE93F7B3866EE73822F729E3C7666C67989CF0B5C6FA1EB6156F2F000001198305B4A5C74FE5A4640EC30A7451AEF61EA704D3CF62666993D2745CE0BA33EF7EC1765BA5F63263BDAACD6CFAEF526DACF92BFF48900EFBB5F8724619D6FC656B42FCACFFBA8C2E7F6E7C373FA244E65E66A1AEBABD93C9ACDDB0D49C151D1B97CC3D52A52D8636A366204EB061D828DA2F0AF5552AF930964CF1707BAF67B595B2205F251B750BEFBD77ABDAD16A32182F6C890969DB6D5DD7F6FB97E6CE922B06A99CB0CD43245792681F9A100FFC1B38EC4894500819C760BC8F2E1FF42B4CCD9787CAF3F07E8F6309FDC4FAFC198BAEFEAAA498D573407DC3B1E49ACEB57968AD7A0E638CB87B78A8889A008519C6026DCF70F7F26D0557C5F18CFE550DF9AADEAD6332E9979521FBB2AD9375F4B94F9FCCB171EB5445",
    BigInteger = list_to_integer(PubKeyHex, 16),
    PubKey = binary:encode_unsigned(BigInteger),
    Args =
        [
            {socket, new},
            {imei, IMEI},
            {host, Host},
            {port, Port},
            {sm2_public_key, PubKey}
            ],
    {ok, Pid} = supervisor:start_child(?SERVER, [Args]),
    erlang:register(lw, Pid),
    Result = lwm2m_simulator:bootstrap_sm2(lw),
    io:format("~0p~n", [Result]),
    sleep(2),
%%    lwm2m_simulator:bootstrap_finished(lw),
    sleep(10),
%%    io:format("logout ~0p~n", [lwm2m_simulator:deregister(lw)]).
    ok.


