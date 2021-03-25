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
%%    Host = "221.229.214.202",
%%    IMEI = <<"202103201518000">>,
    Host = "180.101.145.55",
    IMEI = <<"202103161001999">>,

    Args =
        [
            {imei, IMEI},
            {host, Host},
            {port, Port},
            {socket, new},
            {data_type, json}
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
    Host = "180.101.145.52",
    Port = 8683,
    IMEI = <<"622600100112345">>,
    PubKeyHex = "11017D0156325BF2B9D905FDF099A8EA3C3804B521030B70C546C7E5A1C4B8AA0244529A3DCAEBCF73E7B70E9E71E45E49C12EC7E213D3D3E37888D99FECDC059DABE19F144044AEC35D7A4AC9FA3A1307F49F9B71DE6C99E63C949EE682FA33E8866D1600000119CE9B304DB773B3DBA5D817F61B31A4B7A00D9492DDAE9E44810FC631DD6B63C2319968B7B189BE91D467866BF66F8D2A7529AAE8010FD925A25CFE57538FDEEEE688668C2EC84235363F59FC2ACC32DE402AB99353C9FC2F2811E409925CAEE592B6DEA832B1BD93AD512EE95175B68788F023AE15DE10C0CB570F63771EC65714F9162863E8E9533EFAEFCF38959838C84AFAC962B1665479C5C3D4ADE65CD7B5DB9AEE637D4CBDC65E240FF93D9714CFD771DCA0DBA0C3081B1AB228C91E35744AB697954A6EB2A38D39D5B0584DAF77907605757D1082107187733F95D49D57BA529CDB49AA6D002582BE72FC115EAFD8B503391C95D40C574C3CAFEBDAAED2A5C47AE05033188F897D60CD6B6244FBFE4D353B3395724C",
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
    sleep(10),
%%    io:format("logout ~0p~n", [lwm2m_simulator:deregister(lw)]).
    ok.


