%%%-------------------------------------------------------------------
%% @doc lwm2m_bench top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lwm2m_bench_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).
-export([start_lw_test/0]).

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

start_lw_test() ->
    lwm2m_bench_app:start(a,a),
    Host = "221.229.214.202",
%%    Host = "221.229.214.201",
    Port = 5683,
    IMEI = <<"202002261804000">>,
    Args = [{imei, IMEI}, {host, Host}, {port, Port}],
    {ok, LWPid} = supervisor:start_child(?SERVER, [Args]),
    lwm2m_simulator:register(LWPid).
