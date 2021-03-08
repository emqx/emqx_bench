%%%-------------------------------------------------------------------
%% @doc lwm2m_bench top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lwm2m_bench_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

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
