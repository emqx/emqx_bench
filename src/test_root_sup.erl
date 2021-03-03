-module(test_root_sup).

-behaviour(supervisor).
-include("emqx_bench.hrl").
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([new_test/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

new_test(WorkFlow) ->
    supervisor:start_child(?MODULE, test_sup_child_spec(WorkFlow)).

test_sup_child_spec(Config = #work_flow{test_id = ID}) ->
    #{
        id => ID,
        start => {test_sup, start_link, [Config]},
        restart => permanent,
        shutdown => brutal_kill,
        type => supervisor
    }.



