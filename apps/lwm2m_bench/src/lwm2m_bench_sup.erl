%%%-------------------------------------------------------------------
%% @doc lwm2m_bench top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lwm2m_bench_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).
-export([start_lw_test/0, t/0]).

-include("coap.hrl").

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
    %%                      fun( Task :: #task{},Result :: success | {fail, Reason}, CallBackArgs :: any()),
    CallBackFun = fun(Task,Result,_) -> io:format("task ~0p execute ~0p~n",[Task,Result]) end,
    CallBackArg = ignore,
    Args =
        [   {imei, IMEI},
            {host, Host},
            {port, Port},
            {task_callback, {CallBackFun, CallBackArg}}
        ],
    {ok, LWPid} = supervisor:start_child(?SERVER, [Args]),
    lwm2m_simulator:register(LWPid).

t()->
    A = ?CREATED,
    B = ?DELETED,
    C = ?CREATED,
    AB = A =:= B,
    AC = A =:= C,
    io:format("A ~0p  ~n",[A]),
    io:format("B ~0p  ~n",[B]),
    io:format("C ~0p  ~n",[C]),
    io:format("A =:= B ~0p  ~n",[AB]),
    io:format("A =:= C ~0p  ~n",[AC]).

