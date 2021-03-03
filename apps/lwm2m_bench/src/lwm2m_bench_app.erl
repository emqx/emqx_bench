%%%-------------------------------------------------------------------
%% @doc lwm2m_bench public API
%% @end
%%%-------------------------------------------------------------------

-module(lwm2m_bench_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lwm2m_bench_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
