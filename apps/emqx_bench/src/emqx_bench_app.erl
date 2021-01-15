%%%-------------------------------------------------------------------
%% @doc emqx_bench public API
%% @end
%%%-------------------------------------------------------------------

-module(emqx_bench_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    properties_manager:init(),
    emqx_bench_sup:start_link().

stop(_State) -> ok.

%% internal functions

