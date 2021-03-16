%%%-------------------------------------------------------------------
%% @doc coap_bench_app public API
%% @end
%%%-------------------------------------------------------------------

-module(coap_bench_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    coap_bench_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
