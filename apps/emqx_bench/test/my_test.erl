%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 12月 2020 4:18 下午
%%%-------------------------------------------------------------------
-module(my_test).
-author("DDDHuang").
-export([new_ets/0, insert/2, start/2]).

%% API
-export([]).

%% my_test:start(T,250000).
%% start time {1608,885457,369267}
%% end time {1608,885457,546754}
new_ets() ->
    ets:new(?MODULE, [public]).
start(Table, K) ->
    io:format("start time ~p~n", [os:timestamp()]),
    insert(Table, K).
insert(Table, K) when K > 0 ->
    ets:insert_new(Table, {K, {?MODULE, ?LINE, "key "}}),
    insert(Table, K - 1);
insert(_Table, K) when K == 0 ->
    io:format("end time ~p~n", [os:timestamp()]),
    ok.
