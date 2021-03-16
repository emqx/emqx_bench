%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 3月 2021 2:14 下午
%%%-------------------------------------------------------------------
-module(test).
-author("DDDHuang").

%% API
-export([start/0]).
-include("coap.hrl").

-define(SERVER, coap_bench_sup).


start() ->
    coap_bench_app:start(a, a),
    Host = "127.0.0.1",
    Port = 5683,
    Args =
        [
            {socket, new},
            {host, Host},
            {port, Port}
        ],
    {ok, Pid} = supervisor:start_child(?SERVER, [Args]),
    Message = #coap_message{
        type = ?CON,
        method = ?PUT,
        id = 1,
        token = <<"aa">>,
        options = [
            option(?URI_PATH, <<"mqtt">>),
            option(?URI_PATH, <<"topic1">>),
            option(?URI_QUERY, <<"c=coap2021">>),
            option(?URI_QUERY, <<"u=ddd">>),
            option(?URI_QUERY, <<"p=test">>)
        ],
        payload = <<"hello world">>
    },
    coap_simulator:request(Pid, Message).

option(Option, Value) -> Option#option{value = Value}.
