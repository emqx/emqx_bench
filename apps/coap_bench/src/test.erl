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
    % test:start().
    coap_bench_app:start(a, a),
    Host = "127.0.0.1",
    % Host = "192.168.0.9",
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
        token = <<"tk">>,
        options = [
            option(?URI_PATH, <<"mqtt">>),
            option(?URI_PATH, <<"coap_to_mqtt">>),
            option(?URI_QUERY, <<"c=coap2021">>),
            option(?URI_QUERY, <<"u=username">>),
            option(?URI_QUERY, <<"p=pwd0">>)
        ],
        payload = <<"hello EMQX world, i am coap">>
    },
    coap_simulator:request(Pid, Message),
    SubMessage = #coap_message{
        type = ?CON,
        method = ?GET,
        id = 1,
        token = <<>>,
        options = [
            option(?URI_OBSERVE, <<0>>),
            option(?URI_PATH, <<"mqtt">>),
            option(?URI_PATH, <<"mqtt_to_coap">>),
            option(?URI_QUERY, <<"c=coap2021">>),
            option(?URI_QUERY, <<"u=username">>),
            option(?URI_QUERY, <<"p=pwd0">>)
        ],
        payload = <<>>
    },
    coap_simulator:request(Pid, SubMessage).


option(Option, Value) -> Option#option{value = Value}.
