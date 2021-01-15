-module(mqtt_sim_worker).

%%create time :20.07.27 10:50

-behaviour(gen_statem).

-export([callback_mode/0,
    code_change/4,
    init/1,
    terminate/3]).

-export([start_link/2]).

-export([working/2]).

start_link(ClientInfo, Workflow) ->
    gen_statem:start_link(?MODULE, [ClientInfo, Workflow], []).

%% Mandatory callback functions

terminate(_Reason, _State, _Data) -> void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

init([ClientInfo, WorkFlow]) ->
    logger:debug("~p  init ~p~n", [?MODULE, WorkFlow]),
    {ok, working, {start_mqttclient(ClientInfo), WorkFlow}}.

callback_mode() -> state_functions.

%%% state callback(s)
working(Client, WorkFlow) ->
    logger:debug("~p get workflow and start working:~p ~p",
        [?MODULE, Client, WorkFlow]),
    {}.

start_mqttclient(ClientInfo) ->
    io:format("Client ~p start", [ClientInfo]),
    ok.