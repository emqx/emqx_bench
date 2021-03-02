%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 2月 2021 5:43 下午
%%%-------------------------------------------------------------------
-module(aep_tcp_simulator).
-author("DDDHuang").

-behaviour(gen_statem).
-include("aep_tcp.hrl").
%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3,
    code_change/4, callback_mode/0]).
-export([working/4]).

-define(SERVER, ?MODULE).

-record(aep_tcp_state, {
    host,
    port,
    client_id,
    identity,
    socket ,
    sampler,
    sampler_arg
}).

start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

init(Args) ->
%%    Socket = gen_tcp:connect(Address, Port,
%%        [binary, {packet, raw}, {reuseaddr, true}, {nodelay, true}, {active, false}]),
    {ok, state_name, do_init(Args, #aep_tcp_state{})}.

do_init([{host, Host}, Args], State) ->
    do_init(Args, State#aep_tcp_state{host = Host});
do_init([{port, Port}, Args], State) ->
    do_init(Args, State#aep_tcp_state{port = Port});
do_init([{client_id, ID}, Args], State) ->
    do_init(Args, State#aep_tcp_state{client_id = ID});
do_init([{identity, Data}, Args], State) ->
    do_init(Args, State#aep_tcp_state{identity = Data});
do_init([{port, Port}, Args], State) ->
    do_init(Args, State#aep_tcp_state{port = Port});
do_init([{_, _}, Args], State) -> do_init(Args, State);
do_init([], State) -> State.

callback_mode() ->
    state_functions.

format_status(_Opt, [_PDict, _StateName, _State]) ->
    Status = some_term,
    Status.

working(_EventType, _EventContent, State = #aep_tcp_state{}) ->
    {next_state, working, State}.

wait_response(_EventType, _EventContent, State = #aep_tcp_state{}) ->
    {next_state, working, State}.

working(_EventType, _EventContent, _StateName, _State = #aep_tcp_state{}) ->
    keep_state_and_data.


handle_event(_EventType, _EventContent, _StateName, State = #aep_tcp_state{}) ->
    NextStateName = the_next_state_name,
    {next_state, NextStateName, State}.

terminate(_Reason, _StateName, _State = #aep_tcp_state{}) ->
    ok.

code_change(_OldVsn, StateName, State = #aep_tcp_state{}, _Extra) ->
    {ok, StateName, State}.

