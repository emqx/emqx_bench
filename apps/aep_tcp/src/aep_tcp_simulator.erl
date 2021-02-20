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

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
    code_change/4, callback_mode/0]).
-export([working/4]).

-define(SERVER, ?MODULE).

-record(aep_tcp_state, {}).

start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, state_name, #aep_tcp_state{}}.

callback_mode() ->
    state_functions.

format_status(_Opt, [_PDict, _StateName, _State]) ->
    Status = some_term,
    Status.

state_name(_EventType, _EventContent, State = #aep_tcp_state{}) ->
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

