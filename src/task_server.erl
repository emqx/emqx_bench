%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, EMQX
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(task_server).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
 {ok, #state{}}.

handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

handle_info(_Info, State = #state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% TODO: 
%% task index -> map {id => task}
%% when simulator execute id 1 -> next task  
%% task count -> index : success | fail | {fail, Reason}
%% task metrics


