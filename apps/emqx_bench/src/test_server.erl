-module(test_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(test_state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #test_state{}}.


handle_call({new_test,WorkFlow}, _From, State = #test_state{}) ->
    test_root_sup:new_test(WorkFlow),
    {reply, ok, State};

handle_call(_Request, _From, State = #test_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #test_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #test_state{}) ->
    {noreply, State}.


terminate(_Reason, _State = #test_state{}) ->
    ok.


code_change(_OldVsn, State = #test_state{}, _Extra) ->
    {ok, State}.

