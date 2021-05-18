%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, EMQX
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(coap_simulator).

-behaviour(gen_statem).

-include("coap.hrl").
-include_lib("emqx_bench/include/emqx_bench.hrl").

-export([start_link/1]).

-export([send/2,
        request/2,
        wait/1]).

-export([init/1, 
        format_status/2,
        working/3,
        waiting/3,
        terminate/3,
        code_change/4,
        callback_mode/0]).


  %% module must have callback function:
  %%  init(Args :: any()) -> Loop :: any()
  %%  message_build(Args, Loop) -> 
  %%    {ok, Message, NewLoop} | Ignore :: any()
  %%  handle_message(CoAPMessage, Loop) -> 
  %%    {ok, NewLoop} | Ignore :: any()
  %%  task_result(Task :: #task{}, Result :: #coap_message{} | timeout, Loop) -> 
  %%    {ok, NewLoop} | Ignore :: any()
  %%
  %% Actions :: [Action]
  %% Action :: {new_task, Tasks :: #task{} | [#task{}] } 
  %%          | close

-record(coap_state, {
  socket                      :: gen_udp:socket() | undefined,
  host = {127, 0, 0, 1}       :: binary() | inet:ip_address(),
  port = 5683                 :: integer(),
  message_id = 1              :: integer(),
  tasks = []                  :: list(),
  %%  ResponseMessageID :: integer => Task :: #task{}}
  task_map = #{}              :: map(), 
  callback                    :: term(),
  cb_loop                     :: term()
}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Opts) ->
  gen_statem:start_link(?MODULE, Opts, []).

send(Pid, CoAPMessage) when is_record(CoAPMessage, coap_message) ->
  Task = #task{action = send, args = CoAPMessage},
  new_task(Pid, Task);

send(Pid, CallbackArgs) ->
  Task = #task{action = send, args = CallbackArgs},
  new_task(Pid, Task).

request(Pid, CoAPMessage) when is_record(CoAPMessage, coap_message) ->
  Task = #task{action = request, args = CoAPMessage},
  new_task(Pid, Task);

request(Pid, CallbackArgs) ->
  Task = #task{action = request, args = {build_message, CallbackArgs}},
  new_task(Pid, Task).

wait(Pid) ->
  Task = #task{action = wait, args = ?REQUEST_TIMEOUT},
  new_task(Pid, Task).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
init(Opts) ->
  case proplists:get_value(socket, Opts, undefined) of
    undefined ->
      {ok, working, do_init(Opts ++ [{socket, new}])};    
    _ ->
      {ok, working, do_init(Opts)}    
  end.

callback_mode() ->
  state_functions.

format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = working,
  Status.

working(internal, execute_task, State) ->
  execute_task(State);

working(EventType, EventContent, State) ->
  handle(EventType, EventContent, State).

waiting(state_timeout, {#task{args = CoAPMessage} = Task, AckTimeout, RetryTime}, State) when RetryTime < ?MAX_RETRANSMIT ->
  do_send(CoAPMessage, State),
  NextTimeout = AckTimeout * (?MAX_RETRANSMIT - RetryTime),
  {keep_state, State, [{state_timeout, NextTimeout, {NextTimeout, Task, RetryTime + 1}}]};

waiting(state_timeout, {#task{from = undefined} = Task, _, _}, State) ->
  case callback_apply(task_result, [Task, {fail, timeout}], State) of
    {ok, NewLoop} -> 
      {next_state, working, State#coap_state{cb_loop = NewLoop}, [{next_event, internal, execute_task}]};
    _ ->
      {next_state, working, State, [{next_event, internal, execute_task}]}
  end;

waiting(state_timeout, {#task{from = From}, _, _}, State) ->
  gen_statem:reply(From, {fail, timeout}),
  {next_state, working, State, [{next_event, internal, execute_task}]};

waiting(EventType, EventContent, State) ->
  handle(EventType, EventContent, State).

terminate(_Reason, _StateName, _State = #coap_state{}) ->
  ok.

code_change(_OldVsn, StateName, State = #coap_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_init(Opts) -> 
  lists:foldl(fun(Arg, State) -> do_init(Arg, State) end, #coap_state{}, Opts).

do_init({host, Host}, State)            -> State#coap_state{host = Host};
do_init({port, Port}, State)            -> State#coap_state{port = Port};
do_init({socket, new}, State)           -> State#coap_state{socket = new_socket()};
do_init({socket, keep}, State)          -> State;
do_init({callback, {Module, InitArgs}}, State) ->
    try Module:init(InitArgs) of
        CallbackLoop -> 
          State#coap_state{callback = Module, cb_loop = CallbackLoop}
    catch _:_ -> 
        State
    end;
do_init(_, State) -> State.

new_socket() -> 
  {ok, Socket} = gen_udp:open(0, [binary]), 
  Socket.

new_task(Pid, Task) ->
  try gen_statem:call(Pid, {new_task, Task}, ?REQUEST_TIMEOUT)
  catch _:_ -> 
    {fail, timeout}
  end.

%%%-------------------------------------------------------------------
%%% handle
handle({call, From}, {new_task, Task}, State) ->
  {keep_state, add_task(From, Task, State), [{next_event, internal, execute_task}]};

handle(info, {udp, Sock, PeerIP, PeerPortNo, Packet}, State) ->
  udp_message(Sock, PeerIP, PeerPortNo, Packet, State);

handle(_, _, _State) ->
  keep_state_and_data.

udp_message(_Sock, _PeerIP, _PeerPortNo, Packet, State) ->
  udp_message(Packet, State).
udp_message(Packet, State) ->
  try 
      coap_message_util:parse(Packet)
  of
      {ok, CoAPMessage} ->
          log("receive <<<~n ~0p~n", [CoAPMessage]),
          receive_message(CoAPMessage, State);
      _ ->
          keep_state_and_data
  catch _:_ ->
      keep_state_and_data
  end.

receive_message(#coap_message{id = ID} = CoAPMessage, #coap_state{task_map = Map} = State) ->
  NewState = sync_state_id(CoAPMessage, State),
  case maps:find(ID, Map) of
    {ok, #task{from = undefined} = Task} ->
        case callback_apply(task_result, [Task, CoAPMessage], NewState) of
          {ok, NewLoop} ->
            {next_state, working, NewState#coap_state{cb_loop = NewLoop}, 
              [{state_timeout, cancel}, {next_event, internal, execute_task}]};
          _ -> 
            {next_state, working, NewState, 
              [{state_timeout, cancel}, {next_event, internal, execute_task}]}
        end;
    {ok, #task{from = From}} ->
        gen_statem:reply(From, CoAPMessage),
        {next_state, working, NewState, 
        [{state_timeout, cancel}, {next_event, internal, execute_task}]};
    _ ->
        case callback_apply(handle_message, CoAPMessage, NewState) of
          {ok, Response, NewLoop} ->
            NewState1 = sync_state_id(Response, NewState),
            do_send(Response, NewState1),
            {keep_state, NewState1#coap_state{cb_loop = NewLoop}};
          {ok, NewLoop} -> 
            {keep_state, NewState#coap_state{cb_loop = NewLoop}};
          _ ->
            keep_state_and_data
        end
  end.


add_task(From, #task{action = request, args = {build_message, Args}} = Task, #coap_state{tasks = Tasks} = State) ->
  case callback_apply(build_message, Args, State) of
    {ok, CoAPMessage} -> 
      State#coap_state{tasks = Tasks ++ [Task#task{args = CoAPMessage, from = From}]};
    {ok, CoAPMessage, NewLoop} -> 
      State#coap_state{tasks = Tasks ++ [Task#task{args = CoAPMessage, from = From}], cb_loop = NewLoop};
    Error -> 
      gen_statem:reply(From, Error),
      State
  end;

add_task(From, #task{action = request} = Task, #coap_state{tasks = Tasks} = State) ->
  State#coap_state{tasks = Tasks ++ [Task#task{from = From}]};

add_task(From, #task{action = wait} = Task, #coap_state{tasks = Tasks} = State) ->
  State#coap_state{tasks = Tasks ++ [Task#task{from = From}]};

add_task(undefined, _, State) ->
  State;

add_task(From, _, State) ->
  gen_statem:reply(From, unsupport_task),
  State.

execute_task(#coap_state{tasks = []} = State) ->
  case callback_apply(task_result, [all, complete], State) of
    {ok, NewLoop} -> 
      {next_state, working, State#coap_state{cb_loop = NewLoop}};
    _ -> 
      {next_state, working, State}
  end;

execute_task(#coap_state{tasks = [#task{action = wait} = Task | Tasks]} = State) ->
  do_wait(Task, State#coap_state{tasks = Tasks});

execute_task(#coap_state{tasks = [#task{action = request} = Task | Tasks]} = State) ->
  do_request(Task, State#coap_state{tasks = Tasks}).

do_wait(Task, State) ->
  {next_state, waiting, State, 
    [{state_timeout, ?REQUEST_TIMEOUT, {Task, ?REQUEST_TIMEOUT, ?MAX_RETRANSMIT}}]}.

do_request(#task{args = CoAPMessage} = Task, #coap_state{task_map = TaskMap} = State) ->
  {Message1 = #coap_message{id = MessageID}, State1} = sync_id(CoAPMessage, State),
  TaskMap1 = maps:put(MessageID, Task, TaskMap),
  do_send(Message1, State1),
  {next_state, waiting, State1#coap_state{task_map = TaskMap1}, 
    [{state_timeout, ?ACK_TIMEOUT, {Task, ?ACK_TIMEOUT, 0}}]}.

do_send(CoAPMessage, #coap_state{socket = Socket, host = Host, port = Port}) ->
  {ok, Package} = coap_message_util:serialize(CoAPMessage),
  gen_udp:send(Socket, Host, Port, Package),
  log("send message >>>>~n ~0p~n", [CoAPMessage]).


sync_id(#coap_message{id = ID} = CoAPMessage, #coap_state{message_id = LocalID} = State) when LocalID > ID ->
  {CoAPMessage#coap_message{id = LocalID}, State#coap_state{message_id = LocalID + 1}};

sync_id(#coap_message{id = ID} = CoAPMessage, State) ->
  {CoAPMessage, State#coap_state{message_id = ID + 1}}.

sync_state_id(#coap_message{id = ID}, #coap_state{message_id = LocalID} = State) when LocalID > ID ->
  State;

sync_state_id(#coap_message{id = ID}, State) -> 
  State#coap_state{message_id = ID + 1}.

log(Message, Format) ->
  io:format(Message, Format).

%%%===================================================================
%%% callbacker apply
%%%===================================================================

callback_apply(Function, Args, #coap_state{callback = Callback, cb_loop = Loop}) when is_list(Args)->
  try erlang:apply(Callback, Function, Args ++ [Loop])
  catch E:R -> 
    log("callback fail, ~0p:~0p(~0p, ~0p), ~0p, ~0p~n",[Callback, Function, Args, Loop, E, R]),
    {fail, E, R}
  end;
callback_apply(Function, Args, #coap_state{callback = Callback, cb_loop = Loop})->
  try erlang:apply(Callback, Function, [Args, Loop])
  catch E:R -> 
    log("callback fail, ~0p:~0p(~0p, ~0p), ~0p, ~0p~n",[Callback, Function, Args, Loop, E, R]),
    {fail, E, R}
  end.


