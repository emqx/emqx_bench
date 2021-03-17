%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 3月 2021 11:26 上午
%%%-------------------------------------------------------------------
-module(coap_simulator).
-author("DDDHuang").

-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, terminate/3,
    code_change/4, callback_mode/0]).

-export([working/3, waiting/3]).

-export([request/2, request/3, send/2, wait/3, wait/4, new_callback/2]).

-include("coap.hrl").

-define(SERVER, ?MODULE).

-record(coap_state, {
    socket                              :: gen_udp:socket(),
    host                                :: binary()     | tuple     | inet:ip_address(),
    port                                :: integer(),
    current_request_id                  :: term(), %% request message
    message_id_index    = 0             :: integer(),
    task_list           = []            :: list(),
    message_callback    = undefined     :: term()
}).

start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, []).

init(Args) ->
    {ok, working, do_init(Args, #coap_state{})}.

do_init([], State) -> State;
do_init([{host, Host} | Args], State) -> do_init(Args, State#coap_state{host = Host});
do_init([{port, Port} | Args], State) -> do_init(Args, State#coap_state{port = Port});
do_init([{task_list, TaskList} | Args], State) -> do_init(Args, State#coap_state{task_list = TaskList});
do_init([{message_callback, CallBack} | Args], State) -> do_init(Args, State#coap_state{message_callback = CallBack});
do_init([{socket, new} | Args], State) -> do_init(Args, State#coap_state{socket = new_socket()});
do_init([{socket, keep} | Args], State) -> do_init(Args, State);
do_init([{socket, Socket} | Args], State) when is_port(Socket) -> do_init(Args, State#coap_state{socket = Socket});
do_init([_ | Args], State) -> do_init(Args, State).


new_socket() -> {ok, Socket} = gen_udp:open(0, [binary]), Socket.

callback_mode() -> [state_functions].
format_status(_Opt, [_PDict, _StateName, _State]) ->
    Status = some_term,
    Status.


working(cast, {send, Message}, State) ->
    do_send(Message, State),
    keep_state_and_data;
working(cast, Command, State) -> cast_command(Command, State);

working({call, From},  {wait, MessageSampler, SamplerArgs, Timeout}, State) ->
    put(wait, {MessageSampler, SamplerArgs, From}),
    {next_state, waiting, State, [{state_timeout, Timeout, wait_timeout}]};
working({call, From}, Message, State) -> call_command(From, Message, State);

working(info, {udp, _Sock, _PeerIP, _PeerPortNo, Packet}, #coap_state{message_callback = Callback} = State) ->
    try coap_message_util:decode(Packet) of
        {ok, CoAPMessage} ->
            %% todo 废弃callback功能 new callback
%%            do_callback(CoAPMessage, Callback),
            {_, NewState} = synchronize_message_id(CoAPMessage, State),
            {keep_state, NewState}
    catch _:_  -> keep_state_and_data end;
working(_, _, _) -> keep_state_and_data.

waiting(state_timeout, {request_timeout, CoAPMessage, Time, Retry}, State) when Retry < ?MAX_RETRANSMIT ->
    do_send(CoAPMessage, State),
    NextTimeout = Time * (Retry + 1),
    {keep_state, State, [{state_timeout, NextTimeout, {request_timeout, CoAPMessage, NextTimeout, Retry + 1}}]};
waiting(state_timeout, {request_timeout, #coap_message{id = ID}, _Time, Retry}, State) when Retry >= ?MAX_RETRANSMIT ->
    {RequestCoAPMessage, From} = get({request, ID}),
    erase({request, ID}),
    {next_state, working, State, [{reply, From, {request_timeout, RequestCoAPMessage}}]};
waiting(state_timeout, wait_timeout, State) ->
    case get(wait) of
        {_MessageSampler, _amplerArgs, From} -> {keep_state, State, [{reply, From, {timeout}}]};
        _ -> keep_state_and_data
    end;
waiting(info, {udp, _Sock, _PeerIP, _PeerPortNo, Packet}, #coap_state{message_id_index = IDNow} = State) ->
    try coap_message_util:decode(Packet) of
        {ok, #coap_message{id = ID} = CoAPMessage} ->
            case ID =< IDNow of
                true ->
                    handle_response(CoAPMessage, State);
                false ->
                    {_, NewState} = synchronize_message_id(CoAPMessage, State),
                    handle_waiting(CoAPMessage, NewState)
            end
    catch _:_  -> keep_state_and_data end;
waiting({call, From}, {request, _CoAPMessage}, State) ->
    {keep_state, State, [{reply, From, {error, last_requesting}}]};
waiting({call, From}, {wait, _MessageSampler, _SamplerArgs}, State) ->
    {keep_state, State, [{reply, From, {error, last_requesting}}]};
waiting({call, From}, Message, State) -> call_command(From, Message, State);
waiting(_, _, _) -> keep_state_and_data.


handle_response({timeout, #coap_message{id = ID}, _Time, _Retry}, State)->
    case get({request, ID}) of
        {_RequestCoAPMessage, From} ->
            erase({request, ID}),
            {keep_state, State, [{reply, From, {fail, timeout}}, {state_timeout, cancel}]};
        _ -> keep_state_and_data
    end;
handle_response(#coap_message{id = ID} = CoAPMessage, State) ->
    case get({request, ID}) of
        {_RequestCoAPMessage, From} ->
            erase({request, ID}),
            {keep_state, State, [{reply, From, {ok, CoAPMessage}}, {state_timeout, cancel}]};
        _ -> keep_state_and_data
    end.

handle_waiting({timeout}, State)->
    case get(wait) of
        undefined -> keep_state_and_data;
        {_MessageSampler, _SamplerArgs, From} ->
            erase(wait),
            {keep_state, State, [{reply, From, {fail, timeout}}]}
    end;
handle_waiting(CoAPMessage, #coap_state{message_callback = Callback} = State)->
    case get(wait) of
        undefined ->
%%            todo new call back
%%            do_callback(CoAPMessage,Callback),
            keep_state_and_data;
        {Sampler, SamplerArgs, From} when is_pid(From) ->
            try Sampler(CoAPMessage, SamplerArgs) of
                {ok, SamplerMessage} ->
                    erase(wait),
                    {keep_state, State, [{reply, From, {ok, SamplerMessage}}]};
                _-> keep_state_and_data
            catch _:_ ->
                erase(wait),
                keep_state_and_data
            end
    end.

%%--------------------------------------------------------------------------------
%%  gen_statem function
%%--------------------------------------------------------------------------------
terminate(_Reason, _StateName, _State = #coap_state{socket = Socket}) ->
    try gen_udp:close(Socket)
    catch _:_ -> ok
    end.

code_change(_OldVsn, StateName, State = #coap_state{}, _Extra) ->
    {ok, StateName, State}.

cast_command({send, CoapMessage}, State) ->
    do_send(CoapMessage, State),
    keep_state_and_data;
cast_command(_, _State) ->
    keep_state_and_data.


call_command(From, {request, message_callback, Args}, #coap_state{message_callback = MessageCallback} = State)->
    case do_message_callback(MessageCallback,Args) of
        {ok, CoAPMessage} -> call_command(From, {request, CoAPMessage}, State);
        _ -> {keep_state, State, [{reply, From, {fail, message_callback_error}}]}
    end;
call_command(From, {request, CoAPMessage}, State)->
    do_request(CoAPMessage, From, State);
call_command(From, {new_callback, Callback}, State)->
    {keep_state, do_init([{message_callback, Callback}], State),[{reply, From, ok}]};
call_command(From, _, State)->
    {keep_state, State,[{reply, From, un_support_call}]}.

do_message_callback({CallbackFunction, MessageArgs}, CallbackArgs)->
    %% look: (CallbackArgs, MessageArgs) , new args fist
    try CallbackFunction(CallbackArgs, MessageArgs)
    catch _:_  -> {error, message_error} end.

%%--------------------------------------------------------------------------------
%%  api
%%--------------------------------------------------------------------------------
request(Pid, CoAPMessage) ->
    try gen_statem:call(Pid, {request, CoAPMessage}, cal_max_timeout())
    catch _:_ -> {fail, timeout} end.
request(Pid, message_callback, CallbackArgs) ->
    try gen_statem:call(Pid, {request, message_callback, CallbackArgs}, cal_max_timeout())
    catch _:_ -> {fail, timeout} end.

send(Pid, CoAPMessage) ->
    gen_statem:cast(Pid, {send, CoAPMessage}).

wait(Pid, MessageSampler, SamplerArgs)->
    wait(Pid, MessageSampler, SamplerArgs, 5000).
wait(Pid, MessageSampler, SamplerArgs, Timeout)->
    try gen_statem:call(Pid, {wait, MessageSampler, SamplerArgs, Timeout}, Timeout)
    catch _:_ -> {fail, timeout} end.

new_callback(Pid, Callback)->
    try  gen_statem:call(Pid, {new_callback, Callback}, 200)
    catch _:_ -> {fail, timeout} end.


%%--------------------------------------------------------------------------------
%%  internal function
%%--------------------------------------------------------------------------------
cal_max_timeout()->
    cal_max_timeout(?ACK_TIMEOUT, 0).
cal_max_timeout(Timeout, MaxRetry) when MaxRetry >= ?MAX_RETRANSMIT->
    Timeout;
cal_max_timeout(Timeout, Retry) ->
    cal_max_timeout(Timeout + (Timeout * (Retry + 1)), (Retry + 1)).
%%--------------------------------------------------------------------------------
%%  send request ,will synchronize message id
%%--------------------------------------------------------------------------------
do_request(CoAPMessage, From, State) ->
    {#coap_message{id = ID} = NewMessage, NewState} = synchronize_message_id(CoAPMessage, State),
    put({request, ID}, {NewMessage,From}),
    do_send(NewMessage, NewState),
    {next_state, waiting, NewState,
        [{state_timeout, ?ACK_TIMEOUT, {request_timeout, CoAPMessage, ?ACK_TIMEOUT, 0}}]}.

%%--------------------------------------------------------------------------------
%%  udp send api,will not synchronize message id
%%--------------------------------------------------------------------------------
-spec do_send(#coap_message{}, #coap_state{}) -> #coap_state{}.
do_send(CoAPMessage, #coap_state{socket = Socket, host = Host, port = Port} = State) ->
    io:format("send  >>> ~0p~n", [CoAPMessage]),
    {ok, Package} = coap_message_util:encode(CoAPMessage),
    gen_udp:send(Socket, Host, Port, Package),
    {keep_state, State}.

%%--------------------------------------------------------------------------------
%%  synchronize message id
%%--------------------------------------------------------------------------------
synchronize_message_id(#coap_message{id = MessageID} = CoAPMessage,#coap_state{message_id_index = Index} = State)
    when MessageID =:= -1 ->
    {CoAPMessage#coap_message{id = Index}, State#coap_state{message_id_index = Index + 1}};
synchronize_message_id(#coap_message{id = MessageID} = CoAPMessage,#coap_state{message_id_index = Index} = State)
    when MessageID >= Index ->
    {CoAPMessage, State#coap_state{message_id_index = MessageID + 1}};
synchronize_message_id(#coap_message{id = MessageID} = CoAPMessage,#coap_state{message_id_index = Index} = State)
    when MessageID =< Index ->
    {CoAPMessage#coap_message{id = Index}, State#coap_state{message_id_index = Index + 1}}.

%%--------------------------------------------------------------------------------
%%  impl callback
%%--------------------------------------------------------------------------------
%%-spec do_callback(#coap_message{}, {Function :: fun(), Args :: term()})-> ignore.
%%do_callback(CoAPMessage, {Function, Args}) when is_function(Function) ->
%%    try Function(CoAPMessage, Args), ignore
%%    catch _:_  -> ignore end;
%%do_callback(_, _) -> ignore.
