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
-export([init/1, format_status/2, working/3, terminate/3,
    code_change/4, callback_mode/0]).

-export([request/2, send/2]).

-include("coap.hrl").

-define(SERVER, ?MODULE).

-record(coap_state, {
    socket                              :: gen_udp:socket(),
    host                                :: binary()     | tuple     | inet:ip_address(),
    port                                :: integer(),
    current_request_id                  :: term(), %% request message
    message_id_index    = 0             :: integer(),
    task_list           = []            :: list(),
    %% task_callback :: {
    %%                      fun( Task :: #task{},Result :: success | {fail, Reason}, CallBackArgs :: any()),
    %%                      CallBackArgs :: any()
    %%                  }
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
do_init([{socket, Socket} | Args], State) -> do_init(Args, State#coap_state{socket = Socket});
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
working({call, From}, {request, CoAPMessage}, State) ->
    do_request(CoAPMessage, From, State);
working(info, {udp, _Sock, _PeerIP, _PeerPortNo, Packet}, #coap_state{message_callback = Callback} = State) ->
    try coap_message_util:decode(Packet) of
        {ok, #coap_message{id = ID} = CoAPMessage} ->
            case get({request, ID}) of
                {RequestCoAPMessage, From} ->
                    erase({request, ID}),
                    {keep_state, State, [{reply, From, {ok, RequestCoAPMessage}}]};
                _ ->
                    do_callback(CoAPMessage, Callback),
                    {_, NewState} = synchronize_message_id(CoAPMessage, State),
                    {keep_state, NewState}
            end
    catch _:_  -> keep_state_and_data end;
working(state_timeout, {request_timeout, CoAPMessage, Time, Retry}, State) when Retry < ?MAX_RETRANSMIT ->
    do_send(CoAPMessage, State),
    NextTimeout = Time * (Retry + 1),
    {keep_state, State, [{state_timeout, NextTimeout, {request_timeout, CoAPMessage, NextTimeout, Retry + 1}}]};
working(state_timeout, {request_timeout, #coap_message{id = ID}, _Time, Retry}, State) when Retry >= ?MAX_RETRANSMIT ->
    {RequestCoAPMessage, From} = get({request, ID}),
    erase({request, ID}),
    {keep_state, State, [{reply, From, {request_timeout, RequestCoAPMessage}}]};
working(_, _, _) -> keep_state_and_data.

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



%%--------------------------------------------------------------------------------
%%  api
%%--------------------------------------------------------------------------------
request(Pid, CoAPMessage) ->
    gen_statem:call(Pid, {request, CoAPMessage}).

send(Pid, CoAPMessage) ->
    gen_statem:cast(Pid, {send, CoAPMessage}).


%%--------------------------------------------------------------------------------
%%  internal function
%%--------------------------------------------------------------------------------

%%--------------------------------------------------------------------------------
%%  send request ,will synchronize message id
%%--------------------------------------------------------------------------------
do_request(CoAPMessage, From, State) ->
    {#coap_message{id = ID} = NewMessage, NewState} = synchronize_message_id(CoAPMessage, State),
    put({request, ID}, {NewMessage,From}),
    do_send(NewMessage, NewState),
    {keep_state, NewState,
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
-spec do_callback(#coap_message{}, term())-> ignore.
do_callback(CoAPMessage, {Function, Args}) ->
    try Function(CoAPMessage, Args), ignore
    catch _:_  -> ignore end;
do_callback(_, _) -> ignore.

