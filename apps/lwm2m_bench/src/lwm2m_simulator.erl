%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 3月 2021 10:52 上午
%%%-------------------------------------------------------------------
-module(lwm2m_simulator).
-author("DDDHuang").

%% API
-export([]).
-include_lib("coap_bench/include/coap.hrl").

-define(COAP, coap_simulator).
-record(lw_state, {
    imei                = <<"">>        :: binary(),
    sm2_public_key      = <<"">>        :: binary(),
    sm9_public_key      = <<"">>        :: binary(),
    data_type           = json          :: pass_through | json      | binary,
    lifetime            = <<"300">>     :: binary(),
    register_payload    = <<"</>;rt=\"oma.lwm2m\";ct=11543,<3/0>,<19/0>">>,
    token_19_0_0        = undefined     :: undefined   | binary(),
    task_list           = []            :: list(),
    task_callback       = undefined     :: term()
}).

-export([   start_link/1]).

-export([   bootstrap_sm2/1,
            bootstrap_sm9/1,
            register/1,
            register_standard_module/1,
            publish/2,
            deregister/1]).

-export([build_message/2, handle_message/2]).

start_link(Args) ->
    LWArgs = [{callback_module, ?MODULE}, {callback_loop, do_init(Args, #lw_state{})}],
    gen_statem:start_link(?COAP, Args ++ LWArgs, []).

do_init([], State) -> State;
do_init([{imei, IMEI} | Args], State) -> do_init(Args, State#lw_state{imei = IMEI});
do_init([{register_payload, Payload} | Args], State) -> do_init(Args, State#lw_state{register_payload = Payload});
do_init([{lifetime, Lifetime} | Args], State) ->
    do_init(Args, State#lw_state{lifetime = list_to_binary(integer_to_list(Lifetime))});

do_init([{data_type, pass_through} | Args], State) -> do_init(Args, State#lw_state{data_type = pass_through});
do_init([{data_type, json} | Args], State) -> do_init(Args, State#lw_state{data_type = json});
do_init([{data_type, binary} | Args], State) -> do_init(Args, State#lw_state{data_type = binary});
do_init([{data_type, _} | Args], State) -> do_init(Args, State#lw_state{data_type = pass_through});

do_init([{task_list, TaskList} | Args], State) -> do_init(Args, State#lw_state{task_list = TaskList});
do_init([{task_callback, CallBack} | Args], State) -> do_init(Args, State#lw_state{task_callback = CallBack});
do_init([{_, _} | Args], State) -> do_init(Args, State).
%%--------------------------------------------------------------------------------
%%  api
%%--------------------------------------------------------------------------------
bootstrap_sm2(Pid) -> lw_request(Pid, ?FUNCTION_NAME).
bootstrap_sm9(Pid) -> lw_request(Pid, ?FUNCTION_NAME).
register(Pid) -> lw_request(Pid, ?FUNCTION_NAME).
register_standard_module(Pid) -> lw_request(Pid, ?FUNCTION_NAME).
publish(Pid,Payload) -> lw_request(Pid, {?FUNCTION_NAME, Payload}).
deregister(Pid) -> lw_request(Pid, ?FUNCTION_NAME).
%%--------------------------------------------------------------------------------
%%  internal function
%%--------------------------------------------------------------------------------
lw_request(Pid, Args) -> coap_simulator:request(Pid, build_message, Args).

%%--------------------------------------------------------------------------------
%%  coap simulator callback
%%--------------------------------------------------------------------------------
build_message(bootstrap_sm2, #lw_state{imei = IMEI, sm2_public_key = PubKey} = State) ->
    {ok, lwm2m_message_util:bootstrap_sm2(IMEI, PubKey), State};
build_message(bootstrap_sm9, #lw_state{imei = IMEI, sm2_public_key = PubKey} = State) ->
    {ok, lwm2m_message_util:bootstrap_sm9(IMEI, PubKey), State};
build_message(register, #lw_state{imei = IMEI, lifetime = LifeTime, register_payload = Payload} = State) ->
    {ok, lwm2m_message_util:register(IMEI, LifeTime, Payload), State};
build_message(register_standard_module, #lw_state{imei = IMEI, lifetime = LifeTime, register_payload = Payload} = State) ->
    {ok, lwm2m_message_util:register_standard_module(IMEI, LifeTime, Payload), State};
build_message({publish, Payload}, #lw_state{data_type = ProductDataType, token_19_0_0 = Token} = State) ->
    {ok, lwm2m_message_util:publish(ProductDataType, Token, Payload), State};
build_message(deregister, #lw_state{imei = IMEI} = State) ->
    {ok, lwm2m_message_util:deregister(IMEI), State};
build_message(_Args, _State) -> {error, no_message}.

handle_message(#coap_message{type = ?CON, id = MessageID, token = Token} = CoAPMessage, State) ->
    io:format("<<<<  ~0p~n",[CoAPMessage]),
    {ok, Path} = coap_message_util:get_uri_path(CoAPMessage),
    {ResponseCoAPMessage, NewState} = case Path of
        <<"/3/0">> ->
            {lwm2m_message_util:response_auto_observe_3_0(MessageID, Token), State};
        <<"/19/0/0">> ->
            {lwm2m_message_util:response_auto_observe_19_0_0(MessageID, Token), State#lw_state{token_19_0_0 = Token}};
        <<"/4/0/8">> ->
            {lwm2m_message_util:response_auto_observe_3_0(MessageID, Token), State};
        <<"19/1/0">> ->
            {lwm2m_message_util:simple_ack(CoAPMessage, ?CHANGED), State};
        _ -> ignore
    end,
    coap_simulator:send(self(), ResponseCoAPMessage),
    {ok, NewState};
handle_message(_Message, Loop) -> {ok, Loop}.

