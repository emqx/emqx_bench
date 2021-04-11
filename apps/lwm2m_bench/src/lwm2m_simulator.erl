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
-include_lib("emqx_bench/include/emqx_bench.hrl").

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

-export([init/1, build_message/2, handle_message/2]).

start_link(Args) ->
    Fun = fun()-> ok end,
    spawn(Fun),
    LWArgs = [{callback, ?MODULE, Args}],
    gen_statem:start_link(?COAP, Args ++ LWArgs, []).

%%--------------------------------------------------------------------------------
%%  api
%%--------------------------------------------------------------------------------
bootstrap_sm2(Pid) -> lw_request(Pid, ?FUNCTION_NAME).
bootstrap_sm9(Pid) -> lw_request(Pid, ?FUNCTION_NAME).
register(Pid) -> lw_request(Pid, ?FUNCTION_NAME).
register_standard_module(Pid) -> lw_request(Pid, ?FUNCTION_NAME).
publish(Pid, Payload) -> lw_request(Pid, {?FUNCTION_NAME, Payload}).
deregister(Pid) -> lw_request(Pid, ?FUNCTION_NAME).

%%--------------------------------------------------------------------------------
%%  internal function
%%--------------------------------------------------------------------------------
init(Args)-> 
    lists:foldl(fun(Arg, State) -> do_init(Arg, State) end, #lw_state{}, Args).
do_init({imei, IMEI}, State) -> State#lw_state{imei = IMEI};
do_init({register_payload, Payload} , State) -> State#lw_state{register_payload = Payload};
do_init({lifetime, Lifetime} , State) ->
    State#lw_state{lifetime = list_to_binary(integer_to_list(Lifetime))};

do_init({data_type, pass_through} , State) -> State#lw_state{data_type = pass_through};
do_init({data_type, json} , State) -> State#lw_state{data_type = json};
do_init({data_type, binary} , State) -> State#lw_state{data_type = binary};
do_init({data_type, _} , State) -> State#lw_state{data_type = pass_through};

do_init({sm2_public_key, Data}, State) -> State#lw_state{sm2_public_key = Data};
do_init({sm9_public_key, Data}, State) -> State#lw_state{sm9_public_key = Data};

do_init({task_list, TaskList}, State) -> State#lw_state{task_list = TaskList};
do_init({task_callback, CallBack}, State) -> State#lw_state{task_callback = CallBack};
do_init({_, _}, State) -> State.

lw_request(Pid, Args) -> 
    coap_simulator:request(Pid, build_message, Args).

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
build_message({publish, _Payload}, #lw_state{token_19_0_0 = undefined}) ->
    {error, token_19_0_0_undefined};
build_message({publish, Payload}, #lw_state{data_type = ProductDataType, token_19_0_0 = Token} = State) ->
    {ok, lwm2m_message_util:publish(ProductDataType, Token, Payload), State};
build_message(deregister, #lw_state{imei = IMEI} = State) ->
    {ok, lwm2m_message_util:deregister(IMEI), State};
build_message(_Args, _State) -> {error, no_message}.

handle_message(#coap_message{type = ?CON, method = ?GET, id = MessageID, token = Token} = CoAPMessage, State) ->
    {ok, Path} = coap_message_util:get_uri_path(CoAPMessage),
    {ResponseCoAPMessage, NewState} = case Path of
        <<"/3/0">> ->
            {lwm2m_message_util:response_auto_observe_3_0(MessageID, Token), State};
        <<"/19/0/0">> ->
            {lwm2m_message_util:response_auto_observe_19_0_0(MessageID, Token), State#lw_state{token_19_0_0 = Token}};
        <<"/4/0/8">> ->
            {lwm2m_message_util:response_auto_observe_4_0_8(MessageID, Token), State};
        <<"19/1/0">> ->
            {lwm2m_message_util:simple_ack(CoAPMessage, ?CHANGED), State};
        _ -> ignore
    end,
    gen_statem:cast(self(), {send, ResponseCoAPMessage}),
    {ok, NewState};
handle_message(#coap_message{type = ?CON, method = ?PUT} = CoAPMessage, State) ->
    {ResponseCoAPMessage, NewState} = {lwm2m_message_util:simple_ack(CoAPMessage, ?CHANGED), State},
    gen_statem:cast(self(), {send, ResponseCoAPMessage}),
    {ok, NewState};
handle_message(_Message, Loop) -> {ok, Loop}.

% task_response(Message, Task, #lw{}) ->

