%%%-------------------------------------------------------------------
%%% @author DDDHuang
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 3月 2021 10:52 上午
%%%-------------------------------------------------------------------
-module(base_coap).
-author("DDDHuang").

%% API
-export([]).
-include_lib("coap_bench/include/coap.hrl").

-define(COAP, coap_simulator).
-record(lw_state, {
    imei                                :: binary(),
    sm2_public_key                      :: binary(),
    sm9_public_key                      :: binary(),
    data_type           = json          :: pass_through | json      | binary,
    lifetime            = <<"300">>     :: binary(),
    register_payload    = <<"</>;rt=\"oma.lwm2m\";ct=11543,<3/0>,<19/0>">>,
    token_19_0_0        = undefined     :: undefined   | binary(),
    task_list           = []            :: list(),
    %% task_callback :: {
    %%                      fun( Task :: #task{},Result :: success | {fail, Reason}, CallBackArgs :: any()),
    %%                      CallBackArgs :: any()
    %%                  }
    task_callback       = undefined    :: term()
}).

-export([   start_link/1,
            message_callback/2]).

-export([   bootstrap_sm2/1,
            bootstrap_sm9/1,
            register/1,
            register_standard_module/1,
            publish/2,
            deregister/1]).

start_link(Args) ->
    LWArgs = [{message_callback, message_callback / 2, do_init(Args,#lw_state{})}],
    gen_statem:start_link(?COAP, Args ++ LWArgs, []).

do_init([], State) -> State;
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

message_callback(bootstrap_sm2, #lw_state{imei = IMEI, sm2_public_key = PubKey}) ->
    {ok, lwm2m_message_util2:bootstrap_sm2(IMEI, PubKey)};
message_callback(bootstrap_sm2, #lw_state{imei = IMEI, sm9_public_key = PubKey}) ->
    {ok, lwm2m_message_util2:bootstrap_sm9(IMEI, PubKey)};
message_callback(register, #lw_state{imei = IMEI, lifetime = LifeTime, register_payload = Payload}) ->
    {ok, lwm2m_message_util2:register(IMEI,LifeTime,Payload)};
message_callback(register_standard_module, #lw_state{imei = IMEI, lifetime = LifeTime, register_payload = Payload}) ->
    {ok, lwm2m_message_util2:register_standard_module(IMEI,LifeTime,Payload)};
message_callback({publish, Payload}, #lw_state{data_type = ProductDataType,token_19_0_0 = Token}) ->
    {ok, lwm2m_message_util2:publish(ProductDataType, Token, Payload)};
message_callback(deregister, #lw_state{imei = IMEI}) ->
    {ok, lwm2m_message_util2:deregister(IMEI)};
message_callback(_, _) ->
    {fail, un_support}.


bootstrap_sm2(Pid) -> lw_request(Pid, ?FUNCTION_NAME).
bootstrap_sm9(Pid) -> lw_request(Pid, ?FUNCTION_NAME).
register(Pid) -> lw_request(Pid, ?FUNCTION_NAME).
register_standard_module(Pid) -> lw_request(Pid, ?FUNCTION_NAME).
publish(Pid,Payload) -> lw_request(Pid, {?FUNCTION_NAME, Payload}).
deregister(Pid) -> lw_request(Pid, ?FUNCTION_NAME).

lw_request(Pid, Message) -> coap_simulator:request(Pid, message_callback, Message).



