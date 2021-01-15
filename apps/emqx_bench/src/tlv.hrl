%%%-------------------------------------------------------------------
%%% @author huangdi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 8月 2020 5:12 下午
%%%-------------------------------------------------------------------
-author("huangdi").


-record(tlv, {
    tag, id, value
}).
%% Type              =>  value(2#)
%%  Single_Multiple  =>  00
%%  Single           =>  01
%%  Multiple         =>  10
%%  Resources_Value  =>  11
-define(T_Single_Multiple, 2#0).
-define(T_Single, 2#01).
-define(T_Multiple, 2#10).
-define(T_Resources_Value, 2#11).