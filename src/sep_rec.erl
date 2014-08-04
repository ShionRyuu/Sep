%%%-------------------------------------------------------------------
%%% @author Ryuu
%%% @copyright (C) 2014, <PLANT>
%%% @doc
%%%     auto-generation
%%% @end
%%% Created : 3 Aug 2014, 10:47
%%%-------------------------------------------------------------------
-module(sep_rec).

-export([get_proto_rec/1]).

get_proto_rec(1001) ->
    privilege;
get_proto_rec(1002) ->
    kv_pair;
get_proto_rec(_) ->
    [].

