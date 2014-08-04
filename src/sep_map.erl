%%%-------------------------------------------------------------------
%%% @author Ryuu
%%% @copyright (C) 2014, <PLANT>
%%% @doc
%%%     auto-generation
%%% @end
%%% Created : 3 Aug 2014, 10:47
%%%-------------------------------------------------------------------
-module(sep_map).

-export([get_proto_map/1]).

get_proto_map(privilege) ->
	{1001, [int16, int32, string, {list, kv_pair}]};
get_proto_map(kv_pair) ->
	{1002, [int16, int32]};
get_proto_map(_) ->
	[].
