%%%-------------------------------------------------------------------
%%% @author Ryuu
%%% @copyright (C) 2014, <PLANT>
%%% @doc
%%%     auto-generation
%%% @end
%%% Created : 3 Aug 2014, 10:47
%%%-------------------------------------------------------------------
-author("Ryuu").

%% metadata£º[int16, int32, string, {list, kv_pair}].
-record(privilege, {id = 0, role_id = 0, name = "", data = []}).

%% metadata: [int16, int32]
-record(kv_pair, {key = 0, val = 0}).
