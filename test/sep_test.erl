%% -------------------------------------------------------------------
%%
%% Sep: Simple erlang protobuf
%%
%% Copyright (c) 2014 Shion Ryuu (genesislive@outlook.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(sep_test).
-author("Ryuu").

-include("sep.hrl").

-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

sep_test() ->
    R1 = #privilege{id = 2, role_id	= 2, name = "liu", data = [#kv_pair{key = 5, val = 14}]},
    Bin1 = sep:write_proto(R1),
    DR1 = sep:read_proto(Bin1),
    ?assertEqual(DR1, {R1, <<>>}).
