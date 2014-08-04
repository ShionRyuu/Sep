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
-module(sep).
-author("Ryuu").

-include("sep.hrl").

-export([write_proto/1, read_proto/1]).

%% =====================================
%% Encode protocol
%% =====================================

%% @doc encode protocol
write_proto(Rec) when is_tuple(Rec) ->
	RecordName = erlang:element(1, Rec),
	case sep_map:get_proto_map(RecordName) of
		[] ->
			erlang:error("undefined proto map");
		{ProtoNum, TypeList} ->
			[_ | List] = erlang:tuple_to_list(Rec),
			write_proto(List, TypeList, <<ProtoNum:16>>)
	end.

write_proto([], [], Acc) ->
    Acc;
write_proto([], _, _Acc) ->
	erlang:error("invalid proto map");
write_proto(_, [], _Acc) ->
	erlang:error("invalid proto map");
write_proto([V | VR], [T | TR], Acc) ->
	Bin = write_type(V, T),
	write_proto(VR, TR, <<Acc/binary, Bin/binary>>).

%% @doc encode type, free to add other type
write_type(V, int8) when is_integer(V) ->
	<<V:8>>;
write_type(V, int16) when is_integer(V) ->
	<<V:16>>;
write_type(V, int32) when is_integer(V) ->
	<<V:32>>;
write_type(V, string) when is_list(V); is_binary(V) ->
	write_string(V);
write_type(V, {list, T}) when is_list(V) ->
	Len = erlang:length(V),
	write_list_type(V, T, <<Len:16>>);
write_type(Rec, RecName) when is_tuple(Rec), is_atom(RecName) ->
	write_proto(Rec);
write_type(_V, _Unknown) ->
	erlang:error("undefined proto type").

%% @doc encode list
write_list_type([], _TL, Acc) -> 
	Acc;
write_list_type([V | VR], TL, Acc) ->
	Bin = write_type(V, TL),
	write_list_type(VR, TL, <<Acc/binary, Bin/binary>>).

%% @doc encode string
write_string(S) when is_list(S)->
    SB = iolist_to_binary(S),
    L = byte_size(SB),
    <<L:16, SB/binary>>;
write_string(S) when is_binary(S)->
    L = byte_size(S),
    <<L:16, S/binary>>;
write_string(S) when is_integer(S)->
	SS = integer_to_list(S),
	SB = list_to_binary(SS),
    L = byte_size(SB),
    <<L:16, SB/binary>>;
write_string(_S) ->
	<<0:16, <<>>/binary>>.


%% =====================================
%% Decode protocol
%% =====================================

%% @doc decode protocol
read_proto(<<Proto:16, Rest/binary>>) ->
    RecName = sep_rec:get_proto_rec(Proto),
    case sep_map:get_proto_map(RecName) of
        [] ->
            erlang:error("undefined proto rec");
        {_ProtoNum, TypeList} ->
            read_proto(Rest, TypeList, [RecName])
    end.

read_proto(<<>>, [], Acc) ->
    RL = lists:reverse(Acc),
    {erlang:list_to_tuple(RL), <<>>};
read_proto(<<>>, _, _Acc) ->
    erlang:error("invalid proto rec");
read_proto(_Bin, [], _Acc) ->
    erlang:error("invalid proto rec");
read_proto(Bin, [T | TL], Acc) ->
    {V, NewBin} = read_type(Bin, T),
    read_proto(NewBin, TL, [V | Acc]).

%% @doc decode type, free to add other type
read_type(<<Int8:8, Rest/binary>>, int8) ->
    {Int8, Rest};
read_type(<<Int16:16, Rest/binary>>, int16) ->
    {Int16, Rest};
read_type(<<Int32:32, Rest/binary>>, int32) ->
    {Int32, Rest};
read_type(Bin, string) ->
    read_string(Bin);
read_type(<<Len:16, Bin/binary>>, {list, T}) ->
    read_list_type(Bin, T, Len, []);
read_type(<<_RecNum:16, _/binary>> = Bin, RecName) when is_atom(RecName) ->
    read_proto(Bin);
read_type(_Bin, _Unknown) ->
    erlang:error("undefined proto rec").

%% @doc decode list
read_list_type(Bin, _TL, 0, Acc) ->
    {lists:reverse(Acc), Bin};
read_list_type(Bin, TL, Len, Acc) ->
    {V, NewBin} = read_type(Bin, TL),
    read_list_type(NewBin, TL, Len - 1, [V | Acc]).

%% @doc decode string
read_string(Bin) ->
    case Bin of
        <<Len:16, Bin1/binary>> ->
            case Bin1 of
                <<Str:Len/binary-unit:8, Rest/binary>> ->
                    {binary_to_list(Str), Rest};
                _R1 ->
                    {[], <<>>}
            end;
        _R1 ->
            {[], <<>>}
    end.

