#Sep - simple erlang protobuf

[![Build Status](https://secure.travis-ci.org/genesislive/Sep.png?branch=master)](http://travis-ci.org/genesislive/Sep)

Sep, short for simple erlang protobuf, is a simple erlang version of protobuf.

## Usage

```erl-sh
1> rd(privilege, {id = 0, role_id = 0, name = "", data = []}).
privilege
2> rd(kv_pair, {key = 0, val = 0}).
kv_pair
3> R1 = #privilege{id = 2, role_id = 2, name = "liu", data = [#kv_pair{key
= 5, val = 14}]}.
#privilege{id = 2,role_id = 2,name = "liu",
           data = [#kv_pair{key = 5,val = 14}]}
4> Bin1 = sep:write_proto(R1).
<<3,233,0,2,0,0,0,2,0,3,108,105,117,0,1,3,234,0,5,0,0,0,14>>
5> sep:read_proto(Bin1).
{#privilege{id = 2,role_id = 2,name = "liu",
            data = [#kv_pair{key = 5,val = 14}]},
 <<>>}
6>
```

## Protobuf-like syntax

```
message [ProtocolName] [ProtocolNum] {
	[required, repeated] [int8, int16, int32, string, user-def type] Identifier[ = DefaultValue];
}
```

## Script

```sh
php ./script/proto_gen.php sep
```

## Authors

- Shion Ryuu <genesislive@outlook.com>

## Todo

1. script used for generating .erl and .as3 files from .proto file
