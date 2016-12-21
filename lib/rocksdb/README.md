

# erlang-rocksdb - HTTP client library in Erlang #

Copyright (c) 2016 Benoît Chesneau.

__Version:__ 0.6.0 Erlang wrapper for RocksDB.

Feedback and pull requests welcome! If a particular feature of RocksDB is important to you, please let me know by opening an issue, and I'll prioritize it.

### Examples

```
{ok, Db} = rocksdb:open("path/for/rocksdb/storage", []),
rocksdb:put(Db, <<"my key">>, <<"my value">>),
case rocksdb:get(Db, <<"my key">>, []) of
  {ok, Value} => io:format("retrieved value %p~n", [Value]);
  not_found => io:format("value not found~n", []);
  Error -> io:format("operational problem encountered: %p~n", [Error])
end,
rocksdb:close(Db),
rocksdb:destroy(Db).
```

# Features

- rocksdb 4.13
- all basics db operations
- snapshots support
- checkpoint support
- column families support
- Tested on macosx and

## Notes

This project is a fork of [erocksdb](https://github.com/leo-project/erocksdb) sponsored by Enki Multimedia (https://enkim.eu).


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://gitlab.com/barrel-db/erlang-rocksdb/blob/master/doc/rocksdb.md" class="module">rocksdb</a></td></tr>
<tr><td><a href="http://gitlab.com/barrel-db/erlang-rocksdb/blob/master/doc/rocksdb_bump.md" class="module">rocksdb_bump</a></td></tr></table>

