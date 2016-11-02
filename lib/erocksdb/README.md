erocksdb
========

Erlang bindings to [RocksDB](https://github.com/facebook/rocksdb) datastore.

## Features

- rocksdb 4.9 support
- all basics db operations
- snapshots support
- checkpoint support
- Tested on macosx and 

## Usage

Build it using rebar 3

    $ rebar3 compile

```erl
  os:cmd("rm -rf /tmp/erocksdb.open.test"),
  {ok, Ref} = erocksdb:open("testdb", [{create_if_missing, true}], []),
  true = erocksdb:is_empty(Ref),
  ok = erocksdb:put(Ref, <<"abc">>, <<"123">>, []),
  false = erocksdb:is_empty(Ref),
  {ok, <<"123">>} = erocksdb:get(Ref, <<"abc">>, []),
  {ok, 1} = erocksdb:count(Ref),
  not_found = erocksdb:get(Ref, <<"def">>, []),
  ok = erocksdb:delete(Ref, <<"abc">>, []),
  not_found = erocksdb:get(Ref, <<"abc">>, []),
  true = erocksdb:is_empty(Ref)
  erocksdb:close(Ref).
```
 

## License

erocksdb's license is [Apache License Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html)

## Codebase

This code is based on [basho’s eleveldb](https://github.com/basho/eleveldb).

## Sponsors

LeoProject/LeoFS is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and supported by [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
Enki Multimedia (https://enkim.eu)
