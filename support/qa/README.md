# Quality Assurance helpers

This directory contains some helper function to be run mannualy for Quality
Assurance plan.

Those test are not currently automated, as they sometime represent complex
contexts to recreate. Better have them manual for the time being, rather than no
test at all :-)

## helpchange

This help is to test long working httpchange. Cowboy has some build-in timeout
that barrel compensate with automatic reconnect. This helper enable to start
a change feed and observe the change feed is still working fine overtime.

You should let it run for some minutes, posting some documents.

Start a shell:

    $ rebar3 shell

Type in shell:

```erlang
c("support/qa/httpchange.erl").
Conn = helpchange:start().
helpchange:post(Conn).
helpchange:post(Conn).
```

```
1> c("support/qa/helpchange.erl").
{ok,helpchange}
2> Conn = helpchange:start().
10:50:46.062 [warning] lager_error_logger_h dropped 110 messages in the last second that exceeded the limit of 50 messages/sec
#{db_url => <<"http://localhost:7080/dbs/testdb">>,pool => testdb}
3> 10:50:46.126 [info] [barrel_httpc_changes] connected to conn=#{db_url => <<"http://localhost:7080/dbs/testdb">>,pool => testdb}
change=#{<<"changes">> => [<<"1-1d082e465d26d649aaa6c474f3b861f736b090b9c8cb06949269e648fc2c202d">>],
         <<"id">> => <<"dddd">>,
         <<"rev">> => <<"1-1d082e465d26d649aaa6c474f3b861f736b090b9c8cb06949269e648fc2c202d">>,
         <<"rid">> => <<"AAAAAAAAAAI=">>,
         <<"seq">> => 2}
change=#{<<"changes">> => [<<"2-d72bbb43e659b763e62fbc464469a2b5260394c9aa484087977944040c273366">>],
         <<"deleted">> => true,
         <<"id">> => <<"mydoc">>,
         <<"rev">> => <<"2-d72bbb43e659b763e62fbc464469a2b5260394c9aa484087977944040c273366">>,
         <<"rid">> => <<"AAAAAAAAAAE=">>,
         <<"seq">> => 3}
3> helpchange:post(Conn).
change=#{<<"changes">> => [<<"1-63e9113aea405969d95235162c4e43fc29ea5b6e071ea322aa22b1050d8857a4">>],
         <<"id">> => <<"933e8a2e-228b-4b91-a29a-1c468ef0c7ad">>,
         <<"rev">> => <<"1-63e9113aea405969d95235162c4e43fc29ea5b6e071ea322aa22b1050d8857a4">>,
         <<"rid">> => <<"AAAAAAAAAAM=">>,
         <<"seq">> => 4}
{ok,<<"933e8a2e-228b-4b91-a29a-1c468ef0c7ad">>,
    <<"1-63e9113aea405969d95235162c4e43fc29ea5b6e071ea322aa22b1050d8857a4">>}
4> helpchange:post(Conn).
change=#{<<"changes">> => [<<"1-18e8cddb30c4bf21dde98b57f2966fecb52bdf01f9d7f52cb63679a25e981c89">>],
         <<"id">> => <<"e31112d2-74a3-4c74-8495-d77466ac8582">>,
         <<"rev">> => <<"1-18e8cddb30c4bf21dde98b57f2966fecb52bdf01f9d7f52cb63679a25e981c89">>,
         <<"rid">> => <<"AAAAAAAAAAQ=">>,
         <<"seq">> => 5}
{ok,<<"e31112d2-74a3-4c74-8495-d77466ac8582">>,
    <<"1-18e8cddb30c4bf21dde98b57f2966fecb52bdf01f9d7f52cb63679a25e981c89">>}
5> 10:51:46.129 [warning] [barrel_httpc_changes] hackney connection done
10:51:46.630 [warning] [barrel_httpc_changes] try to reconnect (5)
10:51:46.651 [info] [barrel_httpc_changes] connected to conn=#{db_url => <<"http://localhost:7080/dbs/testdb">>,pool => testdb}
10:52:46.650 [warning] [barrel_httpc_changes] hackney connection done
10:52:47.152 [warning] [barrel_httpc_changes] try to reconnect (5)
10:52:47.156 [info] [barrel_httpc_changes] connected to conn=#{db_url => <<"http://localhost:7080/dbs/testdb">>,pool => testdb}
10:53:47.156 [warning] [barrel_httpc_changes] hackney connection done
10:53:47.657 [warning] [barrel_httpc_changes] try to reconnect (5)
10:53:47.661 [info] [barrel_httpc_changes] connected to conn=#{db_url => <<"http://localhost:7080/dbs/testdb">>,pool => testdb}
10:54:47.661 [warning] [barrel_httpc_changes] hackney connection done
10:54:48.162 [warning] [barrel_httpc_changes] try to reconnect (5)
10:54:48.182 [info] [barrel_httpc_changes] connected to conn=#{db_url => <<"http://localhost:7080/dbs/testdb">>,pool => testdb}
```

## helpreplicate


Start a shell:

    $ rebar3 shell

Type in shell:

```erlang
c("support/qa/httpreplicate.erl").
Conn = helpreplicate:start().
helpchange:post_on_source(1).
helpchange:post_on_source(2).
helpchange:post_on_source(3).
helpchange:target_docs().
```

