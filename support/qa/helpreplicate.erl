-module(helpreplicate).

-export([ start/0
        , post_on_source/1
        , target_docs/0
        ]).

start() ->
  RepConfig = #{<<"source">> => <<"source">>,
                <<"target">> => <<"testdb">>},
  Options = [],
  barrel_replicate:start_replication(RepConfig, Options).


post_on_source(Id) ->
  {ok, Conn} = barrel_httpc:connect(<<"http://localhost:7080/dbs/testdb">>),
  IdBin = barrel_lib:to_binary(Id),
  Doc = #{<<"id">> => IdBin, <<"v">> => 42},
  barrel_httpc:post(Conn, Doc, []).

target_docs() ->
  {ok, Conn} = barrel_httpc:connect(<<"http://localhost:7080/dbs/testdb">>),
  Fun = fun
          (Doc, _Meta, Acc1) ->
            {ok, [Doc | Acc1]}
        end,
  {ok, Acc} = barrel_httpc:fold_by_id(Conn, Fun, [], []),
  lists:reverse(Acc).
