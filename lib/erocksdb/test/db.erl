-module(db).

-include_lib("eunit/include/eunit.hrl").


open_test() -> [{open_test_Z(), l} || l <- lists:seq(1, 20)].
open_test_Z() ->
  os:cmd("rm -rf /tmp/erocksdb.open.test"),
  {ok, Ref} = erocksdb:open("/tmp/erocksdb.open.test", [{create_if_missing, true}], []),
  true = erocksdb:is_empty(Ref),
  ok = erocksdb:put(Ref, <<"abc">>, <<"123">>, []),
  false = erocksdb:is_empty(Ref),
  {ok, <<"123">>} = erocksdb:get(Ref, <<"abc">>, []),
  {ok, 1} = erocksdb:count(Ref),
  not_found = erocksdb:get(Ref, <<"def">>, []),
  ok = erocksdb:delete(Ref, <<"abc">>, []),
  not_found = erocksdb:get(Ref, <<"abc">>, []),
  true = erocksdb:is_empty(Ref).

fold_test() -> [{fold_test_Z(), l} || l <- lists:seq(1, 20)].
fold_test_Z() ->
  os:cmd("rm -rf /tmp/erocksdb.fold.test"),
  {ok, Ref} = erocksdb:open("/tmp/erocksdb.fold.test", [{create_if_missing, true}], []),
  ok = erocksdb:put(Ref, <<"def">>, <<"456">>, []),
  ok = erocksdb:put(Ref, <<"abc">>, <<"123">>, []),
  ok = erocksdb:put(Ref, <<"hij">>, <<"789">>, []),
  [{<<"abc">>, <<"123">>},
    {<<"def">>, <<"456">>},
    {<<"hij">>, <<"789">>}] = lists:reverse(erocksdb:fold(Ref,
    fun({K, V}, Acc) ->
      [{K, V} | Acc]
    end,
    [], [])).

fold_keys_test() -> [{fold_keys_test_Z(), l} || l <- lists:seq(1, 20)].
fold_keys_test_Z() ->
  os:cmd("rm -rf /tmp/erocksdb.fold.keys.test"),
  {ok, Ref} = erocksdb:open("/tmp/erocksdb.fold.keys.test", [{create_if_missing, true}], []),
  ok = erocksdb:put(Ref, <<"def">>, <<"456">>, []),
  ok = erocksdb:put(Ref, <<"abc">>, <<"123">>, []),
  ok = erocksdb:put(Ref, <<"hij">>, <<"789">>, []),
  [<<"abc">>, <<"def">>, <<"hij">>] = lists:reverse(erocksdb:fold_keys(Ref,
    fun(K, Acc) -> [K | Acc] end,
    [], [])).

destroy_test() -> [{destroy_test_Z(), l} || l <- lists:seq(1, 20)].
destroy_test_Z() ->
  os:cmd("rm -rf /tmp/erocksdb.destroy.test"),
  {ok, Ref} = erocksdb:open("/tmp/erocksdb.destroy.test", [{create_if_missing, true}], []),
  ok = erocksdb:put(Ref, <<"def">>, <<"456">>, []),
  {ok, <<"456">>} = erocksdb:get(Ref, <<"def">>, []),
  erocksdb:close(Ref),
  ok = erocksdb:destroy("/tmp/erocksdb.destroy.test", []),
  {error, {db_open, _}} = erocksdb:open("/tmp/erocksdb.destroy.test", [{error_if_exists, true}], []).

compression_test() -> [{compression_test_Z(), l} || l <- lists:seq(1, 20)].
compression_test_Z() ->
  CompressibleData = list_to_binary([0 || _X <- lists:seq(1,20)]),
  os:cmd("rm -rf /tmp/erocksdb.compress.0 /tmp/erocksdb.compress.1"),
  {ok, Ref0} = erocksdb:open("/tmp/erocksdb.compress.0", [{create_if_missing, true}],
    [{compression, none}]),
  [ok = erocksdb:put(Ref0, <<I:64/unsigned>>, CompressibleData, [{sync, true}]) ||
    I <- lists:seq(1,10)],
  {ok, Ref1} = erocksdb:open("/tmp/erocksdb.compress.1", [{create_if_missing, true}],
    [{compression, snappy}]),
  [ok = erocksdb:put(Ref1, <<I:64/unsigned>>, CompressibleData, [{sync, true}]) ||
    I <- lists:seq(1,10)],
  %% Check both of the LOG files created to see if the compression option was correctly
  %% passed down
  MatchCompressOption =
    fun(File, Expected) ->
      {ok, Contents} = file:read_file(File),
      case re:run(Contents, "Options.compression: " ++ Expected) of
        {match, _} -> match;
        nomatch -> nomatch
      end
    end,
  Log0Option = MatchCompressOption("/tmp/erocksdb.compress.0/LOG", "0"),
  Log1Option = MatchCompressOption("/tmp/erocksdb.compress.1/LOG", "1"),
  ?assert(Log0Option =:= match andalso Log1Option =:= match).

close_test() -> [{close_test_Z(), l} || l <- lists:seq(1, 20)].
close_test_Z() ->
  os:cmd("rm -rf /tmp/erocksdb.close.test"),
  {ok, Ref} = erocksdb:open("/tmp/erocksdb.close.test", [{create_if_missing, true}], []),
  ?assertEqual(ok, erocksdb:close(Ref)),
  ?assertEqual({error, einval}, erocksdb:close(Ref)).

close_fold_test() -> [{close_fold_test_Z(), l} || l <- lists:seq(1, 20)].
close_fold_test_Z() ->
  os:cmd("rm -rf /tmp/erocksdb.close_fold.test"),
  {ok, Ref} = erocksdb:open("/tmp/erocksdb.close_fold.test", [{create_if_missing, true}], []),
  ok = erocksdb:put(Ref, <<"k">>,<<"v">>,[]),
  ?assertException(throw, {iterator_closed, ok}, % ok is returned by close as the acc
    erocksdb:fold(Ref, fun(_,_A) -> erocksdb:close(Ref) end, undefined, [])).
