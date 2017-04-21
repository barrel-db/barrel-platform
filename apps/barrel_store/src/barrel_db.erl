%% Copyright 2016, Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(barrel_db).
-author("Benoit Chesneau").
-behaviour(gen_server).

%% API
-export([
  infos/1,
  get/3,
  multi_get/5,
  get_doc_info/3,
  get_doc_info_int/3,
  fold_by_id/4,
  changes_since/5,
  changes_since_int/5,
  revsdiff/3,
  put_system_doc/3,
  get_system_doc/2,
  delete_system_doc/2,
  walk/5,
  get_doc1/7,
  update_docs/2
]).

-export([
  start_link/2,
  get_db/1,
  exists/2,
  exists/1,
  encode_rid/1,
  decode_rid/1,
  get_current_revision/1,
  delete_db/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include("barrel_store.hrl").
-include_lib("barrel_common/include/barrel_common.hrl").

%% internal processes
-define(default_timeout, 5000).

-define(IMAX1, 16#ffffFFFFffffFFFF).

-define(BLK_CACHE_SIZE, 8 bsl 30). % 8 GiB

%%%===================================================================
%%% API
%%%===================================================================

exists(_DbId, #{ <<"in_memory">> := true }) -> true;
exists(DbId, _Config) -> filelib:is_dir(db_path(DbId)).

exists(DbId) -> filelib:is_dir(db_path(DbId)).

infos(DbName) ->
  with_db(
    DbName,
    fun(Db) ->
        Info = #{
          name => DbName,
          id => Db#db.id,
          docs_count => Db#db.docs_count,
          last_update_seq => Db#db.updated_seq,
          system_docs_count => Db#db.system_docs_count
         },
        {ok, Info}
    end
  ).

multi_get(DbName, Fun, AccIn, DocIds, Options) ->
  case barrel_store:whereis_db(DbName) of
    undefined -> {error, not_found};
    Db ->
      %% parse options
      WithHistory = proplists:get_value(history, Options, false),
      MaxHistory = proplists:get_value(max_history, Options, ?IMAX1),
      Ancestors = proplists:get_value(ancestors, Options, []),
      %% initialize a snapshot for reads
      {ok, Snapshot} = rocksdb:snapshot(Db#db.store),
      ReadOptions = [{snapshot, Snapshot}],
      Rev = <<"">>,
      GetRevs = fun(DocId, Acc) ->
                    Res = get_doc1(Db, DocId, Rev,
                                   WithHistory, MaxHistory,
                                   Ancestors, ReadOptions),
                    case Res of
                      {ok, Doc, Meta} ->
                        Fun(Doc, Meta, Acc);
                      _ -> Acc
                    end
                end,
      %% finally retrieve the docs
      try
        lists:foldl(GetRevs, AccIn, DocIds)
      after rocksdb:release_snapshot(Snapshot)
      end
  end.


%% TODO: handle attachment
get(DbName, DocId, Options) ->
  case barrel_store:whereis_db(DbName) of
    undefined -> {error, not_found};
    Db ->
      %% parse options
      Rev = proplists:get_value(rev, Options, <<"">>),
      WithHistory = proplists:get_value(history, Options, false),
      MaxHistory = proplists:get_value(max_history, Options, ?IMAX1),
      Ancestors = proplists:get_value(ancestors, Options, []),
      get_doc1(Db, DocId, Rev, WithHistory, MaxHistory, Ancestors, [])
  end.

get_doc1(Db, DocId, Rev, WithHistory, MaxHistory, Ancestors, ReadOptions) ->
  case get_doc_info_int(Db, DocId, ReadOptions) of
    {ok, #{deleted := true}} when Rev =:= <<"">> ->
      {error, not_found};
    {ok, DocInfo = #{ revtree := RevTree }} ->
      case maybe_get_revision(Rev, DocInfo, ReadOptions, Db) of
        {ok, Body, Meta} ->
          case WithHistory of
            false ->
              {ok, Body, Meta};
            true ->
              RevId = maps:get(<<"rev">>, Meta),
              History = barrel_revtree:history(RevId, RevTree),
              EncodedRevs = barrel_doc:encode_revisions(History),
              Revisions = barrel_doc:trim_history(EncodedRevs, Ancestors, MaxHistory),
              {ok, Body, Meta#{<<"revisions">> => Revisions}}
          end;
        Error ->
          Error

      end;
    Error ->
      Error
  end.

maybe_get_revision(<<>>, #{ deleted := true}, _ReadOptions, _Db) ->
  {error, not_found};
maybe_get_revision(Rev, DocInfo, ReadOptions, Db) ->
  RevId = case Rev of
            <<"">> -> maps:get(current_rev, DocInfo);
            _ -> Rev
          end,
  get_revision(RevId, DocInfo, ReadOptions, Db).

get_body(RevId, #{ body_map := BodyMap}) ->
  maps:find(RevId, BodyMap).

get_revision(RevId, DocInfo, ReadOptions, Db) ->
  case get_body(RevId, DocInfo) of
    {ok, Body} ->
      {ok, Body, meta(RevId, DocInfo)};
    error ->
      DocId = maps:get(id, DocInfo),
      case get_persisted_rev(Db, DocId, RevId, ReadOptions) of
        {ok, Body} ->
          {ok, Body, meta(RevId, DocInfo)};
        Error ->
          Error
      end
  end.

meta(RevId, DocInfo) ->
  #{rid := Rid, revtree := RevTree} = DocInfo,
  {ok, RevInfo} = barrel_revtree:info(RevId, RevTree),
  Deleted = maps:get(deleted, RevInfo, false),
  maybe_add_deleted_meta(
    #{<<"rid">> => encode_rid(Rid),
      <<"rev">> => RevId },
    Deleted
  ).

maybe_add_deleted_meta(Meta, false) -> Meta;
maybe_add_deleted_meta(Meta, Deleted) -> Meta#{ <<"deleted">> => Deleted}.


get_persisted_rev(#db{store=Store}, DocId, RevId, ReadOptions) ->
  case rocksdb:get(Store, barrel_keys:rev_key(DocId, RevId), ReadOptions) of
    {ok, Bin} -> {ok, binary_to_term(Bin)};
    not_found -> {error, not_found};
    Error -> Error
  end.

get_doc_info(DbName, DocId, ReadOptions) when is_binary(DbName) ->
  case barrel_store:whereis_db(DbName) of
    undefined -> {error, not_found};
    Db -> get_doc_info_int(Db, DocId, ReadOptions)
  end.

get_doc_info_int(#db{store=Store}, DocId, ReadOptions) ->
  case rocksdb:get(Store, barrel_keys:doc_key(DocId), ReadOptions) of
    {ok, << RID:64 >>} ->
      case rocksdb:get(Store, barrel_keys:res_key(RID), ReadOptions) of
        {ok, Bin} -> {ok, binary_to_term(Bin)};
        not_found -> {error, not_found}
      end;
    not_found ->
      {error, not_found}
  end.

update_docs(DbName, Batch) ->
  case barrel_store:whereis_db(DbName) of
    undefined -> {error, found};
    Db = #db{pid=DbPid} ->
      ok = hooks:run(db_start_update_docs, [DbName]),
      {DocBuckets, Ref, Async, N} = barrel_write_batch:to_buckets(Batch),
      MRef = erlang:monitor(process, DbPid),
      DbPid ! {update_docs, DocBuckets},
      Res = case Async of
              false ->
                collect_updates(Db, Ref, MRef, [], N);
              true ->
                ok
            end,
      ok = hooks:run(db_end_update_docs, [DbName]),
      Res
  end.

collect_updates(Db = #db{pid=DbPid}, Ref, MRef, Results, N) when N > 0 ->
  receive
    {result, Ref, DbPid, Idx, Result} ->
      collect_updates(Db, Ref, MRef, [{Idx, Result} | Results], N - 1);
    {'DOWN', MRef, _, _, Reason} ->
      exit(Reason)
  end;
collect_updates(_Db, _Ref, MRef, Results, 0) ->
  erlang:demonitor(MRef, [flush]),
  %% we return results  ordered by operation index
  lists:map(
    fun({_, Result}) -> Result end,
    lists:keysort(1, Results)
  ).


fold_by_id(DbName, Fun, Acc, Opts) ->
  case barrel_store:whereis_db(DbName) of
    undefined -> {error, not_found};
    Db -> fold_by_id_int(Db, Fun, Acc, Opts)
  end.

fold_by_id_int(#db{ store=Store }, UserFun, AccIn, Opts) ->
  Prefix = barrel_keys:prefix(doc),
  {ok, Snapshot} = rocksdb:snapshot(Store),
  ReadOptions = [{snapshot, Snapshot}],
  Opts2 = [{read_options, ReadOptions} | Opts],

  WrapperFun =
  fun(_Key, << RID:64 >>, Acc) ->
    %% TODO: optimize it ?
    {ok, Bin} = rocksdb:get(Store, barrel_keys:res_key(RID), ReadOptions) ,
    DocInfo =  binary_to_term(Bin),
    case get_current_revision(DocInfo) of
      {ok, Doc, Meta} ->
        UserFun(Doc, Meta, Acc);
      error ->
        Acc
    end
  end,

  try barrel_rocksdb:fold_prefix(Store, Prefix, WrapperFun, AccIn, Opts2)
  after rocksdb:release_snapshot(Snapshot)
  end.

changes_since(DbName, Since, Fun, AccIn, Opts) when is_binary(DbName), is_integer(Since) ->
  case barrel_store:whereis_db(DbName) of
    undefined -> {error, not_found};
    Db ->  changes_since_int(Db, Since, Fun, AccIn, Opts)
  end.

changes_since_int(Db = #db{ store=Store}, Since0, Fun, AccIn, Opts) ->
  Since = if
            Since0 > 0 -> Since0 + 1;
            true -> Since0
          end,
  Prefix = barrel_keys:prefix(seq),
  {ok, Snapshot} = rocksdb:snapshot(Store),
  ReadOptions = [{snapshot, Snapshot}],
  FoldOpts = [
    {start_key, <<Since:32>>},
    {read_options, ReadOptions}
  ],
  IncludeDoc = proplists:get_value(include_doc, Opts, false),
  WithHistory = proplists:get_value(history, Opts, last) =:= all,

  WrapperFun =
  fun(Key, BinDocInfo, Acc) ->
    DocInfo = binary_to_term(BinDocInfo),
    [_, SeqBin] = binary:split(Key, Prefix),
    <<Seq:32>> = SeqBin,
    RevId = maps:get(current_rev, DocInfo),
    Deleted = maps:get(deleted, DocInfo),
    DocId = maps:get(id, DocInfo),
    Rid = maps:get(rid, DocInfo),
    RevTree = maps:get(revtree, DocInfo),
    Changes = case WithHistory of
                false -> [RevId];
                true -> barrel_revtree:history(RevId, RevTree)
              end,

    %% create change
    Change = change_with_doc(
      changes_with_deleted(
        #{ <<"id">> => DocId, <<"seq">> => Seq,
           <<"rev">> => RevId, <<"changes">> => Changes,
           <<"rid">> => encode_rid(Rid) },
        Deleted
      ),
      Rid, Db, ReadOptions, IncludeDoc
    ),
    Fun(Change, Acc)
  end,

  try barrel_rocksdb:fold_prefix(Store, Prefix, WrapperFun, AccIn, FoldOpts)
  after rocksdb:release_snapshot(Snapshot)
  end.

change_with_doc(Change, Rid, #db{store=Store}, ReadOptions, true) ->
  case rocksdb:get(Store, barrel_keys:res_key(Rid), ReadOptions) of
    {ok, Bin} ->
      DocInfo = binary_to_term(Bin),
      case get_current_revision(DocInfo) of
        {ok, Doc, _Meta} -> Change#{ <<"doc">> => Doc };
        error ->
          Change#{ <<"doc">> => #{ <<"error">> => <<"missing">> } }
      end;
    not_found ->
      Change#{ <<"doc">> => #{ <<"error">> => <<"missing">> } }
  end;
change_with_doc(Change, _Rid, _Db, _ReadOptions, false) ->
  Change.

changes_with_deleted(Change, true) -> Change#{<<"deleted">> => true};
changes_with_deleted(Change, _) -> Change.

get_current_revision(DocInfo) ->
  RevId = maps:get(current_rev, DocInfo),
  case get_body(RevId, DocInfo) of
    {ok, Body} ->
      {ok, Body, meta(RevId, DocInfo)};
    error ->
      error
  end.

revsdiff(DbName, DocId, RevIds) ->
  case get_doc_info(DbName, DocId, []) of
    {ok, #{revtree := RevTree}} -> revsdiff1(RevTree, RevIds);
    {error, not_found} -> {ok, RevIds, []};
    Error -> Error
  end.

revsdiff1(RevTree, RevIds) ->
  {Missing, PossibleAncestors} = lists:foldl(
    fun(RevId, {M, A} = Acc) ->
      case barrel_revtree:contains(RevId, RevTree) of
        true -> Acc;
        false ->
          M2 = [RevId | M],
          {Gen, _} = barrel_doc:parse_revision(RevId),
          A2 = barrel_revtree:fold_leafs(
            fun(#{ id := Id}=RevInfo, A1) ->
              Parent = maps:get(parent, RevInfo, <<"">>),
              case lists:member(Id, RevIds) of
                true ->
                  {PGen, _} = barrel_doc:parse_revision(Id),
                  if
                    PGen < Gen -> [Id | A1];
                    PGen =:= Gen, Parent =/= <<"">> -> [Parent | A1];
                    true -> A1
                  end;
                false -> A1
              end
            end, A, RevTree),
          {M2, A2}
      end
    end, {[], []}, RevIds),
  {ok, lists:reverse(Missing), lists:usort(PossibleAncestors)}.


put_system_doc(DbName, DocId, Doc) ->
  with_db(
    DbName,
    fun(#db{pid=Pid}) ->
      EncKey = barrel_keys:sys_key(DocId),
      EncVal = term_to_binary(Doc),
      gen_server:call(Pid, {put, EncKey, EncVal})
    end
  ).

get_system_doc(DbName, DocId) ->
  with_db(
    DbName,
    fun(#db{store=Store}) ->
      EncKey = barrel_keys:sys_key(DocId),
      case rocksdb:get(Store, EncKey, []) of
        {ok, Bin} -> {ok, binary_to_term(Bin)};
        not_found -> {error, not_found};
        Error -> Error
      end
    end
  ).

delete_system_doc(DbName, DocId) ->
  with_db(
    DbName,
    fun(#db{pid=Pid}) ->
      EncKey = barrel_keys:sys_key(DocId),
      gen_server:call(Pid, {delete, EncKey})
    end
  ).

walk(DbName, Path, Fun, AccIn, Options) ->
  with_db(
    DbName,
    fun(Db) ->
      barrel_walk:walk(Db, Path, Fun, AccIn, Options)
    end
  ).

with_db(DbName, Fun) ->
  case barrel_store:whereis_db(DbName) of
    undefined ->
      _ = lager:debug(
        "~s: db ~p not found",
        [?MODULE_STRING, DbName]
      ),
      {error, not_found};
    Db ->
      Fun(Db)
  end.

start_link(DbId, Config) ->
  gen_server:start_link(?MODULE, [DbId, Config], []).

get_db(DbPid) when is_pid(DbPid) ->
  gen_server:call(DbPid, get_db).

delete_db(DbPid) ->
  try gen_server:call(DbPid, delete_db)
  catch
    exit:{noproc,_} -> ok;
    exit:noproc -> ok;
    %% Handle the case where the monitor triggers
    exit:{normal, _} -> ok
  end.


db_dir() ->
  Dir = filename:join(barrel_store:data_dir(), "dbs"),
  ok = filelib:ensure_dir([Dir, "dummy"]),
  Dir.

db_path(DbId) ->
  Path = binary_to_list(filename:join(db_dir(), DbId)),
  ok = filelib:ensure_dir(Path),
  Path.

encode_rid(Rid) -> base64:encode(<< Rid:64 >>).

decode_rid(Bin) ->
  << Rid:64 >> = base64:decode(Bin),
  Rid.


%% TODO: put dbinfo in a template
init([DbId, Config]) ->
  process_flag(trap_exit, true),
  {ok, Store} = open_db(DbId, Config),
  #{<<"last_rid">> := LastRid,
    <<"updated_seq">> := Updated,
    <<"docs_count">> := DocsCount,
    <<"system_docs_count">> := SystemDocsCount}  = init_meta(Store),

  _ = init_properties(),

  Db =
    #db{id=DbId,
        store=Store,
        pid=self(),
        conf = Config,
        last_rid = LastRid,
        updated_seq = Updated,
        docs_count = DocsCount,
        system_docs_count = SystemDocsCount},
  {ok, Db}.

%% TODO: use a specific column to store the counters
init_meta(Store) ->
  Prefix = barrel_keys:prefix(db_meta),
  barrel_rocksdb:fold_prefix(
    Store,
    Prefix,
    fun(<< _:3/binary, Key/binary >>, ValBin, Meta) ->
      Val = binary_to_term(ValBin),
      {ok, Meta#{ Key => Val }}
    end,
    #{<<"last_rid">> => 0,
      <<"updated_seq">> => 0,
      <<"docs_count">> => 0,
      <<"system_docs_count">> => 0},
    []
  ).

init_properties() ->
  Props = [{num_docs_updated, 0}],

  lists:foreach(fun({K, V}) ->
                    erlang:put(K, V)
                end, Props).

open_db(DbId, Config) ->
  Path = db_path(DbId),
  InMemory = maps:get(<<"in_memory">>, Config, false),
  DbOpts = case InMemory of
             true ->
               [{create_if_missing, true}, {in_memory, true} | default_rocksdb_options()];
             false ->
               [{create_if_missing, true} | default_rocksdb_options()]
           end,
  rocksdb:open(Path, DbOpts).

default_rocksdb_options() ->
  BlockCacheSize = case application:get_env(barrel_store, block_cache_size, 0) of
                     0 ->
                       MaxSize = barrel_memory_monitor:get_total_memory(),
                       %% reserve 1GB for system and binaries, and use 30% of the rest
                       ((MaxSize - 1024 * 1024) * 0.3);
                     Sz ->
                       Sz * 1024 * 1024 * 1024
                   end,

  [{max_open_files, 64},
   {write_buffer_size, 64 * 1024 * 1024}, %% 64MB
   {allow_concurrent_memtable_write, true},
   {enable_write_thread_adaptive_yield, true},
   {table_factory_block_cache_size, BlockCacheSize}
  ].

handle_call({put, K, V}, _From, Db) ->
  Reply = (catch do_put(K, V, Db)),
  {reply, Reply, Db};
handle_call({delete, K}, _From, Db) ->
  Reply = (catch do_delete(K, Db)),
  {reply, Reply, Db};
handle_call(get_db, _From, Db) ->
  {reply, {ok, Db}, Db};

handle_call(delete_db, _From, Db = #db{ id = Id, store = Store }) ->
  ok = (catch rocksdb:close(Store)),
  ok = delete_db_dir(Id),
  {stop, normal, ok, Db#db{ store=nil}};

handle_call(_Request, _From, State) ->
  {reply, {error, bad_call}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({update_docs, DocBuckets}, Db) ->
  NewDb = do_update_docs(DocBuckets, Db),
  {noreply, NewDb};

handle_info(_Info, State) -> {noreply, State}.


terminate(Reason, #db{ id = Id, store = nil }) ->
  _ = lager:info("terminate db ~p: ~p~n", [Id, Reason]),
  ok;

terminate(Reason, #db{ id = Id, store = Store }) ->
  %% finally close the database and return its result
  ok = close_store(Id, Store),
  _ = lager:info("terminate db ~p: ~p~n", [Id, Reason]),
  ok.

close_store(Id, Store) ->
  Result = (catch rocksdb:close(Store)),
  _ = lager:info("~s: ~p closed: ~p",[?MODULE_STRING, Id, Result]),
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


delete_db_dir(Id) ->
  TempName = db_path(barrel_lib:uniqid()),
  case file:rename(db_path(Id), TempName) of
    ok ->
      %% deletion of the database happen asynchronously
      spawn(
        fun() ->
            ok = rocksdb:destroy(TempName, []),
            _ = lager:info("~p: old db files deleleted  in ~p~n", [Id, TempName])
        end
       ),
      ok;
    _ ->
      ok
  end.

empty_doc_info(DocId, Rid) ->
  #{ id => DocId,
     rid => Rid,
     local_seq => 0,
     current_rev => <<>>,
     revtree => #{},
     body_map => #{}
  }.

send_result({Client, Ref, Idx, false}, Result) ->
  Client ! {result, Ref, self(), Idx, Result};
send_result(_, _) ->
  ok.


do_update_docs(DocBuckets, Db =  #db{id=DbId, store=Store, last_rid=LastRid }) ->
  %% try to collect a maximum of updates at once
  DocBuckets2 = collect_updates(DbId, DocBuckets),
  erlang:put(num_docs_updated, maps:size(DocBuckets2)),

  {Updates, NewRid, _, OldDocs} = merge_revtrees(DocBuckets2, Db),

  %% update resource counter
  if
    NewRid /= LastRid ->
      ok = rocksdb:put(
        Store,
        barrel_keys:db_meta_key(<<"last_rid">>),
        term_to_binary(NewRid),
        []
      );
    true -> ok
  end,

  lists:foldl(
    fun
      ({#{ local_seq := 0}, []}, Db1) ->
        %% edge case, an update happened on a none existing doc
        %% it should be safe there to return the db since the not_found
        %% error has already be sent back to the requesters
        Db1;
      ({DocInfo, Reqs}, Db1) ->
        #{ id := DocId, rid := Rid, current_rev := WinningRev} = DocInfo,
        LastSeq = maps:get(update_seq, DocInfo, -1),

        %% increment local document seq
        DocInfo2 = DocInfo#{update_seq => Db1#db.updated_seq + 1},

        %% doc counter increment
        Inc = case DocInfo2 of #{ deleted := true } -> -1; _ -> 1 end,

        %% update db object
        Db2 = Db1#db{updated_seq = Db1#db.updated_seq + 1,
                     docs_count = Db1#db.docs_count + Inc},

        %% revision has changed put the ancestor outside the value
        {DocInfo3, Ancestor} = backup_ancestor(DocInfo2),


        %% Create the changes index metadata
        SeqMeta = maps:remove(body_map, DocInfo3),



        %% create update index events.
        %% TODO: move that code in a cleaner place
        NewDoc = current_body(DocInfo3),
        OldDoc = maps:get(DocId, OldDocs),
        {Added, Removed} = barrel_index:diff(NewDoc, OldDoc),
        Batch0 = update_index(Added, Rid, Db2#db.updated_seq, index,
                              update_index(Removed, Rid, Db2#db.updated_seq, unindex, [])),

        %% finally write the batch
        Batch =
          maybe_update_changes(
            LastSeq,
            maybe_backup_ancestor(
              Ancestor,
              maybe_link_rid(
                DocInfo3,
                [
                  {put, barrel_keys:res_key(Rid), term_to_binary(DocInfo3)},
                  {put, barrel_keys:seq_key(Db2#db.updated_seq), term_to_binary(SeqMeta)},
                  {put, barrel_keys:db_meta_key(<<"docs_count">>), term_to_binary(Db2#db.docs_count)},
                  {put, barrel_keys:db_meta_key(<<"updated_seq">>), term_to_binary(Db2#db.updated_seq)}
                ]
              )
            )
          ) ++ Batch0,


        ResWrite =  rocksdb:write(Store, Batch, [{sync, true}]),

        case ResWrite of
          ok ->
            lists:foreach(
              fun(Req) -> send_result(Req, {ok, DocId, WinningRev}) end,
              Reqs
            ),
            ets:insert(barrel_dbs, Db2),
            barrel_db_event:notify(Db2#db.id, db_updated),
            Db2;
          Error ->
            _ = lager:error(
              "~s: error writing ~p: ~p",
              [?MODULE_STRING, DocId, Error]
            ),

            lists:foreach(
              fun(Req) -> send_result(Req, Error) end,
              Reqs
            ),
            Db2
        end
      end,
    Db#db{last_rid=NewRid},
    Updates
  ).

update_index([Path | Rest], Rid, Seq, index, Batch0) ->
  Batch1 = lists:foldl(fun(P, Acc) ->
                          [ {put, barrel_keys:forward_path_key(P, Rid), <<>>},
                            {put, barrel_keys:reverse_path_key(P, Rid), <<>>} | Acc ]
                      end, Batch0, barrel_index:split_path(Path)),
  update_index(Rest, Rid, Seq, index, Batch1);
update_index([Path | Rest], Rid, Seq, unindex, Batch0) ->
  Batch1 = lists:foldl(fun(P, Acc) ->
                          [ {delete, barrel_keys:forward_path_key(P, Rid)},
                            {delete, barrel_keys:reverse_path_key(P, Rid)} | Acc ]
                      end, Batch0, barrel_index:split_path(Path)),
  update_index(Rest, Rid, Seq, unindex, Batch1);
update_index([], _Rid, _Seq, _Op, Batch) ->
  Batch.

backup_ancestor(DocInfo) ->
  #{ id := Id, current_rev := Rev, revtree := Tree, body_map := BodyMap} = DocInfo,
  case barrel_revtree:parent(Rev, Tree) of
    <<"">> -> {DocInfo, nil};
    Parent ->
      case maps:take(Parent, BodyMap) of
        {Body, BodyMap2} ->
          {DocInfo#{ body_map => BodyMap2}, {Id, Parent, Body}};
        error ->
          {DocInfo, nil}
      end
  end.

maybe_update_changes(undefined, Batch) -> Batch;
maybe_update_changes(LastSeq, Batch) ->
  Batch ++  [{delete, barrel_keys:seq_key(LastSeq)}].

maybe_backup_ancestor(nil, Batch) -> Batch;
maybe_backup_ancestor({DocId, RevId, Body}, Batch) ->
  Batch ++ [{put, barrel_keys:rev_key(DocId, RevId), term_to_binary(Body)}].

maybe_link_rid(#{ id := DocId, rid := Rid, local_seq := 1}, Batch) ->
  Batch ++ [{put, barrel_keys:doc_key(DocId), << Rid:64 >>}];
maybe_link_rid(_DI, Batch) ->
  Batch.

current_body(#{ current_rev := Rev, body_map := BodyMap }) -> maps:get(Rev, BodyMap).

%% TODO: cache doc infos
merge_revtrees(DocBuckets, Db = #db{last_rid=LastRid}) ->
  maps:fold(
    fun(DocId, Bucket, {Updates, Rid, DocInfos, OldDocs}) ->
      {DocInfo, Rid2, DocInfos2, OldDocs2} = case maps:find(DocId, DocInfos) of
                  {ok, DI} -> {DI, Rid, DocInfos, OldDocs};
                  error ->
                    case get_doc_info_int(Db, DocId, []) of
                      {ok,  DI} ->
                        OldDoc = current_body(DI),
                        {DI, Rid, DocInfos#{ DocId => DI}, OldDocs#{ DocId => OldDoc }};
                      {error, not_found} ->
                        DI = empty_doc_info(DocId, Rid +1),
                        {DI, Rid +1, DocInfos#{ DocId => DI}, OldDocs#{ DocId => #{} } }
                    end
                end,

      Update = lists:foldl(
        fun({Doc, WithConflict, CreateIfMissing, ErrorIfExists, Req}, {DI1, Reqs1}) ->
          #{ local_seq := Seq } = DI1,
          case WithConflict of
            true ->
              {ok, DI2} = merge_revtree_with_conflict(Doc, DI1),
              {DI2, [Req | Reqs1]};
            false when CreateIfMissing =/= true, Seq =:= 0 ->
              _ = send_result(Req, {error, not_found}),
              {DI1, Reqs1};
            false ->
              case merge_revtree(Doc, DI1, ErrorIfExists) of
                {ok, DI2} ->
                  {DI2, [Req | Reqs1]};
                Conflict ->
                  _ = send_result(Req, {error, Conflict}),
                  {DI1, Reqs1}
              end
          end
        end,
        {DocInfo, []},
        Bucket
      ),
      {[Update | Updates], Rid2, DocInfos2, OldDocs2}
    end,
    {[], LastRid, #{}, #{}},
    DocBuckets
  ).

merge_revtree(Doc = #doc{ revs = [Rev|_]}, DocInfo, ErrorIfExists) ->
  #{ local_seq := Seq, current_rev := CurrentRev, revtree := RevTree, body_map := BodyMap } = DocInfo,
  {Gen, _}  = barrel_doc:parse_revision(Rev),
  Res = case Rev of
          <<>> ->
            if
              CurrentRev /= <<>>, ErrorIfExists =:= true ->
                case maps:get(CurrentRev, RevTree) of
                  #{ deleted := true} ->
                    {CurrentGen, _} = barrel_doc:parse_revision(CurrentRev),
                    {ok, CurrentGen + 1, CurrentRev};
                  _ ->
                    {conflict, doc_exists}
                end;
              CurrentRev /= <<>> ->
                {CurrentGen, _} = barrel_doc:parse_revision(CurrentRev),
                {ok, CurrentGen + 1, CurrentRev};
              true ->
                {ok, Gen + 1, <<>>}
            end;
          _ ->
            case barrel_revtree:is_leaf(Rev, RevTree) of
              true -> {ok, Gen + 1, Rev};
              false -> {conflict, revision_conflict}
            end
        end,
  case Res of
    {ok, NewGen, ParentRev} ->
      NewRev = barrel_doc:revid(NewGen, Rev, Doc),
      RevInfo = #{  id => NewRev,  parent => ParentRev, deleted => Doc#doc.deleted },
      RevTree2 = barrel_revtree:add(RevInfo, RevTree),

      %% find winning revision and update doc infos with it
      {WinningRev, Branched, Conflict} = barrel_revtree:winning_revision(RevTree2),
      WinningRevInfo = maps:get(WinningRev, RevTree2),

      %% update docinfo
      DocInfo2 = DocInfo#{ body_map => BodyMap#{ NewRev => Doc#doc.body },
                           revtree => RevTree2,
                           local_seq => Seq + 1,
                           current_rev => WinningRev,
                           branched => Branched,
                           conflict => Conflict,
                           deleted => barrel_revtree:is_deleted(WinningRevInfo)},
      {ok, DocInfo2};
    Conflict ->
      Conflict
  end.


merge_revtree_with_conflict(Doc = #doc{revs=[NewRev |_]=Revs, body=Body}, DocInfo) ->
  #{current_rev := CurrentRev, local_seq := Seq, revtree := RevTree, body_map := BodyMap} = DocInfo,
  {OldPos, _}  = barrel_doc:parse_revision(CurrentRev),
  {Idx, Parent} = find_parent(Revs, RevTree, 0),
  if
    Idx =:= 0 ->
      {ok, DocInfo};
    true ->
      ToAdd = lists:sublist(Revs, Idx),
      RevTree2 = edit_revtree(lists:reverse(ToAdd), Parent, Doc#doc.deleted, RevTree),

      %% update docinfo
      DocInfo2 = DocInfo#{ local_seq := Seq + 1,
                           body_map => BodyMap#{ NewRev => Body },
                           revtree => RevTree2},

      %% find winning revision and update doc infos with it
      {WinningRev, Branched, Conflict} = barrel_revtree:winning_revision(RevTree2),
      {NewPos, _}  = barrel_doc:parse_revision(WinningRev),

      %% if the new winning revision is at the same position we keep the current
      %% one as winner. Else we update the doc info.
      if
         NewPos /= OldPos ->
           WinningRevInfo = maps:get(WinningRev, RevTree2),
           {ok, DocInfo2#{current_rev => WinningRev,
                          branched => Branched,
                          conflict => Conflict,
                          deleted => barrel_revtree:is_deleted(WinningRevInfo)}};
        true ->
          {ok, DocInfo2}
      end
  end.

edit_revtree([RevId], Parent, Deleted, Tree) ->
  case Deleted of
    true ->
      barrel_revtree:add(#{ id => RevId, parent => Parent, deleted => true}, Tree);
    false ->
      barrel_revtree:add(#{ id => RevId, parent => Parent}, Tree)
  end;
edit_revtree([RevId | Rest], Parent, Deleted, Tree) ->
  Tree2 = barrel_revtree:add(#{ id => RevId, parent => Parent}, Tree),
  edit_revtree(Rest, RevId, Deleted, Tree2);
edit_revtree([], _Parent, _Deleted, Tree) ->
  Tree.

find_parent([RevId | Rest], RevTree, I) ->
  case barrel_revtree:contains(RevId, RevTree) of
    true -> {I, RevId};
    false -> find_parent(Rest, RevTree, I+1)
  end;
find_parent([], _RevTree, I) ->
  {I, <<"">>}.

merge_updates(DocBuckets1, DocBuckets2) ->
  maps:fold(
    fun(Key, Bucket, M) ->
      case maps:find(Key, M) of
        {ok, OldBucket} -> M#{Key => OldBucket ++ Bucket};
        error -> M#{ Key => Bucket }
      end
    end,
    DocBuckets1,
    DocBuckets2
  ).

collect_updates(DbId, DocBuckets0) ->
  receive
    {update_docs, DocBuckets1} ->
      DocBuckets2 = merge_updates(DocBuckets0, DocBuckets1),
      collect_updates(DbId, DocBuckets2)
  after 0 ->
    DocBuckets0
  end.

do_put(K, V, #db{ id=DbId, store=Store, system_docs_count = Count}) ->
  Batch = [
    {put, K, V},
    {put, barrel_keys:db_meta_key(<<"system_docs_count">>), term_to_binary(Count + 1)}
  ],
  case rocksdb:write(Store, Batch, [{sync, true}]) of
    ok ->
      ets:update_counter(barrel_dbs, DbId, {#db.system_docs_count, 1}),
      ok;
    Error ->
      Error
  end.

do_delete(K, #db{ id=DbId, store=Store, system_docs_count = Count}) ->
  Batch = [
    {delete, K},
    {put, barrel_keys:db_meta_key(<<"system_docs_count">>), term_to_binary(Count - 1)}
  ],
  case rocksdb:write(Store, Batch, [{sync, true}]) of
    ok ->
      ets:update_counter(barrel_dbs, DbId, {#db.system_docs_count, -1}),
      ok;
    Error ->
      Error
  end.
