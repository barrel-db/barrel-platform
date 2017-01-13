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
  put/3,
  put_rev/4,
  get/3,
  get_doc_info/3,
  get_doc_info_int/3,
  delete/4,
  post/3,
  fold_by_id/4,
  changes_since/5,
  changes_since_int/5,
  revsdiff/3,
  write_system_doc/3,
  read_system_doc/2,
  delete_system_doc/2,
  query/5,
  query/6,
  get_doc1/7
]).

-export([
  start_link/3,
  get_db/1
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

-include("barrel.hrl").

%% internal processes
-define(default_timeout, 5000).

-define(IMAX1, 16#ffffFFFFffffFFFF).

%%%===================================================================
%%% API
%%%===================================================================

infos(DbName) ->
  with_db(
    DbName,
    fun(Db) ->
      #{
        name => DbName,
        id => Db#db.id,
        docs_count => Db#db.docs_count,
        last_update_seq => Db#db.updated_seq,
        system_docs_count => Db#db.system_docs_count,
        last_index_seq => Db#db.indexed_seq
      }
    end
  ).

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
      %% initialize a snapshot for reads
      {ok, Snapshot} = rocksdb:snapshot(Db#db.store),
      ReadOptions = [{snapshot, Snapshot}],
      %% finally retieve the doc
      try get_doc1(Db, DocId, Rev, WithHistory, MaxHistory, Ancestors, ReadOptions)
      after rocksdb:release_snapshot(Snapshot)
      end
      
  end.

get_doc1(Db, DocId, Rev, WithHistory, MaxHistory, Ancestors, ReadOptions) ->
  case get_doc_info_int(Db, DocId, ReadOptions) of
    {ok, #{revtree := RevTree} = DocInfo} ->
      RevId = case Rev of
                <<"">> -> maps:get(current_rev, DocInfo);
                UserRev -> UserRev
              end,
      case get_doc_rev(Db, DocId, RevId, ReadOptions) of
        {ok, #{ <<"_deleted">> := true }} when Rev =:= <<"">> ->
          {error, not_found};
        {ok, Doc} ->
          case WithHistory of
            true ->
              History = barrel_revtree:history(RevId, RevTree),
              EncodedRevs = barrel_doc:encode_revisions(History),
              Revisions = barrel_doc:trim_history(EncodedRevs, Ancestors, MaxHistory),
              {ok, Doc#{<<"_revisions">> => Revisions}};
            false ->
              {ok, Doc}
          end;
        Error -> Error
      end;
    Error ->  Error
  end.

get_doc_rev(#db{id=DbId, store=Store}, DocId, RevId, ReadOptions) ->
  case rocksdb:get(Store, barrel_keys:rev_key(DbId, DocId, RevId), ReadOptions) of
    {ok, Bin} -> {ok, binary_to_term(Bin)};
    not_found -> {error, not_found};
    Error -> Error
  end.


get_doc_info(DbName, DocId, ReadOptions) when is_binary(DbName) ->
  case barrel_store:whereis_db(DbName) of
    undefined -> {error, not_found};
    Db -> get_doc_info_int(Db, DocId, ReadOptions)
  end.

get_doc_info_int(#db{id=DbId, store=Store}, DocId, ReadOptions) ->
  DocKey = barrel_keys:doc_key(DbId, DocId),
  case rocksdb:get(Store, DocKey, ReadOptions) of
    {ok, BinDocInfo} -> {ok, binary_to_term(BinDocInfo)};
    not_found -> {error, not_found}
  end.



put(DbName, Doc, _Options) when is_map(Doc) ->
  DocId = get_id(Doc),
  Rev = barrel_doc:rev(Doc),
  {Gen, _} = barrel_doc:parse_revision(Rev),
  Deleted = barrel_doc:deleted(Doc),
  update_doc(
    DbName,
    DocId,
    fun(DocInfo) ->
      #{ current_rev := CurrentRev, revtree := RevTree } = DocInfo,
      Res = case Rev of
              <<>> ->
                if
                  CurrentRev /= <<>> ->
                    case maps:get(CurrentRev, RevTree) of
                      #{ deleted := true} ->
                        {CurrentGen, _} = barrel_doc:parse_revision(CurrentRev),
                        {ok, CurrentGen + 1, CurrentRev};
                      _ ->
                        {conflict, doc_exists}
                    end;
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
          RevInfo = #{  id => NewRev,  parent => ParentRev,  deleted => Deleted},
          RevTree2 = barrel_revtree:add(RevInfo, RevTree),
          Doc2 = Doc#{<<"_rev">> => NewRev},
          %% update the doc infos
          {WinningRev, Branched, Conflict} = barrel_revtree:winning_revision(RevTree2),
          DocInfo2 = DocInfo#{
            id => DocId,
            current_rev => WinningRev,
            branched => Branched,
            conflict => Conflict,
            revtree => RevTree2
          },
          {ok, DocInfo2, Doc2, NewRev};
        Conflict ->
          Conflict
      end
    end);
put(_, _, _) ->
  erlang:error(badarg).

put_rev(DbName, Doc, History, _Options) when is_map(Doc) ->
  DocId = get_id(Doc),
  [NewRev |_] = History,
  Deleted = barrel_doc:deleted(Doc),
  update_doc(
    DbName,
    DocId,
    fun(DocInfo) ->
      #{revtree := RevTree} = DocInfo,
      {Idx, Parent} = find_parent(History, RevTree, 0),
      if
        Idx =:= 0 -> ok;
        true ->
          ToAdd = lists:sublist(History, Idx),
          RevTree2 = edit_revtree(ToAdd, Parent, Deleted, RevTree),
          {WinningRev, Branched, Conflict} = barrel_revtree:winning_revision(RevTree2),
          DocInfo2 = DocInfo#{
            id => DocId,
            current_rev => WinningRev,
            branched => Branched,
            conflict => Conflict,
            revtree => RevTree2
          },
          Doc2 = Doc#{ <<"_rev">> => NewRev },
          {ok, DocInfo2, Doc2, NewRev}
      end
    end);
put_rev(_, _, _, _) ->
  erlang:error(badarg).

edit_revtree([RevId], Parent, Deleted, Tree) ->
  case Deleted of
    true ->
      barrel_revtree:add(#{ id => RevId, parent => Parent, deleted => true}, Tree);
    false ->
      barrel_revtree:add(#{ id => RevId, parent => Parent}, Tree)
  end;
edit_revtree([RevId | Rest], Parent, Deleted, Tree) ->
  Tree2 = barrel_revtree:add(#{ id => RevId, parent => Parent}, Tree),
  edit_revtree(Rest, Parent, Deleted, Tree2);
edit_revtree([], _Parent, _Deleted, Tree) ->
  Tree.

find_parent([RevId | Rest], RevTree, I) ->
  case barrel_revtree:contains(RevId, RevTree) of
    true -> {I, RevId};
    false -> find_parent(Rest, RevTree, I+1)
  end;
find_parent([], _RevTree, I) ->
  {I, <<"">>}.

delete(StoreName, DocId, RevId, Options) ->
  put(StoreName, #{ <<"id">> => DocId, <<"_rev">> => RevId, <<"_deleted">> => true }, Options).


post(_StoreName, #{<<"_rev">> := _Rev}, _Options) -> {error, not_found};
post(StoreName, Doc, Options) ->
  DocId = case barrel_doc:id(Doc) of
            undefined -> barrel_lib:uniqid();
            Id -> Id
          end,
  put(StoreName, Doc#{<<"id">> => DocId}, Options).


fold_by_id(DbName, Fun, Acc, Opts) ->
  case barrel_store:whereis_db(DbName) of
    undefined -> {error, not_found};
    Db -> fold_by_id_int(Db, Fun, Acc, Opts)
  end.

fold_by_id_int(#db{ id=DbId, store=Store }=Db, UserFun, AccIn, Opts) ->
  Prefix = barrel_keys:prefix(DbId, doc),
  {ok, Snapshot} = rocksdb:snapshot(Store),
  ReadOptions = [{snapshot, Snapshot}],
  IncludeDoc = proplists:get_value(include_doc, Opts, false),
  Opts2 = [{read_options, ReadOptions} | Opts],
  
  WrapperFun =
  fun(_Key, BinDocInfo, Acc) ->
    DocInfo = binary_to_term(BinDocInfo),
    RevId = maps:get(current_rev, DocInfo),
    DocId = maps:get(id, DocInfo),
    Doc = case IncludeDoc of
            true ->
              get_doc_rev(Db, DocId, RevId, ReadOptions);
            false -> {ok, nil}
          end,
  
    UserFun(DocId, DocInfo, Doc, Acc)
  end,
  
  try barrel_rocksdb:fold_prefix(Store, Prefix, WrapperFun, AccIn, Opts2)
  after rocksdb:release_snapshot(Snapshot)
  end.

changes_since(DbName, Since, Fun, AccIn, Opts) when is_binary(DbName), is_integer(Since) ->
  case barrel_store:whereis_db(DbName) of
    undefined -> {error, not_found};
    Db ->  changes_since_int(Db, Since, Fun, AccIn, Opts)
  end.

changes_since_int(Db = #db{ id=DbId, store=Store}, Since0, Fun, AccIn, Opts) ->
  Since = if
            Since0 > 0 -> Since0 + 1;
            true -> Since0
          end,
  Prefix = barrel_keys:prefix(DbId, seq),
  {ok, Snapshot} = rocksdb:snapshot(Store),
  ReadOptions = [{snapshot, Snapshot}],
  FoldOpts = [
    {start_key, <<Since:32>>},
    {read_options, ReadOptions}
  ],
  IncludeDoc = proplists:get_value(include_doc, Opts, false),
  WithHistory = proplists:get_value(history, Opts, last) =:= all,
  WithRevtree =  proplists:get_value(revtree, Opts, false) =:= true,
  
  WrapperFun =
  fun(Key, BinDocInfo, Acc) ->
    DocInfo = binary_to_term(BinDocInfo),
    [_, SeqBin] = binary:split(Key, Prefix),
    <<Seq:32>> = SeqBin,
    RevId = maps:get(current_rev, DocInfo),
    DocId = maps:get(id, DocInfo),
    RevTree = maps:get(revtree, DocInfo),
    
    Changes = case WithHistory of
                false -> [RevId];
                true ->  barrel_revtree:history(RevId, RevTree)
              end,
    
    %% create change
    Change = change_with_revtree(
      change_with_doc(
        changes_with_deleted(
          #{ id => DocId, seq => Seq, changes => Changes}, RevId, RevTree
        ),
        DocId, RevId, Db, ReadOptions, IncludeDoc
      ),
      RevTree,
      WithRevtree
    ),
    Fun(Seq, Change, Acc)
  end,
  
  try barrel_rocksdb:fold_prefix(Store, Prefix, WrapperFun, AccIn, FoldOpts)
  after rocksdb:release_snapshot(Snapshot)
  end.

change_with_revtree(Change, DocInfo, true) ->
  Change#{revtree => maps:get(revtree, DocInfo)};
change_with_revtree(Change, _DocInfo, false) ->
  Change.

change_with_doc(Change, DocId, RevId, Db, ReadOptions, true) ->
  case get_doc_rev(Db, DocId, RevId, ReadOptions) of
    {ok, Doc} -> Change#{ doc => Doc };
    not_found -> Change#{ doc => {error, missing} }
  end;

change_with_doc(Change, _DocId, _RevId, _Ref, _ReadOptions, false) ->
  Change.

changes_with_deleted(Change, RevId, RevTree) ->
  {ok, RevInfo} = barrel_revtree:info(RevId, RevTree),
  case RevInfo of
    #{ deleted := true} -> Change#{deleted => true};
    _ -> Change
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



update_doc(DbName, DocId, Fun) ->
  case barrel_store:whereis_db(DbName) of
    undefined ->
      lager:debug(
        "~s: db ~p not found",
        [?MODULE_STRING, DbName]
      ),
      {error, not_found};
    #db{pid=Pid} ->
      gen_server:call(Pid, {update_doc, DocId, Fun})
  end.

write_system_doc(DbName, DocId, Doc) ->
  with_db(
    DbName,
    fun(#db{id =DbId, pid=Pid}) ->
      EncKey = barrel_keys:sys_key(DbId, DocId),
      EncVal = term_to_binary(Doc),
      gen_server:call(Pid, {put, EncKey, EncVal})
    end
  ).

read_system_doc(DbName, DocId) ->
  with_db(
    DbName,
    fun(#db{id =DbId, store=Store}) ->
      EncKey = barrel_keys:sys_key(DbId, DocId),
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
    fun(#db{id =DbId, pid=Pid}) ->
      EncKey = barrel_keys:sys_key(DbId, DocId),
      gen_server:call(Pid, {delete, EncKey})
    end
  ).


query(DbName, Path, Fun, AccIn, Options) ->
  query(DbName, Path, Fun, AccIn, order_by_key, Options).

query(DbName, Path, Fun, AccIn, OrderBy, Options) ->
  with_db(
    DbName,
    fun(Db) ->
      barrel_query:query(Db, Path, Fun, AccIn, OrderBy, Options)
    end
  ).
  
  

get_id(#{ <<"id">> := DocId }) -> DocId;
get_id(_) -> erlang:error({bad_doc, invalid_docid}).

with_db(DbName, Fun) ->
  case barrel_store:whereis_db(DbName) of
    undefined ->
      lager:debug(
        "~s: db ~p not found",
        [?MODULE_STRING, DbName]
      ),
      {error, not_found};
    Db ->
      Fun(Db)
  end.

start_link(DbName, DbId, Options) ->
  gen_server:start_link(?MODULE, [DbName, DbId, Options], []).

get_db(DbPid) when is_pid(DbPid) ->
  gen_server:call(DbPid, get_db).

init([DbName, DbId, Options]) ->
  process_flag(trap_exit, true),
  self() ! {init_db, Options},
  {ok, init_state(DbName, DbId)}.

init_state(DbName, DbId) ->
  {ok, Store} = barrel_store:get_ref(),
  Db =
    #db{name=DbName,
        id=DbId,
        pid=self(),
        store=Store
    },
  _ = ets:insert(barrel_dbs, Db),
  Db.

handle_call({put, K, V}, _From, Db) ->
  Reply = (catch do_put(K, V, Db)),
  {reply, Reply, Db};
handle_call({delete, K}, _From, Db) ->
  Reply = (catch do_delete(K, Db)),
  {reply, Reply, Db};
handle_call({update_doc, DocId, Fun}, _From, Db) ->
  Reply = (catch do_update(DocId, Fun, Db)),
  {reply, Reply, Db};
handle_call(get_db, _From, Db) ->
  {reply, {ok, Db}, Db};
handle_call(delete_db, From, #db{ name=Name, id=Id, store=Store } = Db) ->
  _ = ets:delete(barrel_dbs, Name),
  %% we return immediately and handle the deletion asynchronously
  gen_server:reply(From, ok),
  ok = do_delete_db(Store, Id),
  {stop, normal, Db#db{ id = <<>>}};
handle_call(_Request, _From, State) ->
  {reply, {error, bad_call}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({init_db, Options}, Db) ->
  NewDb = init_db(Db, Options),
  ets:insert(barrel_dbs, NewDb),
  {noreply, NewDb};
handle_info(_Info, State) ->
  lager:info(
    "~s: received unknonwn message:~p~n",
    [?MODULE_STRING, _Info]
  ),
  {noreply, State}.

terminate(_Reason, #db{id = <<>>}) -> ok;
terminate(_Reason, #db{ name = Name}) ->
  _ = ets:delete(barrel_dbs, Name),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

init_db(Db, Options) ->
  #db{name=DbName, id=DbId, store=Store} = Db,
  Create = maps:get(create_if_missing, Options, false),
  Db2 = case Create of
    true ->
      DbInfo = #{
        updated_seq => 0,
        indexed_seq => 0,
        docs_count => 0,
        deleted_count => 0,
        system_docs_count => 0
      },
      Meta = {DbId, maps:remove(create_if_missing, Options)},
      %% initialize the metadata on the disk
      Batch = [
        {put, barrel_keys:db_key(DbName), term_to_binary(Meta)},
        {put, barrel_keys:db_meta_key(DbId, ?DB_INFO), term_to_binary(DbInfo)}
      ],
      ok = rocksdb:write(Store, Batch, [{sync, true}]),
      Db;
    false ->
      %% for now always store the new options (so we don't forger)
      Meta = {DbId, Options},
      ok = rocksdb:put(Store, barrel_keys:db_key(DbName), term_to_binary(Meta), [{sync, true}]),
      %% extract the metadata
      {ok, BinInfo} = rocksdb:get(Store, barrel_keys:db_meta_key(DbId, ?DB_INFO), []),
      #{updated_seq := Updated,
        indexed_seq := Indexed,
        docs_count := DocsCount,
        deleted_count := DeletedCount,
        system_docs_count := SystemDocsCount} = binary_to_term(BinInfo),
      %% init the db object with them
      Db1 = #db{
        updated_seq = Updated,
        indexed_seq = Indexed,
        docs_count = DocsCount,
        deleted_count = DeletedCount,
        system_docs_count = SystemDocsCount
      },
      %% share the last state
      ets:insert(barrel_dbs, Db1),
      Db1
  end,
  {ok, Indexer} = barrel_indexer:start_link(Db2, Options),
  Db2#db{indexer=Indexer}.
  

%% TODO: replace with DeleteRange in latest rocksdb once it's exposed
do_delete_db(Store, Id) ->
  FinalAcc = barrel_rocksdb:fold_prefix(
    Store,
    barrel_keys:db_prefix(Id),
    fun(Key, _Val, Acc) ->
      Acc2 = [{delete, Key} | Acc],
      if
        length(Acc2) >= 100 ->
          ok = rocksdb:write(Store, Acc2, []),
          {ok, []};
        true ->
          {ok, Acc2}
      end
    end,
    [],
    []
  ),
  case FinalAcc of
    [] ->
      ok;
    _ ->
      ok = rocksdb:write(Store, FinalAcc, []),
      ok
  end.


empty_doc_info() ->
  #{ current_rev => <<>>, revtree => #{}}.

do_update(DocId, Fun, Db = #db{ name=Name, id=DbId, store=Store, indexer=Idx }) ->
  DocInfo = case rocksdb:get(Store, barrel_keys:doc_key(DbId, DocId), []) of
              {ok, DI} -> binary_to_term(DI);
              not_found -> empty_doc_info();
              Error -> throw(Error)
            end,
  
  case Fun(DocInfo) of
    {ok, DocInfo2, Body, NewRev} ->
      Seq = ets:update_counter(barrel_dbs, Name, {#db.updated_seq, 0}),
      LastSeq = maps:get(update_seq, DocInfo2, undefined),
      NewSeq = Seq + 1,
      Inc = case DocInfo2 of
              #{ deleted := true } -> -1;
              _ -> 1
            end,
      
      case write_doc(Db, DocId, LastSeq, Inc, DocInfo2#{ update_seq => NewSeq}, Body) of
        ok ->
          ets:update_counter(barrel_dbs, Name, {#db.updated_seq, 1}),
          ets:update_counter(barrel_dbs, Name, {#db.docs_count, Inc}),
          {ok, _Seq} = barrel_indexer:refresh_index(Idx, Seq),
          _ = do_update_index_seq(NewSeq, Db),
          barrel_db_event:notify(Name, db_updated),
          {ok, DocId, NewRev};
        WriteError ->
          lager:error("db error: error writing ~p on ~p", [DocId, Name]),
          WriteError
      end;
    ok ->
      #{ current_rev := Rev } = DocInfo,
      {ok, DocId, Rev};
    Conflict ->
      {error, Conflict}
  end.


%% TODO: use information in ram?
%% do we really need to read from disk? Why not just overriding the full
%% doc from the content in memory?
bin_infos(#db{id=DbId, store=Store}) ->
  {ok, OldDbInfoBin} = rocksdb:get(Store, barrel_keys:db_meta_key(DbId, ?DB_INFO), []),
  binary_to_term(OldDbInfoBin).

write_doc(Db=#db{ id=DbId, store=Store}, DocId, LastSeq, Inc, DocInfo, Body) ->
  #{update_seq := Seq} = DocInfo,
  #{<<"_rev">> := Rev} = Body,
  OldDbInfo = bin_infos(Db),
  #{ docs_count := Count } = OldDbInfo,
  DbInfo = OldDbInfo#{ docs_count => Count + Inc, updated_seq => Seq },
  DocInfoBin = term_to_binary(DocInfo),
  
  Batch = [
            {put, barrel_keys:rev_key(DbId, DocId, Rev), term_to_binary(Body)},
            {put, barrel_keys:doc_key(DbId, DocId), DocInfoBin},
            {put, barrel_keys:seq_key(DbId, Seq), DocInfoBin},
            {put, barrel_keys:db_meta_key(DbId, ?DB_INFO), term_to_binary(DbInfo)}
          ] ++ case LastSeq of
                 undefined -> [];
                 _ -> [{delete, barrel_keys:seq_key(DbId, LastSeq)}]
               end,
  rocksdb:write(Store, Batch, [{sync, true}]).


do_put(K, V, Db = #db{ name=Name, id=DbId, store=Store}) ->
  #{ system_docs_count := Count} = OldDbInfo = bin_infos(Db),
  DbInfo = OldDbInfo#{ system_docs_count => Count + 1 },
  Batch = [
    {put, K, V},
    {put, barrel_keys:db_meta_key(DbId, ?DB_INFO), term_to_binary(DbInfo)}
  ],
  case rocksdb:write(Store, Batch, [{sync, true}]) of
    ok ->
      ets:update_counter(barrel_dbs, Name, {#db.system_docs_count, 1}),
      ok;
    Error ->
      Error
  end.

do_delete(K, Db = #db{ name=Name, id=DbId, store=Store}) ->
  #{ system_docs_count := Count} = OldDbInfo = bin_infos(Db),
  DbInfo = OldDbInfo#{ system_docs_count => Count -1 },
  Batch = [
    {delete, K},
    {put, barrel_keys:db_meta_key(DbId, ?DB_INFO), term_to_binary(DbInfo)}
  ],
  case rocksdb:write(Store, Batch, [{sync, true}]) of
    ok ->
      ets:update_counter(barrel_dbs, Name, {#db.system_docs_count, -1}),
      ok;
    Error ->
      Error
  end.

do_update_index_seq(Seq, Db = #db{ id=DbId, store=Store}) ->
  OldDbInfo = bin_infos(Db),
  DbInfo = OldDbInfo#{ last_index_seq => Seq },
  ok = rocksdb:put(
    Store, barrel_keys:db_meta_key(DbId, ?DB_INFO), term_to_binary(DbInfo),  [{sync, true}]
  ),
  ets:insert(barrel_dbs, {indexed_seq, Seq}),
  ok.