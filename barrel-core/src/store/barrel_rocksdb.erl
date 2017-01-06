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

-module(barrel_rocksdb).
-author("Benoit Chesneau").
-behaviour(gen_server).

%% API
-export([
  open_store/2,
  delete_store/1,
  close_store/1,
  last_update_seq/1,
  infos/1,
  update_doc/3,
  get_doc_info/3,
  get_doc/6,
  fold_by_id/4,
  changes_since/5,
  find_by_key/5,
  write_system_doc/3,
  read_system_doc/2,
  delete_system_doc/2
]).

-export([start_link/2]).

-export([
  fold_prefix/5,
  meta_key/1,
  idx_forward_path_key/1,
  idx_reverse_path_key/1,
  idx_last_doc_key/1
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


open_store(Name, DbOpts) ->
  ProcName = proc_name(Name),
  case whereis(ProcName) of
    Pid when is_pid(Pid) -> ok;
    undefined ->
      barrel_store_sup:start_store(Name, DbOpts#{ adapter => ?MODULE})
  end.

close_store(Name) ->
  case opt_call(Name, close_db) of
    {error, noproc} ->
      lager:debug(
        "~p: close_db(~p) -> noproc~n",
        [self(), Name]),
      ok;
    ok ->
      ok;
    _Other ->
      lager:debug(
        "~p: close_db(~p) -> _Other = ~p~n",
        [self(), Name, _Other]
      ),
      _ = barrel_store_sup:stop_store(Name),
      ok
  end.

delete_store(Name) ->
  case opt_call(Name, delete_db) of
    {error, noproc} ->
      Dir = ets:lookup_element(tab_name(Name), dir, 2),
      do_delete_db(Dir);
    ok ->
      ok
  end,
  _ = barrel_store_sup:stop_store(Name),
  ok.

last_update_seq(Db) -> ets:lookup_element(tab_name(Db), last_update_seq, 2).

infos(Db) ->
  Keys = [name, id, doc_count, last_update_seq, system_doc_count, last_index_seq],
  Info = lists:foldl(
    fun(K, MI) ->
      [{K, V}] = ets:lookup(tab_name(Db), K),
      MI#{ K => V }
    end, #{}, Keys),
  Info.

update_doc(Db, DocId, Fun) ->
  call(Db, {update_doc, DocId, Fun}).


get_doc(Db, DocId, Rev, WithHistory, MaxHistory, HistoryFrom) ->
  Ref = get_ref(Db),
  {ok, Snapshot} = rocksdb:snapshot(Ref),
  ReadOptions = [{snapshot, Snapshot}],

  try get_doc1(Ref, DocId, Rev, WithHistory, MaxHistory, HistoryFrom, ReadOptions)
  after rocksdb:release_snapshot(Snapshot)
  end.

get_doc1(Ref, DocId, Rev, WithHistory, MaxHistory, Ancestors, ReadOptions) ->
  case get_doc_info({ref, Ref}, DocId, ReadOptions) of
    {ok, #{revtree := RevTree} = DocInfo} ->
      RevId = case Rev of
                <<"">> -> maps:get(current_rev, DocInfo);
                UserRev -> UserRev
              end,

      case get_doc_rev(Ref, DocId, RevId, ReadOptions) of
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

get_doc_rev(Ref, DocId, RevId, ReadOptions) ->
  case rocksdb:get(Ref, rev_key(DocId, RevId), ReadOptions) of
    {ok, Bin} -> {ok, binary_to_term(Bin)};
    not_found -> {error, not_found};
    Error -> Error
  end.

get_doc_info(Db, DocId, ReadOptions) when is_atom(Db) ->
  Ref = get_ref(Db),
  get_doc_info({ref, Ref}, DocId, ReadOptions);
get_doc_info({ref, Ref}, DocId, ReadOptions) ->
  DocKey = doc_key(DocId),
  case rocksdb:get(Ref, DocKey, ReadOptions) of
    {ok, BinDocInfo} -> {ok, binary_to_term(BinDocInfo)};
    not_found -> {error, not_found}
  end.

fold_prefix(Db, Prefix, Fun, AccIn, Opts) ->
  ReadOptions = proplists:get_value(read_options, Opts, []),

  {ok, Itr} = rocksdb:iterator(Db, ReadOptions),
  try do_fold_prefix(Itr, Prefix, Fun, AccIn, barrel_lib:parse_fold_options(Opts))
  after rocksdb:iterator_close(Itr)
  end.

do_fold_prefix(Itr, Prefix, Fun, AccIn, Opts = #{ gt := GT, gte := GTE}) ->
  {Start, Inclusive} = case {GT, GTE} of
                         {nil, nil} -> {Prefix, true};
                         {first, _} -> {Prefix, false};
                         {_, first} -> {Prefix, true};
                         {_, K} when is_binary(K) ->
                           FirstKey = << Prefix/binary, K/binary >>,
                           {FirstKey, true};
                         {K, _} when is_binary(K) ->
                           FirstKey = << Prefix/binary, K/binary >>,
                           {FirstKey, false};
                         _ ->
                           error(badarg)
                       end,
  Opts2 = Opts#{prefix => Prefix},
  case rocksdb:iterator_move(Itr, Start) of
    {ok, Start, _V} when Inclusive /= true ->
      fold_prefix_loop(rocksdb:iterator_move(Itr, next), Itr, Fun, AccIn, 0, Opts2);
    Next ->
      fold_prefix_loop(Next, Itr, Fun, AccIn, 0, Opts2)
  end.

fold_prefix_loop({error, iterator_closed}, _Itr, _Fun, Acc, _N, _Opts) ->
  throw({iterator_closed, Acc});
fold_prefix_loop({error, invalid_iterator}, _Itr, _Fun, Acc, _N, _Opts) ->
  Acc;

fold_prefix_loop({ok, K, _V}=KV, Itr, Fun, Acc, N0,
  Opts = #{ lt := Lt, lte := nil, prefix := Prefix})
  when Lt =:= nil orelse K < <<Prefix/binary, Lt/binary>> ->
  fold_prefix_loop1(KV, Itr, Fun, Acc, N0, Opts);


fold_prefix_loop({ok, K, _V}=KV, Itr, Fun, Acc, N,
  Opts = #{ lt := nil, lte := Lte, prefix := Prefix})
  when Lte =:= nil orelse K =< <<Prefix/binary, Lte/binary>> ->
  fold_prefix_loop1(KV, Itr, Fun, Acc, N, Opts);


fold_prefix_loop({ok, K, V}, _Itr, Fun, Acc, _N,  #{ lt := nil, lte := K, prefix := P}) ->
  case match_prefix(K, P) of
    true ->
      case Fun(K, V, Acc) of
        {ok, Acc2} -> Acc2;
        {stop, Acc2} -> Acc2;
        stop -> Acc
      end;
    false ->
      Acc
  end;
fold_prefix_loop(_KV, _Itr, _Fun, Acc, _N, _Opts) ->
  Acc.

fold_prefix_loop1({ok, K, V}, Itr, Fun, Acc0, N0, Opts) ->
  #{max := Max, prefix := P} = Opts,
  N = N0 + 1,
  case match_prefix(K, P) of
    true ->
      case Fun(K, V, Acc0) of
        {ok, Acc} when (Max =:= 0) orelse (N < Max) ->
          fold_prefix_loop(rocksdb:iterator_move(Itr, next),
            Itr, Fun, Acc, N, Opts);
        {ok, Acc} -> Acc;
        stop -> Acc0;
        {stop, Acc} -> Acc
      end;
    false ->
      Acc0
  end.

match_prefix(Bin, Prefix) ->
  L = byte_size(Prefix),
  case Bin of
    << Prefix:L/binary, _/binary >> -> true;
    _ -> false
  end.

fold_by_id(Db, Fun, AccIn, Opts) ->
  Ref = get_ref(Db),
  Prefix = << 0, 50, 0 >>,
  {ok, Snapshot} = rocksdb:snapshot(Ref),
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
                get_doc_rev(Ref, DocId, RevId, ReadOptions);
              false -> {ok, nil}
            end,

      Fun(DocId, DocInfo, Doc, Acc)
    end,

  try fold_prefix(Ref, Prefix, WrapperFun, AccIn, Opts2)
  after rocksdb:release_snapshot(Snapshot)
  end.

changes_since(Db, Since, Fun, AccIn, Opts) when is_atom(Db) ->
  Ref = get_ref(Db),
  changes_since({ref, Ref}, Since, Fun, AccIn, Opts);
changes_since({ref, Ref}, Since, Fun, AccIn, Opts) ->
  Prefix = << 0, 100, 0 >>,
  {ok, Snapshot} = rocksdb:snapshot(Ref),
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
          DocId, RevId, Ref, ReadOptions, IncludeDoc
        ),
        RevTree,
        WithRevtree
      ),
      Fun(Seq, Change, Acc)
    end,

  try fold_prefix(Ref, Prefix, WrapperFun, AccIn, FoldOpts)
  after rocksdb:release_snapshot(Snapshot)
  end.

change_with_revtree(Change, DocInfo, true) ->
  Change#{revtree => maps:get(revtree, DocInfo)};
change_with_revtree(Change, _DocInfo, false) ->
  Change.

change_with_doc(Change, DocId, RevId, Ref, ReadOptions, true) ->
  case get_doc_rev(Ref, DocId, RevId, ReadOptions) of
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

write_system_doc(Db, DocId, Doc) ->
  EncKey = sys_key(DocId),
  EncVal = term_to_binary(Doc),
  call(Db, {put, EncKey, EncVal}).

read_system_doc(Db, DocId) ->
  case rocksdb:get(get_ref(Db), sys_key(DocId), []) of
    {ok, Bin} -> {ok, binary_to_term(Bin)};
    not_found -> {error, not_found};
    Error -> Error
  end.

delete_system_doc(Db, DocId) ->
  EncKey = sys_key(DocId),
  call(Db, {delete, EncKey}).

%% TODO: force the query to pass the "$." first?
find_by_key(Db, Path, Fun, AccIn, Options ) ->
  Ref = get_ref(Db),
  Key= parse_key(Path),

  StartKey = case proplists:get_value(start_at, Options) of
               undefined -> nil;
               Start -> idx_forward_path_key(parse_key(Start))
             end,
  EndKey = case proplists:get_value(end_at, Options) of
             undefined -> nil;
             End -> idx_forward_path_key(parse_key(End))
           end,
  Max = proplists:get_value(limit_to_last, Options, 0),
  Prefix = idx_forward_path_key(Key),

  {ok, Snapshot} = rocksdb:snapshot(Ref),
  ReadOptions = [{snapshot, Snapshot}],
  FoldOptions = [{gte, StartKey}, {lte, EndKey}, {max, Max}, {read_options, ReadOptions}],

  WrapperFun =
    fun(_KeyBin, BinEntries, Acc) ->
        Entries = binary_to_term(BinEntries),
        fold_entries(Entries, Fun, Ref, ReadOptions, Acc)
    end,

  try fold_prefix(Ref, Prefix, WrapperFun, AccIn, FoldOptions)
  after rocksdb:release_snapshot(Snapshot)
  end.

fold_entries([DocId | Rest], Fun, Ref, ReadOptions, Acc) ->
  Res = get_doc1(Ref, DocId, <<>>, false, 0, [], ReadOptions),
  case Res of
    {ok, Doc} ->
      case Fun(DocId, Doc, Acc) of
        {ok, Acc2} ->
          fold_entries(Rest, Fun, Ref, ReadOptions, Acc2);
        Else ->
          Else
      end;
    _ ->
      {ok, Acc}
  end;
fold_entries([], _Fun, _Ref, _ReadOptions, Acc) ->
  {ok, Acc}.

parse_key(Path) ->
  case Path of
    <<>> -> <<"$">>;
    <<"/">> -> <<"$">>;
    _ ->
      Parts = barrel_json:decode_path(<< "$/", Path/binary >>),
      Len = length(Parts),
      if
        Len =< 3 -> Parts;
        true ->
          lists:sublist(Parts, Len - 2, Len)
      end
  end.

get_ref(Name) ->
  call(Name, get_ref).
%%  case catch ets:lookup_element(tab_name(Name), ref, 2) of
%%    {'EXIT', _} ->
%%      lager:error(
%%        "barrel_rocksdb:get_ref(~p) -> noproc",
%%        [Name]
%%      ),
%%      {error, noproc};
%%    Ref -> Ref
%%  end.

opt_call(Name, Req) ->
  ProcName = proc_name(Name),
  case whereis(ProcName) of
    undefined ->
      lager:debug(
        "proc_name(~p): ~p: NO PROCESS~n",
        [Name, ProcName]
      ),
      {error, noproc};
    Pid when is_pid(Pid) ->
      gen_server:call(Pid, Req, infinity)
  end.


call(Name, Req) ->
  ProcName = proc_name(Name),
  gen_server:call(ProcName, Req, infinity).



start_link(Name, Options) ->
  ProcName = proc_name(Name),
  gen_server:start_link({local, ProcName}, ?MODULE, [Name, Options], []).

tab_name(Name) ->
  barrel_lib:to_atom("barrel_rocksdb_info_" ++ atom_to_list(Name)).

proc_name(Name) ->
  barrel_lib:to_atom("barrel_rocksdb_proc_" ++ atom_to_list(Name)).


init([Name, Options]) ->
  process_flag(trap_exit, true),
  DbDir = db_dir(Name, Options),
  {ok, Ref} = init_db(DbDir, Options),
  Ets = ets:new(tab_name(Name), [ordered_set, protected, named_table]),
  ok = load_infos(Ref, Name, DbDir, Ets),
  {ok, Indexer} = barrel_rocksdb_indexer:start_link(self(), Name, Ref, Options),
  {ok, #{ name => Name, dir => DbDir, ref => Ref, ets => Ets, indexer => Indexer}}.

handle_call(get_ref, _From, State = #{ ref := Ref }) ->
  {reply, Ref, State};

handle_call({put, K, V}, _From, State) ->
  Reply = (catch do_put(K, V, State)),
  {reply, Reply, State};

handle_call({delete, K}, _From, State) ->
  Reply = (catch do_delete(K, State)),
  {reply, Reply, State};

handle_call({update_doc, DocId, Fun}, _From, State) ->
  Reply = (catch do_update(DocId, Fun, State)),
  {reply, Reply, State};

handle_call(close_db, _From, State = #{ref := Ref, ets := Ets}) ->
  _ = (catch erocksdb_close(Ref)),
  _ = (catch ets:delete(Ets)),
  {stop, normal, ok, maps:remove(ref, State)};

handle_call(delete_db, _From, State=#{ ref := Ref, ets := Ets, dir := Dir}) ->
  _ = (catch erocksdb_close(Ref)),
  _ = (catch ets:delete(Ets)),
  do_delete_db(Dir),
  {stop, normal, ok, maps:remove(ref, State)};


handle_call(_Request, _From, State) ->
  {reply, {error, bad_call}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({last_index_seq, Seq}, State) ->
  do_update_index_seq(Seq, State),
  {noreply, State};

handle_info(_Info, State) ->
  lager:info(
    "barrel_rocksdb: received unknonwn message:~p~n",
    [_Info]
  ),
  {noreply, State}.

terminate(_Reason, State = #{ ets := Ets}) ->
  lager:info(
    "barrel_rocksdb: terminated: ~p~n",
    [_Reason]
  ),
  case maps:find(ref, State) of
    {ok, Ref} ->
      _ = (catch ets:delete(Ets)),
      _ = (catch erocksdb_close(Ref));
    error ->
      ok
  end,
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

db_dir(Name, Options) ->
  DefaultDir = filename:join([Name, barrel_lib:data_dir()]),
  Dir = maps:get(dir, Options, DefaultDir),
  filelib:ensure_dir(filename:join(Dir, "empty")),
  Dir.

init_db(Dir, Options) ->
  InMemory = maps:get(in_memory, Options, false),
  RocksDbOptions = maps:get(rocksdb_options, Options, []),
  DbOpts = case InMemory of
             true ->
               [{create_if_missing, true}, {in_memory, true} | RocksDbOptions];
             false ->
               [{create_if_missing, true} | RocksDbOptions]
           end,
  rocksdb:open(Dir, DbOpts).

load_infos(Ref, Name, Dir, Ets) ->
  _ = ets:insert(Ets, {ref, Ref}),
  case rocksdb:get(Ref, meta_key(0), []) of
    {ok, BinInfos} ->
      Infos = binary_to_term(BinInfos),
      ets:insert(Ets, maps:to_list(Infos#{ name => Name, dir => Dir }));
    not_found ->
      Infos = #{
        id => barrel_lib:uniqid(),
        last_update_seq => 0,
        doc_count => 0,
        system_doc_count => 0,
        last_index_seq => 0
      },
      ok = rocksdb:put(Ref, meta_key(0), term_to_binary(Infos), [{sync, true}]),
      ets:insert(Ets, maps:to_list(Infos#{ name => Name, dir => Dir }))
  end,
  ok.

do_delete_db(Dir) ->
  (catch rocksdb:destroy(Dir, [])).

erocksdb_close(Ref) ->
  Res = rocksdb:close(Ref),
  erlang:garbage_collect(),
  Res.

empty_doc_info() ->
  #{ current_rev => <<>>, revtree => #{}}.

do_update(DocId, Fun, St = #{ name := Name, ref := Ref, ets := Ets, indexer := Idx }) ->
  DocInfo = case rocksdb:get(Ref, doc_key(DocId), []) of
              {ok, DI} -> binary_to_term(DI);
              not_found -> empty_doc_info();
              Error -> throw(Error)
            end,
  case Fun(DocInfo) of
    {ok, DocInfo2, NewDoc} ->
      Seq = ets:update_counter(Ets, last_update_seq, {2, 0}),
      LastSeq = maps:get(update_seq, DocInfo2, undefined),
      NewSeq = Seq + 1,
      Inc = case DocInfo2 of
              #{ deleted := true } -> -1;
              _ -> 1
            end,
      case write_doc(Ref, DocId, LastSeq, Inc, DocInfo2#{ update_seq => NewSeq}, NewDoc) of
        ok ->
          ets:update_counter(Ets, last_update_seq, {2, 1}),
          ets:update_counter(Ets, doc_count, {2, Inc}),
          {ok, _Seq} = barrel_rocksdb_indexer:refresh_index(Idx, NewSeq),
          _ = do_update_index_seq(NewSeq, St),
          barrel_db_event:notify(Name, db_updated),
          {ok, NewDoc};
        WriteError ->
          lager:error("db error: error writing ~p on ~p", [DocId, Name]),
          WriteError
      end;
    {no_update, Doc} ->
      {ok, Doc};
    Conflict ->
      {error, Conflict}
  end.


bin_infos(Ref) ->
  {ok, OldDbInfoBin} = rocksdb:get(Ref, meta_key(0), []),
  binary_to_term(OldDbInfoBin).

write_doc(Ref, DocId, LastSeq, Inc, DocInfo, Body) ->
  #{update_seq := Seq} = DocInfo,
  #{<<"_rev">> := Rev} = Body,
  OldDbInfo = bin_infos(Ref),
  #{ doc_count := Count } = OldDbInfo,
  DbInfo = OldDbInfo#{ doc_count => Count + Inc, last_update_seq => Seq },
  DocInfoBin = term_to_binary(DocInfo),

  Batch = [
    {put, rev_key(DocId, Rev), term_to_binary(Body)},
    {put, doc_key(DocId), DocInfoBin},
    {put, seq_key(Seq), DocInfoBin},
    {put, meta_key(0), term_to_binary(DbInfo)}
  ] ++ case LastSeq of
         undefined -> [];
         _ -> [{delete, seq_key(LastSeq)}]
       end,
  rocksdb:write(Ref, Batch, [{sync, true}]).


do_put(K, V, #{ ref := Ref, ets := Ets}) ->
  #{ system_doc_count := Count} = OldDbInfo = bin_infos(Ref),
  DbInfo = OldDbInfo#{ system_doc_count => Count + 1 },

  Batch = [
    {put, K, V},
    {put, meta_key(0), term_to_binary(DbInfo)}
  ],
  case rocksdb:write(Ref, Batch, [{sync, true}]) of
    ok ->
      ets:update_counter(Ets, system_doc_count, {2, 1}),
      ok;
    Error ->
      Error
  end.

do_delete(K, #{ ref := Ref, ets := Ets}) ->
  #{ system_doc_count := Count} = OldDbInfo = bin_infos(Ref),
  DbInfo = OldDbInfo#{ system_doc_count => Count -1 },

  Batch = [
    {delete, K},
    {put, meta_key(0), term_to_binary(DbInfo)}
  ],
  case rocksdb:write(Ref, Batch, [{sync, true}]) of
    ok ->
      ets:update_counter(Ets, system_doc_count, {2, -1}),
      ok;
    Error ->
      Error
  end.

do_update_index_seq(Seq, #{ ref := Ref, ets := Ets}) ->
  OldDbInfo = bin_infos(Ref),
  DbInfo = OldDbInfo#{ last_index_seq =>  Seq },
  ok = rocksdb:put(Ref, meta_key(0), term_to_binary(DbInfo), [{sync, true}]),
  ets:insert(Ets, {last_index_seq, Seq}),
  ok.


%% key api

meta_key(Meta) -> <<  0, (barrel_lib:to_binary(Meta))/binary >>.

doc_key(DocId) -> << 0, 50, 0,  DocId/binary >>.

seq_key(Seq) -> << 0, 100, 0, Seq:32>>.

sys_key(DocId) -> << 0, 200, 0, DocId/binary>>.

rev_key(DocId, Rev) -> << DocId/binary, 1, Rev/binary >>.

idx_last_doc_key(DocId) -> <<  0, 400, 0, DocId/binary >>.

idx_forward_path_key(Path) -> << 0, 410, 0, (encode_path(Path))/binary >>.

idx_reverse_path_key(Path) -> << 0, 420, 0, (encode_path(Path))/binary >>.

encode_path(Path) -> encode_path(Path, <<>>).

encode_path([P | R], Acc) ->
  encode_path(R, << Acc/binary, (sext:encode(P))/binary, "/" >>);
encode_path([], Acc) ->
  Acc.
