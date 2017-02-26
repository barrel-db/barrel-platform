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

-module(barrel_local).
-author("benoitc").

%% DOC API

-export([
  put/3,
  put_rev/5,
  get/3,
  mget/5,
  delete/3,
  post/3,
  fold_by_id/4,
  changes_since/4,
  changes_since/5,
  revsdiff/3
]).

-export([
  query/5,
  query/6
]).

-export([
  find_by_key/5
]).

%% Database API

-export([
  create_db/1,
  create_db/2,
  delete_db/1,
  db_infos/1
]).

-export([
  start_replication/2,
  start_replication/3,
  start_replication/4,
  stop_replication/1,
  delete_replication/1,
  replication_info/1
]).


-type dbname() :: binary().
-type db() :: atom().

%% TODO: to define
-type db_infos() :: #{
  name := db(),
  id := binary(),
  docs_count := non_neg_integer(),
  last_update_seq := non_neg_integer(),
  system_docs_count := non_neg_integer(),
  last_index_seq => non_neg_integer()
}.

-type doc() :: map().
-type meta() :: map().
-type rev() :: binary().
-type docid() :: binary().

-type revid() :: binary().

-type revinfo() :: #{
  id := revid(),
  parent := revid(),
  deleted => boolean()
}.

-type revtree() :: #{ revid() => revinfo() }.

-type docinfo() :: #{
  id := docid(),
  current_rev := revid(),
  branched := boolean(),
  conflict := boolean(),
  revtree := revtree()
}.

-type read_options() :: [
  {rev, rev()}
  | {history, boolean()}
  | {max_history, integer()}
  | {ancestors, [rev()]}
].

-type write_options() :: [
  {async, boolean()}
  | {timeout, integer()}
  | {rev, rev()}
].

-type conflict() ::
  {conflict, doc_exists}
  | {conflict, revision_conflict}.

%% TODO: to define
-type fold_options() :: list().

-type change() :: #{
  id := docid(),
  seq := non_neg_integer(),
  changes := [revid()],
  revtree => revtree(),
  doc => doc()
}.


-export_type([
  dbname/0,
  db/0,
  doc/0,
  rev/0,
  docid/0,
  read_options/0,
  write_options/0,
  revid/0,
  revinfo/0,
  revtree/0,
  docinfo/0
]).

-deprecated([create_db/2]).

-include_lib("barrel_common/include/barrel_common.hrl").

create_db(DbId, Config) ->
  lager:warning("barrel_db:create/2 is deprecated", []),
  barrel_store:create_db(Config#{ <<"database_id">> => DbId }).

create_db(Config) ->
  barrel_store:create_db(Config).


delete_db(DbId) ->
  barrel_store:delete_db(DbId).


-spec db_infos(Db::db()) ->
  {ok, DbInfos::db_infos()} | {error, term()}.
db_infos(Db) ->
  barrel_db:infos(Db).

%% Database API.

%% @doc retrieve a document by its key
-spec get(Db, DocId, Options) -> Res when
  Db::db(),
  DocId :: docid(),
  Options :: read_options(),
  Doc :: doc(),
  Res :: {ok, Doc} | {error, not_found} | {error, any()}.
get(Db, DocId, Options) ->
  barrel_db:get(Db, DocId, Options).

%% @doc retrieve several documents
-spec mget(Db, Fun, AccIn, DocIds, Options) -> [Res] when
    Db::db(),
    Fun :: fun(({ok, Doc} | {error, any()} ) -> Res),
    AccIn :: any(),
    DocIds :: [docid()],
    Options :: read_options(),
    Doc :: doc(),
    Res :: any().
mget(Db, Fun, AccIn, DocIds, Options) ->
  barrel_db:mget(Db, Fun, AccIn, DocIds, Options).

%% @doc create or update a document. Return the new created revision
%% with the docid or a conflict.
-spec put(Db, Doc, Options) -> Res when
  Db::db(),
  Doc :: doc(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict()} | {error, any()}.
put(Db, Doc, Options) when is_map(Doc) ->
  ok = validate_docid(Doc),
  Rev = proplists:get_value(rev, Options, <<>>),
  Deleted = false,
  barrel_db:update_doc(Db, barrel_doc:make_doc(Doc, [Rev], Deleted), Options);
put(_,  _, _) ->
  erlang:error(badarg).



%% @doc insert a specific revision to a a document. Useful for the replication.
%% It takes the document id, the doc to edit and the revision history (list of ancestors).
-spec put_rev(Db, Doc, History, Deleted, Options) -> Res when
  Db::db(),
  Doc :: doc(),
  History :: [rev()],
  Deleted :: boolean(),
  Options :: write_options(),
  Res ::  {ok, docid(), rev()} | {error, conflict()} | {error, any()}.
put_rev(Db, Doc, History, Deleted, Options) when is_map(Doc) ->
  ok = validate_docid(Doc),
  Doc2 = barrel_doc:make_doc(Doc, History, Deleted),
  barrel_db:update_doc(Db, Doc2, [{with_conflict, true} | Options]);
put_rev(_, _, _, _, _) ->
  erlang:error(badarg).

%% @doc delete a document
-spec delete(Db, DocId, Options) -> Res when
  Db::db(),
  DocId :: docid(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict()} | {error, any()}.
delete(Db, DocId, Options) ->
  Doc = #{<<"id">> => DocId},
  Rev = proplists:get_value(rev, Options, <<>>),
  Deleted = true,
  barrel_db:update_doc(Db, barrel_doc:make_doc(Doc, [Rev], Deleted), Options).

%% @doc create a document . Like put but only create a document without updating the old one.
%% Optionally the document ID can be set in the doc.
-spec post(Db, Doc, Options) -> Res when
  Db::db(),
  Doc :: doc(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict()} | {error, any()}.
post(Db, Doc0, Options0) ->
  case proplists:get_value(rev, Options0) of
    undefined ->
      DocId = case barrel_doc:id(Doc0) of
                undefined -> barrel_lib:uniqid();
                Id -> Id
              end,
      %% create doc record
      Doc1 = barrel_doc:make_doc(
        maps:put(<<"id">>, DocId, Doc0),
        [<<>>],
        false
      ),
      
      Options1 = case proplists:get_value(is_upsert, Options0, false) of
                   false -> [{error_if_exists, true} | Options0];
                   _ -> Options0
                 end,
      barrel_db:create_doc(Db, Doc1, Options1);
   _Rev ->
      erlang:error(badarg)
  end.

%% @doc fold all docs by Id
-spec fold_by_id(Db, Fun, AccIn, Options) -> AccOut | Error when
  Db::db(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((Doc :: doc(), Meta :: meta(), Acc1 :: any()) -> FunRes),
  Options :: fold_options(),
  AccIn :: any(),
  AccOut :: any(),
  Error :: {error, term()}.
fold_by_id(Db, Fun, Acc, Options) ->
  barrel_db:fold_by_id(Db, Fun, Acc, Options).

%% @doc fold all changes since last sequence
-spec changes_since(Db, Since, Fun, AccIn) -> AccOut when
  Db::db(),
  Since :: non_neg_integer(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((Seq :: non_neg_integer(), Change :: change(), Acc :: any()) -> FunRes),
  AccIn :: any(),
  AccOut :: any().
changes_since(Db, Since, Fun, Acc) ->
  barrel_db:changes_since(Db, Since, Fun, Acc, []).

%% @doc fold all changes since last sequence
-spec changes_since(Db, Since, Fun, AccIn, Opts) -> AccOut when
  Db::db(),
  Since :: non_neg_integer(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((Seq :: non_neg_integer(), Change :: change(), Acc :: any()) -> FunRes),
  AccIn :: any(),
  AccOut :: any(),
  Opts :: list().
changes_since(Db, Since, Fun, Acc, Opts) ->
  barrel_db:changes_since(Db, Since, Fun, Acc, Opts).

%% @doc find in the index a document by its path
-spec query(Db, Path, Fun, AccIn, Options) -> AccOut | Error when
  Db::db(),
  Path :: binary(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((DocId :: docid(), Doc :: doc(), Acc1 :: any()) -> FunRes),
  Options :: fold_options(),
  AccIn :: any(),
  AccOut :: any(),
  Error :: {error, term()}.
query(Db, Path, Fun, AccIn, Opts) ->
  barrel_db:query(Db, Path, Fun, AccIn, Opts).

%% @doc find in the index a document
-spec query(Db, Path, Fun, AccIn, OrderBy, Options) -> AccOut | Error when
  Db::db(),
  Path :: binary(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((DocId :: docid(), Doc :: doc(), Acc1 :: any()) -> FunRes),
  OrderBy :: order_by_key | order_by_value | {order_by_child, ChildKey :: binary()},
  Options :: fold_options(),
  AccIn :: any(),
  AccOut :: any(),
  Error :: {error, term()}.
query(Db, Path, Fun, AccIn, OrderBy, Opts) ->
  barrel_db:query(Db, Path, Fun, AccIn, OrderBy, Opts).

%% @deprecated
find_by_key(Db, Path, Fun, AccIn, Opts) ->
  lager:warning("~s : find_by_key is deprecated", [?MODULE_STRING]),
  barrel_db:query(Db, Path, Fun, AccIn, Opts).

%% @doc get all revisions ids that differ in a doc from the list given
-spec revsdiff(Db, DocId, RevIds) -> Res when
  Db::db(),
  DocId :: docid(),
  RevIds :: [revid()],
  Res:: {ok, Missing :: [revid()], PossibleAncestors :: [revid()]}.
revsdiff(Db, DocId, RevIds) ->
  barrel_db:revsdiff(Db, DocId, RevIds).


%% replication API
start_replication(Source, Target) ->
  start_replication(Source, Target, []).

%% TODO: maybe we should pass the calculated replication id in options?
start_replication(Source, Target, Options) when is_list(Options) ->
  Name = barrel_replicate_task:repid(Source, Target),
  start_replication(Name, Source, Target, Options);

start_replication(Name, Source, Target) ->
  start_replication(Name, Source, Target, []).

start_replication(Name, Source, Target, Options) ->
  Config = #{source => Source, target => Target, options => Options},
  case barrel_replicate:start_replication(Name, Config) of
    ok -> {ok, Name};
    Error -> Error
  end.

stop_replication(Name) ->
  barrel_replicate:stop_replication(Name).

delete_replication(Name) ->
  barrel_replicate:delete_replication(Name).

replication_info(Name) ->
  case barrel_replicate:where(Name) of
    Pid when is_pid(Pid) -> barrel_replicate_task:info(Pid);
    undefined -> {error, not_found}
  end.


%% internal
validate_docid(#{ <<"id">> := _DocId }) -> ok;
validate_docid(_) -> erlang:error({bad_doc, invalid_docid}).
