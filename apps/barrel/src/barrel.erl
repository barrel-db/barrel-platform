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

-module(barrel).
-author("benoitc").

%% DOC API

-export([
  put/3,
  put_rev/4,
  get/3,
  delete/4,
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
  create_db/2,
  delete_db/1,
  db_infos/1
]).


-export([
  attach/4,
  attach/5,
  get_attachment/4,
  get_attachment_binary/4,
  replace_attachment/5,
  replace_attachment_binary/5,
  delete_attachment/4,
  attachments/3
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


create_db(Name, Options) ->
  barrel_store:create_db(Name, Options).

delete_db(Name) ->
  barrel_store:delete_db(Name).


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


%% @doc create or update a document. Return the new created revision
%% with the docid or a conflict.
-spec put(Db, Doc, Options) -> Res when
  Db::db(),
  Doc :: doc(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict()} | {error, any()}.
put(Db, Doc, Options) ->
  barrel_db:put(Db, Doc, Options).

%% @doc insert a specific revision to a a document. Useful for the replication.
%% It takes the document id, the doc to edit and the revision history (list of ancestors).
-spec put_rev(Db, Doc, History, Options) -> Res when
  Db::db(),
  Doc :: doc(),
  History :: [rev()],
  Options :: write_options(),
  Res ::  {ok, docid(), rev()} | {error, conflict()} | {error, any()}.
put_rev(Db, Doc, History, Options) ->
  barrel_db:put_rev(Db, Doc, History, Options).

%% @doc delete a document
-spec delete(Db, DocId, RevId, Options) -> Res when
  Db::db(),
  DocId :: docid(),
  RevId :: rev(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict()} | {error, any()}.
delete(Db, DocId, RevId, Options) ->
  barrel_db:delete(Db, DocId, RevId, Options).

%% @doc create a document . Like put but only create a document without updating the old one.
%% A doc shouldn't have revision. Optionally the document ID can be set in the doc.
-spec post(Db, Doc, Options) -> Res when
  Db::db(),
  Doc :: doc(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict()} | {error, any()}.
post(Db, Doc, Options) ->
  barrel_db:post(Db, Doc, Options).

%% @doc fold all docs by Id
-spec fold_by_id(Db, Fun, AccIn, Options) -> AccOut | Error when
  Db::db(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((DocId :: docid(), DocInfo :: docinfo(), Doc :: doc(), Acc1 :: any()) -> FunRes),
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



attach(Db, DocId, AttDescription, Options) ->
  barrel_attachments:attach(Db, DocId, AttDescription, Options).

attach(Db, DocId, AttDescription, Binary, Options) ->
  barrel_attachments:attach(Db, DocId, AttDescription, Binary, Options).

get_attachment(Db, DocId, AttId, Options) ->
  barrel_attachments:get_attachment(Db, DocId, AttId, Options).

get_attachment_binary(Db, DocId, AttId, Options) ->
  barrel_attachments:get_attachment_binary(Db, DocId, AttId, Options).

replace_attachment(Db, DocId, AttId, AttDescription, Options) ->
  barrel_attachments:replace_attachment(Db, DocId, AttId, AttDescription, Options).

replace_attachment_binary(Db, DocId, AttId, Binary, Options) ->
  barrel_attachments:replace_attachment_binary(Db, DocId, AttId, Binary, Options).

delete_attachment(Db, DocId, AttId, Options) ->
  barrel_attachments:delete_attachment(Db, DocId, AttId, Options).

attachments(Db, DocId, Options) ->
  barrel_attachments:attachments(Db, DocId, Options).

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
