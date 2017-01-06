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
  post/3,
  put/3,
  put_rev/3,
  get/3,
  delete/3,
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
  open_store/2,
  close_store/1,
  delete_store/1,
  store_infos/1
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
-type store() :: atom().

%% TODO: to define
-type store_infos() :: #{
  name := store(),
  id := binary(),
  doc_count := non_neg_integer(),
  last_update_seq := non_neg_integer(),
  system_doc_count := non_neg_integer(),
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

-type update_options() :: [
  {rev, rev()}
  | {async, boolean()}
  | {timeout, integer()}
].

-type create_options() :: [
{upsert, boolean()}
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
  store/0,
  doc/0,
  rev/0,
  docid/0,
  read_options/0,
  update_options/0,
  create_options/0,
  revid/0,
  revinfo/0,
  revtree/0,
  docinfo/0
]).


open_store(Name, Options) ->
  Module = maps:get(adapter, Options, barrel_rocksdb),
  Module:open_store(Name, Options).

close_store(Name) ->
  case ets:lookup(barrel_stores, Name) of
    [] ->
      ok;
    [{Name, Mod}] ->
      Mod:stop_store(Name)
  end.


delete_store(Name) ->
  case ets:lookup(barrel_stores, Name) of
    [] ->
      ok;
    [{Name, Mod}] ->
      Mod:delete_store(Name)
  end.


-spec store_infos(Store::store()) ->
  {ok, DbInfos::store_infos()} | {error, term()}.
store_infos(Store) ->
  barrel_store:infos(Store).

%% Database API.

%% @doc retrieve a document by its key
-spec get(Store, DocId, Options) -> Res when
  Store::store(),
  DocId :: docid(),
  Options :: read_options(),
  Doc :: doc(),
  Res :: {ok, Doc} | {error, not_found} | {error, any()}.
get(Store, DocId, Options) ->
  barrel_store:get(Store, DocId, Options).


%% @doc create or update a document. Return the new created revision
%% with the docid or a conflict.
-spec put(Store, Doc, Options) -> Res when
  Store::store(),
  Doc :: doc(),
  Options :: update_options(),
  Res :: {ok, doc()} | {error, conflict()} | {error, any()}.
put(Store, Doc, Options) ->
  barrel_store:put(Store, Doc, Options).

%% @doc insert a specific revision to a a document. Useful for the replication.
%% It takes the document id, the doc to edit and the revision history (list of ancestors).
-spec put_rev(Store, Doc, History) -> Res when
  Store::store(),
  Doc :: doc(),
  History :: [rev()],
  Res ::  {ok, doc()} | {error, conflict()} | {error, any()}.
put_rev(Store, Doc, History) ->
  barrel_store:put_rev(Store, Doc, History).

%% @doc delete a document
-spec delete(Store, DocId, Options) -> Res when
  Store::store(),
  DocId :: docid(),
  Options :: update_options(),
  Res :: ok | {error, conflict()} | {error, any()}.
delete(Store, DocId, Options) ->
  barrel_store:delete(Store, DocId, Options).

%% @doc create a document . Like put but only create a document without updating the old one.
%% A doc shouldn't have revision. Optionally the document ID can be set in the doc.
-spec post(Store, Doc, Options) -> Res when
  Store::store(),
  Doc :: doc(),
  Options :: create_options(),
  Res :: {ok, doc()} | {error, conflict()} | {error, any()}.
post(Store, Doc, Options) ->
  barrel_store:post(Store, Doc, Options).

%% @doc fold all docs by Id
-spec fold_by_id(Store, Fun, AccIn, Options) -> AccOut | Error when
  Store::store(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((DocId :: docid(), DocInfo :: docinfo(), Doc :: doc(), Acc1 :: any()) -> FunRes),
  Options :: fold_options(),
  AccIn :: any(),
  AccOut :: any(),
  Error :: {error, term()}.
fold_by_id(Store, Fun, Acc, Options) ->
  barrel_store:fold_by_id(Store, Fun, Acc, Options).

%% @doc fold all changes since last sequence
-spec changes_since(Store, Since, Fun, AccIn) -> AccOut when
  Store::store(),
  Since :: non_neg_integer(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((Seq :: non_neg_integer(), Change :: change(), Acc :: any()) -> FunRes),
  AccIn :: any(),
  AccOut :: any().
changes_since(Store, Since, Fun, Acc) ->
  barrel_store:changes_since(Store, Since, Fun, Acc, []).

%% @doc fold all changes since last sequence
-spec changes_since(Store, Since, Fun, AccIn, Opts) -> AccOut when
  Store::store(),
  Since :: non_neg_integer(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((Seq :: non_neg_integer(), Change :: change(), Acc :: any()) -> FunRes),
  AccIn :: any(),
  AccOut :: any(),
  Opts :: list().
changes_since(Store, Since, Fun, Acc, Opts) ->
  barrel_store:changes_since(Store, Since, Fun, Acc, Opts).

%% @doc find in the index a document by its path
-spec query(Store, Path, Fun, AccIn, Options) -> AccOut | Error when
  Store::store(),
  Path :: binary(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((DocId :: docid(), Doc :: doc(), Acc1 :: any()) -> FunRes),
  Options :: fold_options(),
  AccIn :: any(),
  AccOut :: any(),
  Error :: {error, term()}.
query(Store, Path, Fun, AccIn, Opts) ->
  barrel_store:query(Store, Path, Fun, AccIn, Opts).

%% @doc find in the index a document
-spec query(Store, Path, Fun, AccIn, OrderBy, Options) -> AccOut | Error when
  Store::store(),
  Path :: binary(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((DocId :: docid(), Doc :: doc(), Acc1 :: any()) -> FunRes),
  OrderBy :: order_by_key | order_by_value | {order_by_child, ChildKey :: binary()},
  Options :: fold_options(),
  AccIn :: any(),
  AccOut :: any(),
  Error :: {error, term()}.
query(Store, Path, Fun, AccIn, OrderBy, Opts) ->
  barrel_store:query(Store, Path, Fun, AccIn, OrderBy, Opts).

%% @deprecated
find_by_key(Store, Path, Fun, AccIn, Opts) ->
  lager:warning("~s : find_by_key is deprecated", [?MODULE_STRING]),
  barrel_store:query(Store, Path, Fun, AccIn, Opts).

%% @doc get all revisions ids that differ in a doc from the list given
-spec revsdiff(Store, DocId, RevIds) -> Res when
  Store::store(),
  DocId :: docid(),
  RevIds :: [revid()],
  Res:: {ok, Missing :: [revid()], PossibleAncestors :: [revid()]}.
revsdiff(Store, DocId, RevIds) ->
  barrel_store:revsdiff(Store, DocId, RevIds).



attach(Store, DocId, AttDescription, Options) ->
  barrel_attachments:attach(Store, DocId, AttDescription, Options).

attach(Store, DocId, AttDescription, Binary, Options) ->
  barrel_attachments:attach(Store, DocId, AttDescription, Binary, Options).

get_attachment(Store, DocId, AttId, Options) ->
  barrel_attachments:get_attachment(Store, DocId, AttId, Options).

get_attachment_binary(Store, DocId, AttId, Options) ->
  barrel_attachments:get_attachment_binary(Store, DocId, AttId, Options).

replace_attachment(Store, DocId, AttId, AttDescription, Options) ->
  barrel_attachments:replace_attachment(Store, DocId, AttId, AttDescription, Options).

replace_attachment_binary(Store, DocId, AttId, Binary, Options) ->
  barrel_attachments:replace_attachment_binary(Store, DocId, AttId, Binary, Options).

delete_attachment(Store, DocId, AttId, Options) ->
  barrel_attachments:delete_attachment(Store, DocId, AttId, Options).

attachments(Store, DocId, Options) ->
  barrel_attachments:attachments(Store, DocId, Options).

%% replication API
start_replication(Source, Target) ->
  start_replication(Source, Target, []).

%% TODO: maybe we should pass the calculated replication id in options?
start_replication(Source, Target, Options) when is_list(Options) ->
  Name = barrel_replicate:repid(Source, Target),
  start_replication(Name, Source, Target, Options);

start_replication(Name, Source, Target) ->
  start_replication(Name, Source, Target, []).

start_replication(Name, Source, Target, Options) ->
  Config = #{source => Source, target => Target, options => Options},
  case barrel_replicate_manager:start_replication(Name, Config) of
    ok -> {ok, Name};
    Error -> Error
  end.

stop_replication(Name) ->
  barrel_replicate_manager:stop_replication(Name).

delete_replication(Name) ->
  barrel_replicate_manager:delete_replication(Name).

replication_info(Name) ->
  case barrel_replicate_manager:where(Name) of
    Pid when is_pid(Pid) -> barrel_replicate:info(Pid);
    undefined -> {error, not_found}
  end.
