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
  put/4,
  put_rev/5,
  get/3,
  delete/4,
  post/3,
  fold_by_id/4,
  changes_since/4,
  revsdiff/3
]).

%% Database API

-export([
  open_database/3,
  close_database/1,
  delete_database/1,
  database_names/1,
  database_infos/1
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
  stop_replication/1,
  replication_info/1
]).


-type dbname() :: binary().
-type store() :: atom().
-type db_options() :: [{create_if_missing, boolean()}].

%% TODO: to define
-type db_infos() :: list().

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


-export_type([
  dbname/0,
  store/0,
  db_options/0,
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



%% @doc open or create a database in a store.
-spec open_database(Name, Store, Options) -> Res when
  Name :: dbname(),
  Store :: store(),
  Options :: db_options(),
  Res :: ok | {error, not_found} | {error, any()}.
open_database(Name, Store, Options) ->
  barrel_db:start(Name, Store, Options).

%% @doc close a database
-spec close_database(Name::dbname()) -> ok.
close_database(Name) ->
  barrel_db:stop(Name).

%% @doc delete a database
-spec delete_database(Name :: dbname()) -> ok.
delete_database(Name) ->
  barrel_db:clean(Name).

-spec database_infos(Name::dbname()) ->
  {ok, DbInfos::db_infos()} | {error, term()}.
database_infos(Name) ->
  barrel_db:infos(Name).


%% @doc Returns a list of database names for a store.
-spec database_names(Store::store()) -> [Name::dbname()].
database_names(Store) ->
  barrel_store:all_dbs(Store).


%% Database API.

%% @doc retrieve a document by its key
-spec get(Db, DocId, Options) -> Res when
  Db :: dbname(),
  DocId :: docid(),
  Options :: read_options(),
  Doc :: doc,
  Res :: {ok, Doc} | {error, not_found} | {error, any()}.
get(Db, DocId, Options) ->
  barrel_db:get(Db, DocId, Options).


%% @doc create or update a document. Return the new created revision
%% with the docid or a conflict.
-spec put(Db, DocId, Body, Options) -> Res when
  Db :: dbname(),
  DocId :: docid(),
  Body :: doc(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict()} | {error, any()}.
put(Db, DocId, Body, Options) ->
  barrel_db:put(Db, DocId, Body, Options).

%% @doc insert a specific revision to a a document. Useful for the replication.
%% It takes the document id, the doc to edit and the revision history (list of ancestors).
-spec put_rev(Db, DocId, Body, History, Options) -> Res when
  Db :: dbname(),
  DocId :: docid(),
  Body :: doc(),
  History :: [rev()],
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict()} | {error, any()}.
put_rev(Db, DocId, Body, History, Options) ->
  barrel_db:put_rev(Db, DocId, Body, History, Options).

%% @doc delete a document
-spec delete(Db, DocId, RevId, Options) -> Res when
  Db :: dbname(),
  DocId :: docid(),
  RevId :: rev(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict()} | {error, any()}.
delete(Db, DocId, RevId, Options) ->
  barrel_db:delete(Db, DocId, RevId, Options).

%% @doc create a document . Like put but only create a document without updating the old one.
%% A doc shouldn't have revision. Optionally the document ID can be set in the doc.
-spec post(Db, Doc, Options) -> Res when
  Db :: dbname(),
  Doc :: doc(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict()} | {error, any()}.
post(Db, Doc, Options) ->
  barrel_db:post(Db, Doc, Options).

%% @doc fold all docs by Id
-spec fold_by_id(Db, Fun, AccIn, Options) -> AccOut | Error when
  Db :: dbname(),
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
  Db :: dbname(),
  Since :: non_neg_integer(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((Seq :: non_neg_integer(), DocInfo :: docinfo(), Doc :: doc, Acc :: any()) -> FunRes),
  AccIn :: any(),
  AccOut :: any().
changes_since(Db, Since, Fun, Acc) ->
  barrel_db:changes_since(Db, Since, Fun, Acc).


%% @doc get all revisions ids that differ in a doc from the list given
-spec revsdiff(Db, DocId, RevIds) -> Res when
  Db :: dbname(),
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

start_replication(Source, Target) -> start_replication(Source, Target, []).

start_replication(Source, Target, Options) ->
  RepId = barrel_replicate:repid(Source, Target),
  case supervisor:start_child(barrel_replicate_sup, [RepId, Source, Target, Options]) of
    {ok, _Pid} -> {ok, RepId};
    {error, {already_started, _Pid}} -> {ok, RepId};
    Error -> Error
  end.

stop_replication(RepId) ->
  case gproc:where(barrel_replicate:replication_key(RepId)) of
    undefined -> ok;
    Pid when is_pid(Pid) ->
      supervisor:terminate_child(barrel_replicate_sup, Pid)
  end.
  
replication_info(RepId) ->
  case gproc:where(barrel_replicate:replication_key(RepId)) of
    undefined -> {error, not_found};
    Pid -> barrel_replicate:info(Pid)
  end.
