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
  changes_since/5,
  revsdiff/3
]).

-export([
  find_by_key/5
]).

%% Database API

-export([
  create_database/2,
  connect_database/2,
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
  start_replication/4,
  stop_replication/1,
  delete_replication/1,
  replication_info/1
]).


-type dbname() :: binary().
-type store() :: atom().
-type conn() :: map().

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
  write_options/0,
  revid/0,
  revinfo/0,
  revtree/0,
  docinfo/0
]).


%% @doc create a database in the store. Return true if it has been created or false if it's already existed.
-spec create_database(Store, DbName) -> Res when
  Store :: store(),
  DbName :: dbname(),
  Res :: {Created:: boolean(), Conn::conn()} | {error, any()}.
create_database(Store, Name) ->
  barrel_db:start(Name, Store, [{create_if_missing, true}]).

%% @doc connect to a database in a store.
-spec connect_database(Store, DbName) -> Res when
  Store :: store(),
  DbName :: dbname(),
  Res :: {_, conn()} | {error, not_found} | {error, any()}.
connect_database(Store, Name) ->
  case barrel_db:start(Name, Store, []) of
    {error, _Reason} = Error -> Error;
    {OK, Conn} when is_boolean(OK) -> {ok, Conn}
  end.

%% @doc close a database
-spec close_database(Conn::conn()) -> ok.
close_database(Conn) ->
  barrel_db:stop(Conn).

%% @doc delete a database
-spec delete_database(Name :: dbname()) -> ok.
delete_database(Conn) ->
  barrel_db:clean(Conn).

-spec database_infos(Conn::conn()) ->
  {ok, DbInfos::db_infos()} | {error, term()}.
database_infos(Conn) ->
  barrel_db:infos(Conn).


%% @doc Returns a list of database names for a store.
-spec database_names(Store::store()) -> [Conn::conn()].
database_names(Store) ->
  barrel_store:all_dbs(Store).


%% Database API.

%% @doc retrieve a document by its key
-spec get(Conn, DocId, Options) -> Res when
  Conn::conn(),
  DocId :: docid(),
  Options :: read_options(),
  Doc :: doc(),
  Res :: {ok, Doc} | {error, not_found} | {error, any()}.
get(Conn, DocId, Options) ->
  barrel_db:get(Conn, DocId, Options).


%% @doc create or update a document. Return the new created revision
%% with the docid or a conflict.
-spec put(Conn, DocId, Body, Options) -> Res when
  Conn::conn(),
  DocId :: docid(),
  Body :: doc(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict()} | {error, any()}.
put(Conn, DocId, Body, Options) ->
  barrel_db:put(Conn, DocId, Body, Options).

%% @doc insert a specific revision to a a document. Useful for the replication.
%% It takes the document id, the doc to edit and the revision history (list of ancestors).
-spec put_rev(Conn, DocId, Body, History, Options) -> Res when
  Conn::conn(),
  DocId :: docid(),
  Body :: doc(),
  History :: [rev()],
  Options :: write_options(),
  Res :: ok | {error, conflict()} | {error, any()}.
put_rev(Conn, DocId, Body, History, Options) ->
  barrel_db:put_rev(Conn, DocId, Body, History, Options).

%% @doc delete a document
-spec delete(Conn, DocId, RevId, Options) -> Res when
  Conn::conn(),
  DocId :: docid(),
  RevId :: rev(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict()} | {error, any()}.
delete(Conn, DocId, RevId, Options) ->
  barrel_db:delete(Conn, DocId, RevId, Options).

%% @doc create a document . Like put but only create a document without updating the old one.
%% A doc shouldn't have revision. Optionally the document ID can be set in the doc.
-spec post(Conn, Doc, Options) -> Res when
  Conn::conn(),
  Doc :: doc(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict()} | {error, any()}.
post(Conn, Doc, Options) ->
  barrel_db:post(Conn, Doc, Options).

%% @doc fold all docs by Id
-spec fold_by_id(Conn, Fun, AccIn, Options) -> AccOut | Error when
  Conn::conn(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((DocId :: docid(), DocInfo :: docinfo(), Doc :: doc(), Acc1 :: any()) -> FunRes),
  Options :: fold_options(),
  AccIn :: any(),
  AccOut :: any(),
  Error :: {error, term()}.
fold_by_id(Conn, Fun, Acc, Options) ->
  barrel_db:fold_by_id(Conn, Fun, Acc, Options).

%% @doc fold all changes since last sequence
-spec changes_since(Conn, Since, Fun, AccIn) -> AccOut when
  Conn::conn(),
  Since :: non_neg_integer(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((Seq :: non_neg_integer(), Change :: change(), Acc :: any()) -> FunRes),
  AccIn :: any(),
  AccOut :: any().
changes_since(Conn, Since, Fun, Acc) ->
  barrel_db:changes_since(Conn, Since, Fun, Acc, []).

%% @doc fold all changes since last sequence
-spec changes_since(Conn, Since, Fun, AccIn, Opts) -> AccOut when
  Conn::conn(),
  Since :: non_neg_integer(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((Seq :: non_neg_integer(), Change :: change(), Acc :: any()) -> FunRes),
  AccIn :: any(),
  AccOut :: any(),
  Opts :: list().
changes_since(Conn, Since, Fun, Acc, Opts) ->
  barrel_db:changes_since(Conn, Since, Fun, Acc, Opts).

%% @doc find in the index a document by its path
-spec find_by_key(Conn, Path, Fun, AccIn, Options) -> AccOut | Error when
  Conn::conn(),
  Path :: binary(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((DocId :: docid(), Doc :: doc(), Acc1 :: any()) -> FunRes),
  Options :: fold_options(),
  AccIn :: any(),
  AccOut :: any(),
  Error :: {error, term()}.
find_by_key(#{store := Store, id := DbId}, Path, Fun, AccIn, Opts) ->
  barrel_store:find_by_key(Store, DbId, Path, Fun, AccIn, Opts).

%% @doc get all revisions ids that differ in a doc from the list given
-spec revsdiff(Conn, DocId, RevIds) -> Res when
  Conn::conn(),
  DocId :: docid(),
  RevIds :: [revid()],
  Res:: {ok, Missing :: [revid()], PossibleAncestors :: [revid()]}.
revsdiff(Conn, DocId, RevIds) ->
  barrel_db:revsdiff(Conn, DocId, RevIds).



attach(Conn, DocId, AttDescription, Options) ->
  barrel_attachments:attach(Conn, DocId, AttDescription, Options).

attach(Conn, DocId, AttDescription, Binary, Options) ->
  barrel_attachments:attach(Conn, DocId, AttDescription, Binary, Options).

get_attachment(Conn, DocId, AttId, Options) ->
  barrel_attachments:get_attachment(Conn, DocId, AttId, Options).

get_attachment_binary(Conn, DocId, AttId, Options) ->
  barrel_attachments:get_attachment_binary(Conn, DocId, AttId, Options).

replace_attachment(Conn, DocId, AttId, AttDescription, Options) ->
  barrel_attachments:replace_attachment(Conn, DocId, AttId, AttDescription, Options).

replace_attachment_binary(Conn, DocId, AttId, Binary, Options) ->
  barrel_attachments:replace_attachment_binary(Conn, DocId, AttId, Binary, Options).

delete_attachment(Conn, DocId, AttId, Options) ->
  barrel_attachments:delete_attachment(Conn, DocId, AttId, Options).

attachments(Conn, DocId, Options) ->
  barrel_attachments:attachments(Conn, DocId, Options).



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
