%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Feb 2017 08:53
%%%-------------------------------------------------------------------
-module(barrel_peer).
-author("benoitc").

%% API
-export([
  create_database/1,
  delete_database/1,
  database_names/1,
  database_infos/1,
  connect/1,
  get/3,
  put/3,
  post/3,
  delete/4,
  fold_by_id/4,
  fold_by_path/5,
  changes_since/5
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

-type conn() :: term().
-type docid() :: binary().
-type rev() :: binary().

-type revid() :: binary().

-type revinfo() :: #{
id := revid(),
parent := revid(),
deleted => boolean()
}.

-type revtree() :: #{ revid() => revinfo() }.

-type read_option() ::  rev() | {history, true | false}.

-type write_options() :: list().

-type db_infos() :: #{}.

-type doc() :: term().

-type read_options() :: [read_option()].

%% TODO: to define
-type fold_options() :: list().

-type change() :: #{}.

-export_type([
  conn/0,
  docid/0,
  rev/0,
  db_infos/0,
  doc/0,
  read_option/0, read_options/0,
  write_options/0,
  fold_options/0,
  revid/0,
  revtree/0,
  change/0
]).

create_database(Url) ->
  barrel_httpc:create_database(Url).

delete_database(Url) ->
  barrel_httpc:delete_database(Url).

database_names(Url) ->
  barrel_httpc:database_names(Url).

%% @doc get database infos
-spec database_infos(Url) -> Res when
  Url::binary(),
  Res::db_infos().
database_infos(Url) ->
  barrel_httpc:database_infos(Url).

connect(Url) ->
  barrel_httpc:connect(Url).

%% @doc retrieve a document by its key
-spec get(Conn, DocId, Options) -> Res when
  Conn::conn(),
  DocId :: docid(),
  Options :: read_options(),
  Doc :: doc(),
  Res :: {ok, Doc} | {error, not_found} | {error, any()}.
get(Conn, DocId, Options) ->
  barrel_httpc:get(Conn, DocId, Options).

%% @doc create or update a document. Return the new created revision
%% with the docid or a conflict.
-spec put(Conn, Doc, Options) -> Res when
  Conn::conn(),
  Doc :: doc(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict} | {error, any()}.
put(Conn, Doc, Options) ->
  barrel_httpc:put(Conn, Doc, Options).

%% @doc delete a document
-spec delete(Conn, DocId, RevId, Options) -> Res when
  Conn::conn(),
  DocId :: docid(),
  RevId :: rev(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict} | {error, any()}.
delete(Conn, DocId, RevId, Options) ->
  barrel_httpc:delete(Conn, DocId, RevId, Options).

%% @doc create a document . Like put but only create a document without updating the old one.
%% A doc shouldn't have revision. Optionally the document ID can be set in the doc.
-spec post(Conn, Doc, Options) -> Res when
  Conn::conn(),
  Doc :: doc(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict} | {error, any()}.
post(Conn, Doc, Options) ->
  barrel_httpc:post(Conn, Doc, Options).

%% @doc fold all docs by Id
-spec fold_by_id(Conn, Fun, AccIn, Options) -> AccOut | Error when
  Conn::conn(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((Doc :: doc(), Acc1 :: any()) -> FunRes),
  Options :: fold_options(),
  AccIn :: any(),
  AccOut :: any(),
  Error :: {error, term()}.
fold_by_id(Db, Fun, Acc, Options) ->
  barrel_httpc_fold:fold_by_id(Db, Fun, Acc, Options).

%% @doc fold all docs using a Pointer to the doc
-spec fold_by_path(Conn, Path, Fun, AccIn, Options) -> AccOut | Error when
  Conn::conn(),
  Path::binary(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((Doc :: doc(), Acc1 :: any()) -> FunRes),
  Options :: fold_options(),
  AccIn :: any(),
  AccOut :: any(),
  Error :: {error, term()}.
fold_by_path(Conn, Path, Fun, AccIn, Options) ->
  barrel_httpc_fold:fold_by_path(Conn, Path, Fun, AccIn, Options).

%% @doc fold all changes since last sequence
-spec changes_since(Conn, Since, Fun, AccIn, Opts) -> AccOut when
  Conn::conn(),
  Since :: non_neg_integer(),
  FunRes :: {ok, Acc2::any()} | stop | {stop, Acc2::any()},
  Fun :: fun((Seq :: non_neg_integer(), Change :: change(), Acc :: any()) -> FunRes),
  AccIn :: any(),
  AccOut :: any(),
  Opts :: list().
changes_since(Conn, Since, Fun, AccIn, Opts) ->
  barrel_httpc_fold:changes_since(Conn, Since, Fun, AccIn, Opts).


attach(Db, DocId, AttDescription, Options) ->
  barrel_httpc_attachments:attach(Db, DocId, AttDescription, Options).

attach(Db, DocId, AttDescription, Binary, Options) ->
  barrel_httpc_attachments:attach(Db, DocId, AttDescription, Binary, Options).

get_attachment(Db, DocId, AttId, Options) ->
  barrel_httpc_attachments:get_attachment(Db, DocId, AttId, Options).

get_attachment_binary(Db, DocId, AttId, Options) ->
  barrel_httpc_attachments:get_attachment_binary(Db, DocId, AttId, Options).

replace_attachment(Db, DocId, AttId, AttDescription, Options) ->
  barrel_httpc_attachments:replace_attachment(Db, DocId, AttId, AttDescription, Options).

replace_attachment_binary(Db, DocId, AttId, Binary, Options) ->
  barrel_httpc_attachments:replace_attachment_binary(Db, DocId, AttId, Binary, Options).

delete_attachment(Db, DocId, AttId, Options) ->
  barrel_httpc_attachments:delete_attachment(Db, DocId, AttId, Options).

attachments(Db, DocId, Options) ->
  barrel_httpc_attachments:attachments(Db, DocId, Options).
