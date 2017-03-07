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
  create_database/2,
  delete_database/1,
  database_names/1,
  database_infos/1,
  connect/1,
  get/3,
  put/3,
  put/4,
  post/3,
  post/4,
  delete/3,
  update_with/4,
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

-type doc() :: #{}.
-type meta() :: #{}.

-type read_options() :: [read_option()].

%% TODO: to define
-type fold_options() :: list().

%% a change is under the following form
%% #{
%%   <<"id">> := binary(),  % id of the document updated
%%   <<"seq">> := non_neg_integer(), % sequence of the change
%%   <<"changes">> => [revid(], % revision id of the change or the full history if history is true (from last to first),
%%   <<"deleted">> => true | false % present if deleted
%%}
-type change() :: #{ binary() => any() }.

-type attachment() :: {atom(), any()}.

-export_type([
  conn/0,
  docid/0,
  rev/0,
  db_infos/0,
  doc/0,
  meta/0,
  read_option/0, read_options/0,
  write_options/0,
  fold_options/0,
  revid/0,
  revtree/0,
  change/0
]).

%% @doc create a database from its URL
-spec create_database(DbUrl) -> Res when
  DbUrl :: binary(),
  Res :: {ok, binary()} | {error, any()}.
create_database(Url) ->
  barrel_httpc:create_database(Url).

%% @doc create a database with a configuration
%%
%% Example of config:
%% #{ <<"database_id'>> => << "DbName">>, <<"index_mode">> => <<"consistent">> }
%%
%% Index Mode can  be : <<"consistent">> | <<"lazy">>.
-spec create_database(NodeUrl, Config) -> Res when
  NodeUrl :: binary(),
  DbUrl :: binary(),
  Config :: #{},
  Res :: {ok, DbUrl} | {error, any()}.
create_database(Url, Config) ->
  barrel_httpc:create_database(Url, Config).

%% @doc delete a database from its URL
-spec delete_database(DbUrl) -> Res when
  DbUrl :: binary(),
  Res :: ok | {error, any()}.
delete_database(Url) ->
  barrel_httpc:delete_database(Url).

%% @doc get all database names on the node
-spec database_names(NodeUrl) -> Res when
  NodeUrl :: binary(),
  DbName :: binary(),
  Res :: [DbName] | {error, any()}.
database_names(Url) ->
  barrel_httpc:database_names(Url).

%% @doc get database infos
-spec database_infos(Url) -> Res when
  Url::binary(),
  Res::db_infos().
database_infos(Url) ->
  barrel_httpc:database_infos(Url).

%% @doc connect to a database from its URL.
%% If the database is not found, an error is returned
-spec connect(DbUrl) -> Res when
  DbUrl :: binary(),
  Res :: ok | {error, any()}.
connect(Url) ->
  barrel_httpc:connect(Url).

%% @doc retrieve a document by its key
-spec get(Conn, DocId, Options) -> Res when
  Conn::conn(),
  DocId :: docid(),
  Options :: read_options(),
  Doc :: doc(),
  Meta :: meta(),
  Attachments :: list(),
  Res :: {ok, Doc, Meta} | {ok, Doc, Meta, Attachments}  | {error, not_found} | {error, any()}.
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

%% @doc update a document with attachments
-spec put(Conn, Doc, Attachments, Options) -> Res when
    Conn::conn(),
    Doc :: doc(),
    Attachments :: [attachment()],
    Options :: write_options(),
    Res :: {ok, docid(), rev()} | {error, conflict} | {error, any()}.
put(Conn, Doc, Attachments, Options) when is_list(Attachments) ->
  barrel_httpc:put(Conn, Doc, Attachments, Options).

%% @doc delete a document
-spec delete(Conn, DocId, Options) -> Res when
  Conn::conn(),
  DocId :: docid(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict} | {error, any()}.
delete(Conn, DocId, Options) ->
  barrel_httpc:delete(Conn, DocId, Options).

%% @doc create a document . Like put but only create a document without updating the old one.
%% A doc shouldn't have revision. Optionally the document ID can be set in the doc.
-spec post(Conn, Doc, Options) -> Res when
  Conn::conn(),
  Doc :: doc(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict} | {error, any()}.
post(Conn, Doc, Options) ->
  barrel_httpc:post(Conn, Doc, Options).

%% @doc create a document with attachments.
-spec post(Conn, Doc, Attachments, Options) -> Res when
    Conn::conn(),
    Doc :: doc(),
    Attachments :: [attachment()],
    Options :: write_options(),
    Res :: {ok, docid(), rev()} | {error, conflict} | {error, any()}.
post(Conn, Doc, Attachments, Options) when is_list(Attachments) ->
  barrel_httpc:post(Conn, Doc, Attachments, Options).

%% Atomically modifies the a document, this function takes the docId and pass the Doc and its attachments to the
%% callback.
-spec update_with(Conn, DocId, Fun, Options) -> Res when
  Conn::conn(),
  DocId :: docid(),
  Fun :: fun((Doc :: doc() | nil, Attachments :: list()) -> UpdatedDoc :: doc() | {UpdatedDoc :: doc(),
                                                                                   UpdatedAttachments :: list()} ),
  Options :: read_options(),
  Res :: {ok, docid(), rev()}  | {error, any()}.
update_with(Conn, DocId, Fun, Options) ->
  barrel_httpc:update_with(Conn, DocId, Fun, Options).

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
%% a change given to the fold function is under the following form
%% #{
%%   <<"id">> := binary(),  % id of the document updated
%%   <<"seq">> := non_neg_integer(), % sequence of the change
%%   <<"changes">> => [revid(], % revision id of the change or the full history if history is true (from last to first),
%%   <<"deleted">> => true | false % present if deleted
%%}
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
