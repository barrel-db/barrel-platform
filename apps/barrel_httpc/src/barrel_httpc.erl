%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jan 2017 12:35
%%%-------------------------------------------------------------------
-module(barrel_httpc).
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
  post/3,
  delete/3,
  put_rev/5,
  fold_by_id/4,
  fold_by_path/5,
  changes_since/5,
  revsdiff/2,
  revsdiff/3
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
  get_system_doc/2,
  put_system_doc/3,
  delete_system_doc/2
]).

-include_lib("hackney/include/hackney_lib.hrl").


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

-type change() :: #{ binary() => any() }.

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


%% @doc create a database from its URL
-spec create_database(DbUrl) -> Res when
  DbUrl :: binary(),
  Res :: ok | {error, any()}.
create_database(Url0) ->
  {Url1, DbName} = name_from_url(Url0),
  DbObj = jsx:encode(#{ <<"database_id">> => DbName }),
  case hackney:request(<<"POST">>, Url1, [], DbObj, [with_body]) of
    {ok, 201, _, _} -> ok;
    Error -> Error
  end.

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
create_database(Url0, Config) ->
  Url1 = hackney_url: make_url(Url0, <<"dbs">>, []),
  DbObj = jsx:encode(Config),
  case hackney:request(<<"POST">>, Url1, [], DbObj, [with_body]) of
    {ok, 201, _, Obj} ->
      #{ <<"database_id">> := DbId} = jsx:decode(Obj, [return_maps]),
      DbUrl = hackney_url: make_url(Url0, [<<"dbs">>, DbId], []),
      {ok, DbUrl};
    Error -> Error
  end.

%% @doc delete a database from its URL
-spec delete_database(DbUrl) -> Res when
  DbUrl :: binary(),
  Res :: ok | {error, any()}.
delete_database(Url) ->
  case hackney:request(<<"DELETE">>, Url, [], <<>>, [with_body]) of
    {ok, 200, _, _} -> ok;
    Error -> Error
  end.

%% @doc get all database names on the node
-spec database_names(NodeUrl) -> Res when
  NodeUrl :: binary(),
  DbName :: binary(),
  Res :: [DbName] | {error, any()}.
database_names(Url0) ->
  Url1 = hackney_url: make_url(Url0, <<"dbs">>, []),
  case hackney:request(<<"GET">>, Url1, [], <<>>, [with_body]) of
    {ok, 200, _, JsonBody} -> jsx:decode(JsonBody);
    Error -> Error
  end.

%% @doc get database infos
-spec database_infos(Url) -> Res when
  Url::binary(),
  Res::db_infos().
database_infos(Url) ->
  case hackney:request(<<"GET">>, Url, [], <<>>, [with_body]) of
    {ok, 200, _, JsonBody} -> jsx:decode(JsonBody, [return_maps]);
    Error -> Error
  end.


%% @doc connect to a database from its URL.
%% If the database is not found, an error is returned
-spec connect(DbUrl) -> Res when
  DbUrl :: binary(),
  Res :: ok | {error, any()}.
connect(Url) ->
  Max = application:get_env(barrel, max_connections, 12),
  {_, DbName} = name_from_url(Url),
  PoolName = binary_to_atom(DbName, latin1),
  _ = hackney_pool:start_pool(PoolName, [{pool_size, Max}]),
  case hackney:request(<<"HEAD">>, Url, [], <<>>, [{pool, PoolName}]) of
    {ok, 200, _} ->
      {ok, #{ pool => PoolName, db_url => Url}};
    {ok, 404, _} ->
      _ = hackney_pool:stop_pool(PoolName),
      {error, not_found}
  end.

%% @doc retrieve a document by its key
-spec get(Conn, DocId, Options) -> Res when
  Conn::conn(),
  DocId :: docid(),
  Options :: read_options(),
  Doc :: doc(),
  Meta :: meta(),
  Res :: {ok, Doc, Meta} | {error, not_found} | {error, any()}.
get(Conn, DocId, Options0) ->
  {Headers, Options1} = headers(Options0),
  Url = barrel_httpc_lib:make_url(Conn, [<<"docs">>, DocId], Options1),
  case request(Conn, <<"GET">>, Url, Headers, <<>>) of
    {ok, 200, _, JsonBody} ->
      ReqObj = jsx:decode(JsonBody, [return_maps]),
      {Meta, Doc} = maps:take(<<"_meta">>, ReqObj),
      {ok, Doc, Meta};
    Error ->
      Error
  end.

%% @doc create or update a document. Return the new created revision
%% with the docid or a conflict.
-spec put(Conn, Doc, Options) -> Res when
  Conn::conn(),
  Doc :: doc(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict} | {error, any()}.
put(Conn, #{ <<"id">> := DocId } = Doc, Options0) ->
  {Headers, Options1} = headers(Options0),
  Url = barrel_httpc_lib:make_url(Conn, [<<"docs">>, DocId], Options1),
  post_put(Conn, <<"PUT">>, Doc, Url, Headers);
put(_, _, _) -> erlang:error({bad_doc, invalid_docid}).

post_put(Conn, Method, Doc, Url, Headers) ->
  Body = jsx:encode(Doc),
  case request(Conn, Method, Url, Headers, Body) of
    {ok, Status, _, JsonBody}=Resp ->
      case lists:member(Status, [200, 201]) of
        true ->
          Json = jsx:decode(JsonBody, [return_maps]),
          {ok, maps:get(<<"id">>, Json), maps:get(<<"rev">>, Json)};
        false ->
          {error, {bad_response, Resp}}
      end;
    Error -> Error
  end.


%% @doc delete a document
-spec delete(Conn, DocId, Options) -> Res when
  Conn::conn(),
  DocId :: docid(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict} | {error, any()}.
delete(Conn, DocId, Options0) ->
  {Headers, Options1} = headers(Options0),
  Url = barrel_httpc_lib:make_url(Conn, [<<"docs">>, DocId], Options1),
  case request(Conn, <<"DELETE">>, Url, Headers, <<>>) of
    {ok, Status, _, JsonBody}=Resp ->
      case lists:member(Status, [200, 201]) of
        true ->
          Json = jsx:decode(JsonBody, [return_maps]),
          {ok, maps:get(<<"id">>, Json), maps:get(<<"rev">>, Json)};
        false ->
          {error, {bad_response, Resp}}
      end;
    Error -> Error
  end.

headers(Options) ->
  case proplists:get_value(rev, Options) of
    undefined ->
      {[{<<"Content-Type">>, <<"application/json">>}], Options};
    Rev ->
      Hdrs = [{<<"Content-Type">>, <<"application/json">>},
              {<<"ETag">>, Rev}],
      {Hdrs, proplists:delete(rev, Options)}
  end.


%% @doc create a document . Like put but only create a document without updating the old one.
%% A doc shouldn't have revision. Optionally the document ID can be set in the doc.
-spec post(Conn, Doc, Options) -> Res when
  Conn::conn(),
  Doc :: doc(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict} | {error, any()}.
post(Conn, Doc, Options0) ->
  DocWithId = case maps:find(<<"id">>, Doc) of
                {ok, _Id} -> Doc;
                error ->
                  Id = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
                  Doc#{<<"id">> => Id}
              end,
  {Headers, Options1} = headers(Options0),
  Url = barrel_httpc_lib:make_url(Conn, [<<"docs">>], Options1),
  post_put(Conn, <<"POST">>, DocWithId, Url, Headers).


%% @doc insert a specific revision to a a document. Useful for the replication.
%% It takes the document id, the doc to edit and the revision history (list of ancestors).
-spec put_rev(Conn, Doc, History, Deleted, Options) -> Res when
  Conn::conn(),
  Doc :: doc(),
  History :: [rev()],
  Deleted :: true | false,
  Options :: write_options(),
  Res ::  {ok, docid(), rev()} | {error, conflict} | {error, any()}.
put_rev(Conn, #{ <<"id">> := DocId } = Doc, History, Deleted, _Options) ->
  Req =
    #{
      <<"document">> => Doc,
      <<"history">> => History,
      <<"deleted">> => Deleted
    },
  Url = barrel_httpc_lib:make_url(Conn, [<<"docs">>, DocId], [{<<"edit">>, <<"true">>}]),
  Body = jsx:encode(Req),
  case request(Conn, <<"PUT">>, Url, [], Body) of
    {ok, Status, _, JsonBody}=Resp ->
      case lists:member(Status, [200, 201]) of
        true ->
          Json = jsx:decode(JsonBody, [return_maps]),
          {ok, maps:get(<<"id">>, Json), maps:get(<<"rev">>, Json)};
        false ->
          {error, {bad_response, Resp}}
      end;
    Error -> Error
  end;
put_rev(_, _, _, _, _) -> erlang:error({bad_doc, invalid_docid}).

%% @doc get all revisions ids that differ in a doc from the list given
-spec revsdiff(Conn, DocId, RevIds) -> Res when
  Conn::conn(),
  DocId :: docid(),
  RevIds :: [revid()],
  Res:: {ok, Missing :: [revid()], PossibleAncestors :: [revid()]}.
revsdiff(Conn, DocId, RevIds) ->
  case revsdiff(Conn, #{ DocId => RevIds }) of
    {ok, Res} ->
      #{ DocId := #{ <<"missing">> := Missing,
                     <<"possible_ancestors">> := PossibleAncestors}} = Res,
      {ok, Missing, PossibleAncestors};
    Error ->
      Error
  end.

%% @doc get all revisions ids that differ in docs from the list given.
%% Docs are passed in the object where eack Key is the document ID and
%% the value the list of the revisions to compare.
-spec revsdiff(Conn, Docs) -> Res when
  Conn :: conn(),
  Docs :: #{ docid() => [revid()]},
  Res :: {ok, #{ docid() => [revid()] }} | {error, any()}.
revsdiff(Conn, Docs) when is_map(Docs) ->
  Url = barrel_httpc_lib:make_url(Conn, [<<"revsdiff">>], []),
  Body = jsx:encode(Docs),
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  case request(Conn, <<"POST">>, Url, Headers, Body) of
    {ok, 200, _, JsonBody} ->
      Result = jsx:decode(JsonBody, [return_maps]),
      {ok, Result};
    Error -> Error
    end;
revsdiff(_, _) ->  erlang:error(badarg).

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
  Fun :: fun((Change :: change(), Acc :: any()) -> FunRes),
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


get_system_doc(Conn, DocId) ->
  Url = barrel_httpc_lib:make_url(Conn, [<<"system">>, DocId], []),
  case request(Conn, <<"GET">>, Url) of
    {ok, 200, _, JsonBody} -> {ok, jsx:decode(JsonBody, [return_maps])};
    Error -> Error
  end.

put_system_doc(Conn, DocId, Doc) when is_map(Doc) ->
  Url = barrel_httpc_lib:make_url(Conn, [<<"system">>, DocId], []),
  Body = jsx:encode(Doc),
  case request(Conn, <<"PUT">>, Url, [], Body) of
    {ok, Status, _, _JsonBody}=Resp ->
      case lists:member(Status, [200, 201]) of
        true -> ok;
        false -> {error, {bad_response, Resp}}
      end;
    Error -> Error
  end;
put_system_doc(_, _, _) -> erlang:error(bad_doc).

delete_system_doc(Conn, DocId) ->
  Url = barrel_httpc_lib:make_url(Conn, [<<"system">>, DocId], []),
  case request(Conn, <<"DELETE">>, Url) of
    {ok, 200, _, _JsonBody} -> ok;
    Error -> Error
  end.


%% internal

name_from_url(Url) ->
  #hackney_url{path=Path} = Parsed = hackney_url:parse_url(Url),
  Parts = binary:split(Path, <<"/">>, [global]),
  Path2 = barrel_httpc_lib:binary_join(
    lists:sublist(Parts, length(Parts) -1),
    <<"/">>
  ),
  Url2 = hackney_url:unparse_url(Parsed#hackney_url{path=Path2}),
  {Url2, lists:last(Parts)}.

make_headers(Headers) ->
  case proplists:get_value(<<"Accept">>, Headers) of
    undefined ->
      [{<<"Accept">>, <<"application/json, */*;q=0.9">>} | Headers];
    _ ->
      Headers
  end.

request(Conn, Method, Url) ->
  request(Conn, Method, Url, [], <<>>).

request(#{pool := Pool}, Method, Url, Headers0, Body) ->
  Headers1 = make_headers(Headers0),
  Options = [with_body, {pool, Pool}],
  Resp = hackney:request(Method, Url, Headers1, Body, Options),
  db_resp(Resp).


db_resp({ok, 401, _}) ->
  {error, not_authenticated};
db_resp({ok, 403, _}) ->
  {error, forbidden};
db_resp({ok, 404, _}) ->
  {error, not_found};
db_resp({ok, 409, _}) ->
  {error, conflict};
db_resp({ok, 412, _}) ->
  {error, precondition_failed};
db_resp({ok, Status, _}) when Status >= 500 ->
  {error, server_error};
db_resp({ok, _, _}=Resp) ->
  Resp;
db_resp({ok, 401, _, _}) ->
  {error, not_authenticated};
db_resp({ok, 403, _, _}) ->
  {error, forbidden};
db_resp({ok, 404, _, _}) ->
  {error, not_found};
db_resp({ok, 409, _, _}) ->
  {error, conflict};
db_resp({ok, 412, _, _}) ->
  {error, precondition_failed};
db_resp({ok, Status, _, Body}) when Status >= 500 ->
  {error, {server_error, Body}};
db_resp(Resp) ->
  Resp.
