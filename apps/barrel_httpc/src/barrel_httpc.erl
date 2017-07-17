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
  multi_get/5,
  put/3,
  put/4,
  post/3,
  post/4,
  delete/3,
  update_with/4,
  put_rev/5,
  write_batch/3,
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

-export([
  start_changes_listener/2,
  stop_changes_listener/1,
  get_changes/1
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

-type db_infos() :: map().

-type doc() :: map().
-type meta() :: map().

-type read_options() :: [read_option()].

%% TODO: to define
-type fold_options() :: list().

-type change() :: #{ binary() => any() }.

-type attachment() :: #{binary() => any()}.

-type attid() :: binary().

-type att_description() :: map().

-type batch_options() :: [
{async, boolean()}
].

-type batch_results() :: [
  {ok, docid(), revid()}
  | {error, not_found}
  | {error, {conflict, doc_exists}}
  | {error, {conflict, revision_conflict}}
  | {error, any()}
].

-type batch_op() ::
  {put, Doc :: barrel:doc()} |
  {put, Doc :: barrel:doc(), Rev :: barrel:revid()} |
  {put, Doc :: barrel:doc(), Attachments :: [attachment()]} |
  {put, Doc :: barrel:doc(), Attachments :: [attachment()], Rev :: barrel:revid()} |
  {post, Doc :: barrel:doc()} |
  {post, Doc :: barrel:doc(), IsUpsert :: boolean()} |
  {post, Doc :: barrel:doc(), Attachments :: [attachment()]} |
  {post, Doc :: barrel:doc(), Attachments :: [attachment()], IsUpsert :: boolean()} |
  {delete, DocId :: barrel:docid(), Rev :: barrel:revid()} |
  {put_rev, Doc :: barrel:doc(), History :: list(), Deleted :: boolean()} |
  {put_rev, Doc :: barrel:doc(), Attachments :: [attachment()], History :: list(), Deleted :: boolean()}.

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
  change/0,
  batch_options/0,
  batch_results/0,
  batch_op/0,
  attid/0,
  att_description/0,
  attachment/0
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
  Res :: {ok, conn()} | {error, any()}.
connect(Url) ->
  Max = application:get_env(barrel_httpc, max_connections, 12),
  {_, DbName} = name_from_url(Url),
  PoolName = binary_to_atom(DbName, latin1),
  _ = hackney_pool:start_pool(PoolName, [{pool_size, Max}]),
  case hackney:request(<<"HEAD">>, Url, [], <<>>, [{pool, PoolName}]) of
    {ok, 200, _} ->
      {ok, #{ pool => PoolName, db_url => Url}};
    {ok, 404, _} ->
      _ = hackney_pool:stop_pool(PoolName),
      {error, not_found};
    Error ->
      Error
  end.

%% @doc retrieve a document by its key
-spec get(Conn, DocId, Options) -> Res when
  Conn::conn(),
  DocId :: docid(),
  Options :: read_options(),
  Doc :: doc(),
  Meta :: meta(),
  Attachments :: [attachment()],
  Res :: {ok, Doc, Meta} | {ok, Doc, Attachments, Meta} | {error, not_found} | {error, any()}.
get(Conn, DocId, Options0) when is_binary(DocId)->
  {WithAttachment, Options1} = maybe_with_attachments(Options0),
  {Headers, Options2} = headers(Options1),
  Url = barrel_httpc_lib:make_url(Conn, [<<"docs">>, DocId], Options2),
  case request(Conn, <<"GET">>, Url, Headers, <<>>) of
    {ok, 200, RespHeaders, JsonBody} ->
      Doc = jsx:decode(JsonBody, [return_maps]),
      Meta = parse_header(RespHeaders),
      case WithAttachment of
        decoded ->
          {Attachments, DocWithoutAttachment} = maybe_take(<<"_attachments">>, Doc, []),
          DecodedAttachments = decode_attachments(Attachments),
          {ok, DocWithoutAttachment, DecodedAttachments, Meta};
        raw ->
          {ok, Doc, Meta};
        false ->
          {_, DocWithoutAttachment} = maybe_take(<<"_attachments">>, Doc, []),
          {ok, DocWithoutAttachment, Meta}
      end;
    Error ->
      Error
  end.

%% @doc retrieve several documents
-spec multi_get(Conn, Fun, AccIn, DocIds, Options) -> AccOut when
  Conn::conn(),
  Fun :: fun( (doc(), meta(), any()) -> any()),
  AccIn :: any(),
  DocIds :: [docid()],
  Options :: read_options(),
  AccOut :: any().
multi_get(_Db, _UserFun, AccIn, [], _Options) -> AccIn;
multi_get(Db, UserFun, AccIn, DocIds, Options) ->
  WrapperFun = fun(Doc, Meta, Acc) -> {ok, UserFun(Doc, Meta, Acc)} end,
  {ok, Res} = barrel_httpc_fold:fold_by_id(Db, WrapperFun, AccIn, [{docids, DocIds} | Options]),
  Res.

parse_header(HeadersList) ->
  Headers = hackney_headers_new:from_list(HeadersList),
  maybe_add_revisions(
    maybe_add_deleted(
      maybe_add_rev(#{}, Headers),
      Headers
    ),
    Headers
  ).

maybe_add_rev(Meta, Headers) ->
  case hackney_headers_new:get_value(<<"etag">>, Headers) of
    undefined -> Meta;
    ETag -> Meta#{ <<"rev">> => ETag}
  end.

maybe_add_deleted(Meta, Headers) ->
  case hackney_headers_new:get_value(<<"x-barrel-deleted">>, Headers) of
    <<"true">> -> Meta#{ <<"deleted">> => true };
    _ -> Meta
  end.

maybe_add_revisions(Meta, Headers) ->
  case hackney_headers_new:lookup(<<"x-barrel-revisions-id">>, Headers) of
    [] -> Meta;
    Values ->
      History = lists:flatten([binary:split(V, <<",">>, [global]) || {_, V} <- Values]),
      Meta#{ <<"revisions">> => barrel_doc:encode_revisions(History) }
  end.

maybe_take(Key, Map, Default) when is_map(Map) ->
  case maps:take(Key, Map) of
    {K, M} ->
      {K,M};
    error ->
      {Default, Map}
  end.

maybe_with_attachments(Options) ->
  maybe_with_attachments(Options, false, []).
maybe_with_attachments([], WithAttachment, Acc) ->
  {WithAttachment, lists:reverse(Acc)};
maybe_with_attachments([{attachments, all}|Options],_, Acc) ->
  maybe_with_attachments(Options, decoded, Acc);
maybe_with_attachments([{attachments_parsing, false}|Options],_, Acc) ->
  maybe_with_attachments(Options, raw, Acc);
maybe_with_attachments([H|Options], W, Acc) ->
  maybe_with_attachments(Options, W, [H|Acc]).

%% @doc create or update a document. Return the new created revision
%% with the docid or a conflict.
-spec put(Conn, Doc, Options) -> Res when
  Conn::conn(),
  Doc :: doc(),
  Options :: write_options(),
  Res :: {ok, docid(), rev()} | {error, conflict} | {error, any()} | no_return().
put(Conn, #{ <<"id">> := DocId } = Doc, Options0)  when is_binary(DocId) ->
  {Headers, Options1} = headers(Options0),
  Url = barrel_httpc_lib:make_url(Conn, [<<"docs">>, DocId], Options1),
  Async = proplists:get_value(async, Options1, false),
  post_put(Conn, <<"PUT">>, Doc, Url, Headers, Async);
put(_, _, _) ->
  erlang:error({bad_doc, invalid_docid}).

%% @doc update a document with attachments
-spec put(Conn, Doc, Attachments, Options) -> Res when
    Conn::conn(),
    Doc :: doc(),
    Attachments :: [attachment()],
    Options :: write_options(),
    Res :: {ok, docid(), rev()} | ok | {error, conflict} | {error, any()} | no_return().
put(Conn, Doc, Attachments, Options0) when is_list(Attachments) ->
  case encode_attachments(Doc, Attachments) of
    {ok, DocWithAttachments} ->
      put(Conn, DocWithAttachments, Options0);
    {error, Error} ->
      {error, Error}
  end.

post_put(Conn, Method, Doc, Url, Headers, Async) ->
  Body = jsx:encode(Doc),
  case request(Conn, Method, Url, Headers, Body) of
    {ok, Status, RespHeaders, JsonBody}=Resp ->
      case lists:member(Status, [200, 201, 202]) of
        true when Async =:= true ->
          ok;
        true ->
          Json = jsx:decode(JsonBody, [return_maps]),
          DocId = maps:get(<<"id">>, Json),
          RevId = proplists:get_value(<<"etag">>, RespHeaders),
          {ok, DocId, RevId};
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
delete(Conn, DocId, Options0) when is_binary(DocId) ->
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
      {[{<<"Content-Type">>, <<"application/json">>}], proplists:delete(rev, Options)};
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
  Res :: {ok, docid(), rev()} | ok |{error, conflict} | {error, any()}.
post(Conn, Doc, Options0) ->
  DocWithId = case maps:find(<<"id">>, Doc) of
                {ok, _Id} -> Doc;
                error ->
                  Id = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
                  Doc#{<<"id">> => Id}
              end,
  {Headers, Options1} = headers(Options0),
  Url = barrel_httpc_lib:make_url(Conn, [<<"docs">>], Options1),
  Async = proplists:get_value(async, Options1, false),
  post_put(Conn, <<"POST">>, DocWithId, Url, Headers, Async).

%% @doc create a document with attachments.
-spec post(Conn, Doc, Attachments, Options) -> Res when
    Conn::conn(),
    Doc :: doc(),
    Attachments :: [attachment()],
    Options :: write_options(),
    Res :: {ok, docid(), rev()} | {error, conflict} | {error, any()}.
post(Conn, Doc, Attachments, Options0) when is_list(Attachments) ->
  case encode_attachments(Doc, Attachments) of
    {ok, DocWithAttachments} ->
      post(Conn, DocWithAttachments, Options0);
    {error, Error} ->
      {error, Error}
  end.


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
   case do_update_with(Conn, DocId, Fun, Options) of
     {ok, _, _} = OK -> OK;
     {error, {conflict, _}} -> update_with(Conn, DocId, Fun, Options);
     Error -> Error
   end.

do_update_with(Conn, DocId, Fun, Options) ->
  case barrel_httpc:get(Conn, DocId, Options) of
    {ok, _, _} = Res ->
      try_put(Res, Conn, Fun);
    {ok, _, _, _} = Res ->
      try_put(Res, Conn, Fun);
    {error, not_found} ->
      try_post(Conn, Fun);
    Error ->
      Error
  end.

try_put({ok, Doc, Meta}, Conn, Fun) ->
  Rev = maps:get(<<"rev">>, Meta),
  try_put_1(Fun(Doc, []), Rev, Conn);

try_put({ok, Doc, Atts, Meta}, Conn, Fun) ->
  Rev = maps:get(<<"rev">>, Meta),
  try_put_1(Fun(Doc, Atts), Rev, Conn).


try_put_1({Doc, Atts}, Rev, Conn) when is_map(Doc), is_list(Atts) ->
  barrel_httpc:put(Conn, Doc, Atts, [{rev, Rev}]);
try_put_1(Doc, Rev, Conn) when is_map(Doc) ->
  barrel_httpc:put(Conn, Doc, [{rev, Rev}]).


try_post(Conn, Fun) ->
  case Fun(nil, []) of
    Doc when is_map(Doc) ->
      barrel_httpc:post(Conn, Doc, []);
    {Doc, Atts} ->
      barrel_httpc:post(Conn, Doc, Atts, [])
  end.

encode_attachments(Doc, Attachments) ->
  encode_attachments(Doc, Attachments, []).

encode_attachments(Doc, [], []) ->
  {ok, Doc};
encode_attachments(Doc, [], EncodedAttachments) ->
  {ok, Doc#{<<"_attachments">> => lists:reverse(EncodedAttachments)}};
encode_attachments(Doc, [A|Tail], Encoded) ->
  case encode_attachment(A) of
    {ok, E} ->
      encode_attachments(Doc, Tail, [E|Encoded]);
    {error, Error} ->
      {error, Error}
  end.

encode_attachment(#{<<"blob">> := Blob, <<"id">> := Id,
                    <<"content-type">> := <<"application/erlang">>})
  when is_binary(Blob) ->
  {error, {erlang_term_expected, Id}};
encode_attachment(#{<<"blob">> := Blob }=A) when is_binary(Blob) ->
  B64 = base64:encode(Blob),
  ContentType = maps:get(<<"content-type">>, A, <<"application/octet-stream">>),
  {ok, A#{<<"content-type">> => ContentType,
          <<"blob">> := B64,
          <<"content-length">> => byte_size(Blob)}};
encode_attachment(#{<<"blob">> := ErlangTerm}=A) ->
  TermAsBinary = term_to_binary(ErlangTerm),
  B64 = base64:encode(TermAsBinary),
  {ok, A#{<<"blob">> => B64,
          <<"content-type">> => <<"application/erlang">>,
          <<"content-length">> => byte_size(TermAsBinary)}};
encode_attachment(#{<<"link">> := Link}=A) ->
  B64 = base64:encode(Link),
  {ok, A#{<<"link">> := B64}}.



decode_attachments(Attachments) ->
  [decode_attachment(A) || A <- Attachments].

decode_attachment(#{<<"content-type">> := <<"application/erlang">>}=A) ->
  #{<<"blob">> := B64} = A,
  Blob = base64:decode(B64),
  DecodedBlob = binary_to_term(Blob),
  A#{<<"blob">> => DecodedBlob};
decode_attachment(#{<<"blob">> := B64}=A) ->
  Blob = base64:decode(B64),
  A#{<<"blob">> := Blob};
decode_attachment(#{<<"link">> := B64}=A) ->
  Blob = base64:decode(B64),
  A#{<<"link">> := Blob}.

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
    {ok, Status, RespHeaders, JsonBody}=Resp ->
      case lists:member(Status, [200, 201]) of
        true ->
          Json = jsx:decode(JsonBody, [return_maps]),
          RevId = proplists:get_value(<<"etag">>, RespHeaders),
          DocId = maps:get(<<"id">>, Json),
          {ok, DocId, RevId};
        false ->
          {error, {bad_response, Resp}}
      end;
    Error -> Error
  end;
put_rev(_, _, _, _, _) -> erlang:error({bad_doc, invalid_docid}).

%% @doc Apply the specified updates to the database.
%% Note: The batch is not guaranteed to be atomic, atomicity is only guaranteed at the doc level.
-spec write_batch(Conn, Updates, Options) -> Results when
  Conn :: conn(),
  Updates :: [batch_op()],
  Options :: batch_options(),
  Results :: batch_results().
write_batch(Conn, Updates, Options) ->
  Async = proplists:get_value(async, Options, false),
  Headers = [ {<<"Content-Type">>, <<"application/json">>},
              {<<"x-barrel-write-batch">>, <<"true">>},
              {<<"x-barrel-async">>, Async }],
  Url = barrel_httpc_lib:make_url(Conn, [<<"docs">>], []),
  JsonUpdate = [batch_update(Update) || Update <- Updates],
  Body = jsx:encode(#{ <<"updates">> => JsonUpdate }),
  case request(Conn, <<"POST">>, Url, Headers, Body) of
    {ok, Status, _RespHeaders, JsonBody}=Resp ->
      case lists:member(Status, [200]) of
        true ->
          Json = jsx:decode(JsonBody, [return_maps]),
          case maps:find(<<"results">>, Json) of
            {ok, Results} ->
              [ batch_result(Res) || Res <- Results ];
            error ->
              ok
          end;
        false ->
          {error, {bad_response, Resp}}
      end;
    Error -> Error
  end.

batch_update({post, Doc}) when is_map(Doc) ->
  #{ <<"op">> => <<"post">>, <<"doc">> => Doc };
batch_update({post, Doc0, Attachments}) when is_map(Doc0), is_list(Attachments) ->
  {ok, Doc1} = encode_attachments(Doc0, Attachments),
  #{ <<"op">> => <<"post">>, <<"doc">> => Doc1 };
batch_update({post, Doc, IsUpsert}) when is_map(Doc), is_boolean(IsUpsert) ->
  #{ <<"op">> => <<"post">>, <<"doc">> => Doc, <<"is_upsert">> => IsUpsert };
batch_update({post, Doc0, Attachments,  IsUpsert}) when is_map(Doc0), is_list(Attachments), is_boolean(IsUpsert) ->
  {ok, Doc1} = encode_attachments(Doc0, Attachments),
  #{ <<"op">> => <<"post">>, <<"doc">> => Doc1, <<"is_upsert">> => IsUpsert };
batch_update({put, Doc}) when is_map(Doc) ->
  #{ <<"op">> => <<"put">>, <<"doc">> => Doc };
batch_update({put, Doc0, Attachments}) when is_map(Doc0), is_list(Attachments) ->
  {ok, Doc1} = encode_attachments(Doc0, Attachments),
  #{ <<"op">> => <<"put">>, <<"doc">> => Doc1 };
batch_update({put, Doc, Rev}) when is_map(Doc), is_binary(Rev) ->
  #{ <<"op">> => <<"put">>, <<"doc">> => Doc, <<"rev">> => Rev };
batch_update({put, Doc0, Attachments, Rev}) when is_map(Doc0), is_list(Attachments), is_binary(Rev) ->
  {ok, Doc1} = encode_attachments(Doc0, Attachments),
  #{ <<"op">> => <<"put">>, <<"doc">> => Doc1, <<"rev">> => Rev };
batch_update({delete, DocId}) when is_binary(DocId) ->
  #{ <<"op">> => <<"delete">>, <<"id">> => DocId };
batch_update({delete, DocId, Rev}) when is_binary(DocId), is_binary(Rev) ->
  #{ <<"op">> => <<"delete">>, <<"id">> => DocId, <<"rev">> => Rev };
batch_update({put_rev, Doc, History, Deleted}) when is_binary(Doc), is_binary(History), is_boolean(Deleted) ->
  #{ <<"op">> => <<"put_rev">>, <<"doc">> => Doc, <<"history">> => History, <<"deleted">> => Deleted };
batch_update(
  {put_rev, Doc0, Attachments, History, Deleted}
) when is_binary(Doc0), is_list(Attachments), is_binary(History), is_boolean(Deleted) ->
  {ok, Doc1} = encode_attachments(Doc0, Attachments),
  #{ <<"op">> => <<"put_rev">>, <<"doc">> => Doc1, <<"history">> => History, <<"deleted">> => Deleted };
batch_update(_) ->
  erlang:error(badarg).

batch_result(#{ <<"status">> := <<"ok">>, <<"id">> := DocId, <<"rev">> := Rev }) ->
  {ok, DocId, Rev};
batch_result(#{ <<"status">> := <<"error">>, <<"reason">> := <<"not found">> }) ->
  {error, not_found};
batch_result(#{ <<"status">> := <<"error">>, <<"reason">> := Other }) ->
  {error, Other};
batch_result(#{ <<"status">> := <<"conflict">>, <<"reason">> := <<"doc exists">> }) ->
  {error, {conflict, doc_exists}};
batch_result(#{ <<"status">> := <<"conflict">>, <<"reason">> := <<"revision conflict">> }) ->
  {error, {conflict, revision_conflict}}.

%% @doc get all revisions ids that differ in a doc from the list given
-spec revsdiff(Conn, DocId, RevIds) -> Res when
  Conn::conn(),
  DocId :: docid(),
  RevIds :: [revid()],
  Res:: {ok, Missing :: [revid()], PossibleAncestors :: [revid()]} | {error, any()}.
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
  Res :: {ok, #{ docid() => map() }} | {error, any()}.
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


-spec attach(Conn, DocId, AttDescription, Options) -> Res when
    Conn :: conn(),
    DocId :: docid(),
    AttDescription :: att_description(),
    Options :: list(),
    RevId :: revid(),
    Res :: {ok, DocId, RevId} | {error, any()}.
attach(Db, DocId, AttDescription, Options) ->
  barrel_httpc_attachments:attach(Db, DocId, AttDescription, Options).

-spec attach(Conn, DocId, AttDescription, Binary, Options) -> Res when
    Conn :: conn(),
    DocId :: docid(),
    AttDescription :: att_description(),
    Binary :: binary(),
    Options :: list(),
    RevId :: revid(),
    Res :: {ok, DocId, RevId}.
attach(Db, DocId, AttDescription, Binary, Options) ->
  barrel_httpc_attachments:attach(Db, DocId, AttDescription, Binary, Options).

-spec get_attachment(Conn, DocId, AttId, Options) -> Res when
    Conn :: conn(),
    DocId :: docid(),
    AttId :: att_description(),
    Options :: list(),
    AttDescription :: att_description(),
    Res :: {ok, AttDescription}.
get_attachment(Db, DocId, AttId, Options) ->
  barrel_httpc_attachments:get_attachment(Db, DocId, AttId, Options).

-spec get_attachment_binary(Conn, DocId, AttId, Options) -> Res when
    Conn :: conn(),
    DocId :: docid(),
    AttId :: attid(),
    Options :: list(),
    Binary :: binary(),
    Res :: {ok, Binary}.
get_attachment_binary(Db, DocId, AttId, Options) ->
  barrel_httpc_attachments:get_attachment_binary(Db, DocId, AttId, Options).

-spec replace_attachment(Conn, DocId, AttId, AttDescription, Options) -> Res when
    Conn :: conn(),
    DocId :: docid(),
    AttId :: attid(),
    AttDescription :: att_description(),
    Options :: list(),
    RevId :: revid(),
    Res :: {ok, DocId, RevId}.
replace_attachment(Db, DocId, AttId, AttDescription, Options) ->
  barrel_httpc_attachments:replace_attachment(Db, DocId, AttId, AttDescription, Options).

-spec replace_attachment_binary(Conn, DocId, AttId, Binary, Options) -> Res when
    Conn :: conn(),
    DocId :: docid(),
    AttId :: attid(),
    Binary :: binary(),
    Options :: list(),
    RevId :: revid(),
    Res :: {ok, DocId, RevId}.
replace_attachment_binary(Db, DocId, AttId, Binary, Options) ->
  barrel_httpc_attachments:replace_attachment_binary(Db, DocId, AttId, Binary, Options).

-spec delete_attachment(Conn, DocId, AttId, Options) -> Res when
    Conn :: conn(),
    DocId :: docid(),
    AttId :: attid(),
    Options :: list(),
    RevId :: revid(),
    Res :: {ok, DocId, RevId}.
delete_attachment(Db, DocId, AttId, Options) ->
  barrel_httpc_attachments:delete_attachment(Db, DocId, AttId, Options).

-spec attachments(Conn, DocId, Options) -> Res when
    Conn :: conn(),
    DocId :: docid(),
    Options :: list(),
    Res :: [att_description()].
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

%% @doc start a change listener on the database.
%% This function create a process that will listen on the changes feed API.
%% If not callback is given, changes are queued in the process and need
%% to be fetched using the `fetch_changes' function. When a callback is given,
%% a change is passed to the function, no state is kept in the process.
%% a change given to the callback or in the list is under the following form
%% #{
%%   <<"id">> := binary(),  % id of the document updated
%%   <<"seq">> := non_neg_integer(), % sequence of the change
%%   <<"changes">> => [revid(], % revision id of the change or
%%                              % the full history if history is true (from last to first),
%%   <<"deleted">> => true | false % present if deleted
%%}
%%
%% In case the connection is lost or closed, it will retry to connect, at most
%% `max_retry` times (default=5 times), waiting `delay_before_retry` ms between each
%% try (default=500 ms)
-spec start_changes_listener(Conn, ListenerOptions) -> Res when
  Conn :: barrel_httpc:conn(),
  ListenerOptions :: barrel_httpc_changes:listener_options(),
  ListenerPid :: pid(),
  Res :: {ok, ListenerPid} | {error, any()}.
start_changes_listener(Conn, ListenerOptions) ->
  barrel_httpc_changes:start_link(Conn, ListenerOptions).

%% @doc stop a change listener
-spec stop_changes_listener(ListenerPid) -> Res when
  ListenerPid :: pid(),
  Res :: ok.
stop_changes_listener(ListenerPid) ->
  barrel_httpc_changes:stop(ListenerPid).

%% @doc fetch all changes received by a listener à that time.
%% Only useful when no changes callback is given.
%% Otherwise the list will always be empty.
-spec get_changes(ListenerPid) -> Changes when
  ListenerPid :: pid(),
  Changes :: [barrel_httpc:change()].
get_changes(ListenerPid) ->
  barrel_httpc_changes:changes(ListenerPid).


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
