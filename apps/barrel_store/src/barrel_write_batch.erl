%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Mar 2017 22:16
%%%-------------------------------------------------------------------
-module(barrel_write_batch).
-author("benoitc").

%% API
-export([
  new/1,
  put/3,
  post/3,
  delete/3,
  put_rev/4,
  to_buckets/1,
  from_list/2,
  is_batch/1
]).

-include_lib("barrel_common/include/barrel_common.hrl").

-type batch() :: tuple().
-type batch_op() :: {put, Doc :: barrel_local:doc(), Rev :: barrel_local:revid()} |
                    {post, Doc :: barrel_local:doc(), IsUpsert :: boolean()} |
                    {delete, DocId :: barrel_local:docid(), Rev :: barrel_local:revid()} |
                    {put_rev, Doc :: barrel_local:doc(), History :: list(), Deleted :: boolean()} |
                    map().

-export_type([
  batch/0,
  batch_op/0
]).

-spec new(Async) -> Batch when
  Async :: boolean(),
  Batch :: batch().
new(Async) ->
  {#{}, self(), make_ref(), Async, 0}.

-spec put(Obj, Rev, BatchIn) -> BatchOut when
  Obj :: barrel_local:doc(),
  Rev :: barrel_local:revid(),
  BatchIn :: batch(),
  BatchOut :: batch().
put(Obj, Rev, Batch) ->
  ok = validate_docid(Obj),
  Doc = barrel_doc:make_doc(Obj, [Rev], false),
  Req = make_req(Batch),
  Op = make_op(Doc, Req, false, false, false),
  update_batch(Doc#doc.id, Op, Batch).

-spec post(Obj, IsUpsert, BatchIn) -> BatchOut when
  Obj :: barrel_local:doc(),
  IsUpsert :: boolean(),
  BatchIn :: batch(),
  BatchOut :: batch().
post(Obj0, IsUpsert, Batch) ->
  %% maybe create the doc id
  Obj1 = case barrel_doc:id(Obj0) of
            undefined ->
              Obj0#{ <<"id">> => barrel_lib:uniqid() };
            _Id ->
              Obj0
          end,
  %% create doc record
  Doc = barrel_doc:make_doc(Obj1, [<<>>], false),
  %% update the batch
  ErrorIfExists = (IsUpsert =:= false),
  Req = make_req(Batch),
  Op = make_op(Doc, Req, false, true, ErrorIfExists),
  update_batch(Doc#doc.id, Op, Batch).


-spec delete(DocId, Rev, BatchIn) -> BatchOut when
  DocId :: barrel_local:docid(),
  Rev :: barrel_local:revid(),
  BatchIn :: batch(),
  BatchOut :: batch().
delete(DocId, Rev, Batch) when is_binary(DocId) ->
  Doc = barrel_doc:make_doc(#{<<"id">> => DocId}, [Rev], true),
  Req = make_req(Batch),
  Op = make_op(Doc, Req, false, false, false),
  update_batch(Doc#doc.id, Op, Batch).

-spec put_rev(Obj, History, Deleted, BatchIn) -> BatchOut when
  Obj :: barrel_local:doc(),
  History :: barrel_local:list(),
  Deleted :: boolean(),
  BatchIn :: batch(),
  BatchOut :: batch().
put_rev(Obj, History, Deleted, Batch) ->
  ok = validate_docid(Obj),
  Doc = barrel_doc:make_doc(Obj, History, Deleted),
  Req = make_req(Batch),
  Op = make_op(Doc, Req, true, false, false),
  update_batch(Doc#doc.id, Op, Batch).

-spec to_buckets(Batch) -> {DocBuckets, Ref, Async, N} when
  Batch :: batch(),
  DocBuckets :: list(),
  Ref :: reference(),
  Async :: boolean(),
  N :: non_neg_integer().
to_buckets({DocBuckets, _, Ref, Async, N}) -> {DocBuckets, Ref, Async, N}.


make_req({_DocBuckets, Client, Ref, Async, Idx}) ->
  {Client, Ref, Idx, Async}.

-spec from_list(OPs, Async) -> Batch when
  OPs :: [batch_op()],
  Async :: boolean(),
  Batch :: batch().
from_list(OPs, Async) ->
  from_list_1(OPs, barrel_write_batch:new(Async)).


from_list_1([Op | Rest], Batch) ->
  case parse_op(Op) of
    {put, Obj, Rev} ->
      Batch2 = barrel_write_batch:put(Obj, Rev, Batch),
      from_list_1(Rest, Batch2);
    {post, Obj, IsUpsert} ->
      Batch2 = barrel_write_batch:post(Obj, IsUpsert, Batch),
      from_list_1(Rest, Batch2);
    {delete, Id, Rev} ->
      Batch2 = barrel_write_batch:delete(Id, Rev, Batch),
      from_list_1(Rest, Batch2);
    {put_rev, Obj, History, Deleted} ->
      Batch2 = barrel_write_batch:put_rev(Obj, History, Deleted, Batch),
      from_list_1(Rest, Batch2)
  end;
from_list_1([], Batch) ->
  Batch.

is_batch({_, _, _, _, _}) -> true;
is_batch(_) -> false.

%% expected ops coming from the API
parse_op(#{ <<"op">> := <<"put">>, <<"doc">> := Doc} = OP) ->
  Rev = maps:get(<<"rev">>, OP, <<>>),
  {put, Doc, Rev};
parse_op(#{ <<"op">> := <<"post">>, <<"doc">> := Doc} = OP) ->
  IsUpsert = maps:get(<<"is_upsert">>, OP, false),
  {post, Doc, IsUpsert};
parse_op(#{ <<"op">> := <<"delete">>, <<"id">> := DocId} = OP) ->
  Rev = maps:get(<<"rev">>, OP, <<>>),
  {delete, DocId, Rev};
parse_op(#{ <<"op">> := <<"put_rev">>, <<"doc">> := Doc, <<"history">> := History} = OP) ->
  Deleted = maps:get(<<"deleted">>, OP, false),
  {put_rev, Doc, History, Deleted};

%% internal batch
parse_op({put, Doc, Rev} = OP) when is_map(Doc), is_binary(Rev) -> OP;
parse_op({post, Doc, IsUpsert} = OP) when is_map(Doc), is_boolean(IsUpsert) -> OP;
parse_op({delete, Id, Rev} = OP) when is_binary(Id), is_binary(Rev) -> OP;
parse_op({put_rev, Doc, History, Deleted} = OP)  when is_map(Doc), is_list(History), is_boolean(Deleted) -> OP;

parse_op(_) -> erlang:error(badarg).


make_op(Doc, Req, WithConflict, CreateIfMissing, ErrorIfExists) ->
  {Doc, WithConflict, CreateIfMissing, ErrorIfExists, Req}.

update_batch(Id, Op, {DocBuckets, Client, Ref, Async, Idx}) ->
  DocBuckets2 = case maps:find(Id, DocBuckets) of
                  {ok, OldUpdates} ->
                    maps:put(Id,  OldUpdates ++ [Op], DocBuckets);
                  error ->
                    maps:put(Id, [Op], DocBuckets)
                end,
  { DocBuckets2, Client, Ref, Async, Idx +1 }.

%% validate docid
validate_docid(#{ <<"id">> := _DocId }) -> ok;
validate_docid(_) -> erlang:error({bad_doc, invalid_docid}).