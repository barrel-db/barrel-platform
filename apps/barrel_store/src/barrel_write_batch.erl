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
  to_buckets/1
]).

-include_lib("barrel_common/include/barrel_common.hrl").

new(Async) ->
  {#{}, self(), make_ref(), Async, 0}.

put(Obj, Rev, Batch) ->
  ok = validate_docid(Obj),
  Doc = barrel_doc:make_doc(Obj, [Rev], false),
  Req = make_req(Batch),
  Op = make_op(Doc, Req, false, false, false),
  update_batch(Doc#doc.id, Op, Batch).

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

delete(DocId, Rev, Batch) when is_binary(DocId) ->
  Doc = barrel_doc:make_doc(#{<<"id">> => DocId}, [Rev], true),
  Req = make_req(Batch),
  Op = make_op(Doc, Req, false, false, false),
  update_batch(Doc#doc.id, Op, Batch).

put_rev(Obj, History, Deleted, Batch) ->
  ok = validate_docid(Obj),
  Doc = barrel_doc:make_doc(Obj, History, Deleted),
  Req = make_req(Batch),
  Op = make_op(Doc, Req, true, false, false),
  update_batch(Doc#doc.id, Op, Batch).

to_buckets({DocBuckets, _, Ref, Async, N}) -> {DocBuckets, Ref, Async, N}.

make_req({_DocBuckets, Client, Ref, Async, Idx}) ->
  {Client, Ref, Idx, Async}.

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

%% internal
validate_docid(#{ <<"id">> := _DocId }) -> ok;
validate_docid(_) -> erlang:error({bad_doc, invalid_docid}).