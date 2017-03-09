%% Copyright 2016, Bernard Notarianni
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

-module(barrel_http_rest_docs_list).
-author("Bernard Notarianni").


%% API
-export([get_resource/3]).
-export([handle_write_batch/2]).

-include("barrel_http_rest_docs.hrl").


get_resource(Database, Req0, #state{idmatch=undefined}=State) ->
  Options = parse_params(Req0),
  #{last_update_seq := Seq} = barrel_local:db_infos(Database),
  Req = cowboy_req:stream_reply(
    200,
    #{<<"Content-Type">> => <<"application/json">>,
      <<"ETag">> =>  <<"W/\"", (integer_to_binary(Seq))/binary, "\"" >>},
    Req0
  ),
  %% start the initial chunk
  ok = cowboy_req:stream_body(<<"{\"docs\":[">>, nofin, Req),
  Fun =
    fun (Doc, Meta, {N, Pre}) ->
        DocWithMeta =  #{ <<"doc">>  => Doc, <<"meta">> => Meta },
        Chunk = << Pre/binary, (jsx:encode(DocWithMeta))/binary >>,
        ok = cowboy_req:stream_body(Chunk, nofin, Req),
        {ok, {N + 1, <<",">>}}
    end,
  {Count, _} = barrel_local:fold_by_id(Database, Fun, {0, <<"">>}, [{include_doc, true} | Options]),

  %% close the document list and return the calculated count
  ok = cowboy_req:stream_body(
    iolist_to_binary([
      <<"],">>,
      <<"\"count\":">>,
      integer_to_binary(Count),
      <<"}">>
      ]),
    fin,
    Req
  ),
  {ok, Req, State};

get_resource(Database, Req0, #state{idmatch=DocIds}=State) when is_list(DocIds) ->
  %% let's process it
  Req = cowboy_req:stream_reply(
          200,
          #{<<"Content-Type">> => <<"application/json">>},
          Req0
         ),
  %% start the initial chunk
  ok = cowboy_req:stream_body(<<"{\"docs\":[">>, nofin, Req),
  Fun =
    fun (Doc, Meta, {N, Pre}) ->
        DocWithMeta =  #{ <<"doc">>  => Doc, <<"meta">> => Meta },
        Chunk = << Pre/binary, (jsx:encode(DocWithMeta))/binary >>,
        ok = cowboy_req:stream_body(Chunk, nofin, Req),
        {N + 1, <<",">>}
    end,
  AccIn = {0, <<"">>},
  Options = [],
  {Count, _} = barrel_local:multi_get(Database, Fun, AccIn, DocIds, Options),

  %% close the document list and return the calculated count
  ok = cowboy_req:stream_body(
         iolist_to_binary([
                           <<"],">>,
                           <<"\"count\":">>,
                           integer_to_binary(Count),
                           <<"}">>
                          ]),
         fin,
         Req
        ),
  {ok, Req, State}.


handle_write_batch(Req, State) ->
  {ok, Body, Req2} = cowboy_req:read_body(Req),
  case Body of
    <<>> ->
      barrel_http_reply:error(400, <<"empty body">>, Req2, State);
    Body ->
      try jsx:decode(Body, [return_maps]) of Json ->
        do_write_batch(Json, Req2, State)
      catch
        _:_ ->
          barrel_http_reply:error(400, <<"malformed json document">>, Req2, State)
      end

  end.

do_write_batch(Json, Req, State) ->
  Async = case Req of
            #{ headers := #{ <<"x-barrel-async">> := << "true">> }} -> true;
            _ -> false
          end,
  
  Ops = maps:get(<<"updates">>, Json),
  InitBatch = barrel_write_batch:new(Async),
  update_docs(Ops, InitBatch, Req, State).


update_docs([Op | Rest], Batch, Req, State) ->
  case parse_op(Op) of
    {put, Obj, Rev} ->
      Batch2 = barrel_write_batch:put(Obj, Rev, Batch),
      update_docs(Rest, Batch2, Req, State);
    {post, Obj, IsUpsert} ->
      Batch2 = barrel_write_batch:post(Obj, IsUpsert, Batch),
      update_docs(Rest, Batch2, Req, State);
    {delete, Id, Rev} ->
      Batch2 = barrel_write_batch:delete(Id, Rev, Batch),
      update_docs(Rest, Batch2, Req, State);
    {put_rev, Obj, History, Deleted} ->
      Batch2 = barrel_write_batch:put_rev(Obj, History, Deleted, Batch),
      update_docs(Rest, Batch2, Req, State);
    error ->
      barrel_http_reply:error(400, <<"invalid batch">>, Req, State)
  end;
update_docs([], Batch, Req, #state{database=Db}=State) ->
  lager:info("batch is ~p~n", [Batch]),
  case barrel_db:update_docs(Db, Batch) of
    ok ->
      barrel_http_reply:json(200, #{ <<"ok">> => true }, Req, State);
    Results ->
      JsonResults = [ batch_result(Result) || Result <- Results ],
      JsonResp = #{ <<"ok">> => true, <<"results">> =>  JsonResults },
      lager:info("results is ~p~n", [JsonResults]),
      
      barrel_http_reply:json(200, JsonResp, Req, State)
  end.

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
parse_op(_) ->
  error.

batch_result({ok, Id, Rev}) ->
  #{ <<"status">> => <<"ok">>, <<"id">> => Id, <<"rev">> => Rev};
batch_result({error, not_found}) ->
  #{ <<"status">> => <<"error">>, <<"reason">> => <<"not found">>};
batch_result({error, {conflict, doc_exists}}) ->
  #{ <<"status">> => <<"conflict">>, <<"reason">> => <<"doc exists">>};
batch_result({error, {conflict, revision_conflict}}) ->
  #{ <<"status">> => <<"conflict">>, <<"reason">> => <<"revision conflict">>};
batch_result({error, Reason}) ->
  #{ <<"status">> => <<"error">>, <<"reason">> => Reason}.

parse_params(Req) ->
  Params = cowboy_req:parse_qs(Req),
  Options = lists:foldl(fun({Param, Value}, Acc) ->
                            [param(Param, Value)|Acc]
                        end, [], Params),
  Options.

param(<<"start_key">>, StartKey) ->
  {gte, StartKey};
param(<<"end_key">>, EndKey) ->
  {lte, EndKey};
param(<<"gt">>, StartKey) ->
  {gt, StartKey};
param(<<"gte">>, EndKey) ->
  {gte, EndKey};
param(<<"lt">>, StartKey) ->
  {lt, StartKey};
param(<<"lte">>, EndKey) ->
  {lte, EndKey};
param(<<"max">>, MaxBin) ->
  {max, binary_to_integer(MaxBin)}.
