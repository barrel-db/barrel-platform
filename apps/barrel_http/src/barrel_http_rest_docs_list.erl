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
  case barrel:db_infos(Database) of
    {ok, Info} ->
      Seq = maps:get(last_update_seq, Info),
      get_resource_since(Seq, Database, Req0, State);
    {error, _} ->
      barrel_http_reply:error(500, Req0, State)
  end;

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
  {Count, _} = barrel:multi_get(Database, Fun, AccIn, DocIds, Options),

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

get_resource_since(Seq, Database, Req0, #state{idmatch=undefined}=State) ->
  Options = parse_params(Req0),
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
  {Count, _} = barrel:fold_by_id(Database, Fun, {0, <<"">>}, [{include_doc, true} | Options]),

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

do_write_batch(Json, Req, #state{database=Db}=State) ->
  Async = case Req of
            #{ headers := #{ <<"x-barrel-async">> := << "true">> }} -> true;
            _ -> false
          end,

  OPs = maps:get(<<"updates">>, Json),
  try  barrel:write_batch(Db, OPs, [{async, Async}]) of
    ok ->
      barrel_http_reply:json(200, #{ <<"ok">> => true }, Req, State);
    Results ->
      JsonResults = [ batch_result(Result) || Result <- Results ],
      JsonResp = #{ <<"ok">> => true, <<"results">> =>  JsonResults },
      barrel_http_reply:json(200, JsonResp, Req, State)
  catch
    error:badarg ->
      barrel_http_reply:error(400, <<"invalid batch">>, Req, State)
  end.


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
