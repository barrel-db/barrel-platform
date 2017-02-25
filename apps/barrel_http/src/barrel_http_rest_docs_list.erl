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

get_resource(Database, Req0, State) ->
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
        DocWithMeta = Doc#{<<"_meta">> => Meta},
        Chunk = << Pre/binary, (jsx:encode(DocWithMeta))/binary >>,
        ok = cowboy_req:stream_body(Chunk, nofin, Req),
        {ok, {N + 1, <<",">>}}
    end,
  {Count, _} = barrel_local:fold_by_id(Database, Fun, {0, <<"">>}, [{include_doc, true} | Options]),

  %% close the document list and return the calculated count
  ok = cowboy_req:stream_body(
    iolist_to_binary([
      <<"],">>,
      <<"\"_count\":">>,
      integer_to_binary(Count),
      <<"}">>
      ]),
    fin,
    Req
  ),
  {ok, Req, State}.

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
