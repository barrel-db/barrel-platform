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

-module(barrel_http_rest_all_docs).
-author("Bernard Notarianni").


%% API
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([trails/0]).

trails() ->
  Metadata =
    #{ get => #{ summary => "Get list of all available documents."
               , produces => ["application/json"]
               , parameters =>
                   [#{ name => <<"store">>
                     , description => <<"Store ID">>
                     , in => <<"path">>
                     , required => true
                     , type => <<"string">>}

                     , #{ name => <<"gt">>
                     , description => <<"greater than">>
                     , in => <<"query">>
                     , required => false
                     , type => <<"string">>}

                     , #{ name => <<"gte">>
                     , description => <<"greater or equal to">>
                     , in => <<"query">>
                     , required => false
                     , type => <<"string">>}

                     , #{ name => <<"lt">>
                     , description => <<"lesser than">>
                     , in => <<"query">>
                     , required => false
                     , type => <<"string">>}

                     , #{ name => <<"lte">>
                     , description => <<"lesser or equal to">>
                     , in => <<"query">>
                     , required => false
                     , type => <<"string">>}

                     , #{ name => <<"max">>
                     , description => <<"maximum keys to return">>
                     , in => <<"query">>
                     , required => false
                     , type => <<"integer">>}
                   ]
               }
     },
  [trails:trail("/:store/_all_docs", ?MODULE, [], Metadata)].

-record(state, {method, store, dbid, start_seq, end_seq, max, conn}).

init(_Type, Req, []) ->
  {ok, Req, #state{}}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  route(Req2, State#state{method=Method}).

terminate(_Reason, _Req, _State) ->
  ok.

route(Req, #state{method= <<"GET">>}=State) ->
  check_store_db(Req, State);
route(Req, State) ->
  barrel_http_reply:error(405, "method not allowed", Req, State).

check_store_db(Req, State) ->
  {Store, Req2} = cowboy_req:binding(store, Req),
  case barrel_http_lib:has_store(Store) of
    false ->
      barrel_http_reply:error(400, "store not found", Req2, State);
    true ->
      State2 = State#state{store=Store},
      get_resource(Req2, State2)
  end.

get_resource(Req0, State = #state{store=Store}) ->
  Options = parse_params(Req0),
  #{last_update_seq := Seq} = barrel:db_infos(Store),
  {ok, Req} = cowboy_req:chunked_reply(
    200,
    [{<<"Content-Type">>, <<"application/json">>},
      {<<"ETag">>,  <<"W/\"", (integer_to_binary(Seq))/binary, "\"" >>}],
    Req0
  ),
  %% start the initial chunk
  ok = cowboy_req:chunk(<<"{\"docs\":[">>, Req),
  Fun =
    fun
      (_DocId, _DocInfo, {ok, nil}, Acc) ->
        {ok, Acc};
      (_DocId, _DocInfo, {ok, Doc}, {N, Pre}) ->
        Chunk = << Pre/binary, (jsx:encode(Doc))/binary >>,
        ok = cowboy_req:chunk(Chunk, Req),
        {ok, {N + 1, <<",">>}}
    end,
  {Count, _} = barrel:fold_by_id(Store, Fun, {0, <<"">>}, [{include_doc, true} | Options]),

  %% close the document list and return the calculated count
  ok = cowboy_req:chunk(
    iolist_to_binary([
      <<"],">>,
      <<"\"_count\":">>,
      integer_to_binary(Count),
      <<"}">>
      ]),
    Req
  ),
  {ok, Req, State}.

parse_params(Req) ->
  {Params, _} = cowboy_req:qs_vals(Req),
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
