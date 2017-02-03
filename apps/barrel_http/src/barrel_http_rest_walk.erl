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

-module(barrel_http_rest_walk).
-author("Benoit Chesneau").


%% API
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([trails/0]).

trails() ->
  Metadata =
    #{ get => #{ summary => "Retrieve documents and the values matching a path"
      , produces => ["application/json"]
      , parameters =>
      [#{ name => <<"database">>
        , description => <<"Database Name">>
        , in => <<"path">>
        , required => true
        , type => <<"string">>}
        ,#{ name => <<"include_docs">>
        , description => <<"Include documents">>
        , in => <<"query">>
        , required => false
        , type => <<"boolean">>}

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
  [trails:trail("/dbs/:database/walk/[...]", ?MODULE, [], Metadata)].

-record(state, {method, database, dbid, start_seq, end_seq, max, conn}).

init(_Type, Req, []) ->
  {ok, Req, #state{}}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  route(Req2, State#state{method=Method}).

terminate(_Reason, _Req, _State) ->
  ok.

route(Req, #state{method= <<"GET">>}=State) ->
  check_database(Req, State);
route(Req, State) ->
  barrel_http_reply:error(405, "method not allowed", Req, State).

check_database(Req, State) ->
  {Database, Req2} = cowboy_req:binding(database, Req),
  case barrel_http_lib:has_database(Database) of
    false ->
      barrel_http_reply:error(404, "db not found", Req2, State);
    true ->
      State2 = State#state{database=Database},
      get_resource(Req2, State2)
  end.

get_resource(Req0, State = #state{database=Database}) ->
  {Path, _} = cowboy_req:path(Req0),
  case binary:split(Path, << "/dbs/", Database/binary, "/walk">>) of
    [<<>>, <<>>] ->
      fold_docs(Req0, State);
    [<<>>, <<"/">>] ->
      fold_docs(Req0, State);
    [<<>>, << "/", Pointer/binary >>] ->
      fold_query(Pointer, Req0, State);
    _ ->
      barrel_http_reply:error(400, "bad_request", Req0, State)
  end.

fold_query(Path, Req0, State = #state{database=Database}) ->
  Options = parse_params(Req0),
  IncludeDocs = proplists:get_value(include_docs, Options, false),
  OrderBy = proplists:get_value(order_by, Options, order_by_key),
  Req = start_chunked_response(Req0, State),
  Fun =
    fun(DocId, Doc, Val, {N, Pre}) ->
      Obj = case IncludeDocs of
              true ->
                #{ <<"id">> => DocId, <<"val">> => Val, <<"doc">> => Doc };
              false ->
                #{ <<"id">> => DocId, <<"val">> => Val}
            end,
      Chunk = << Pre/binary, (jsx:encode(Obj))/binary >>,
      ok = cowboy_req:chunk(Chunk, Req),
      {ok, {N + 1, <<",">>}}
    end,
  %% start the initial chunk
  ok = cowboy_req:chunk(<<"{\"docs\":[">>, Req),
  {Count, _} = barrel:query(Database, Path, Fun, {0, <<"">>}, OrderBy, Options),

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

fold_docs(Req0, State = #state{database=Database}) ->
  Options = parse_params(Req0),
  Req = start_chunked_response(Req0, State),

  %% start the initial chunk
  ok = cowboy_req:chunk(<<"{\"docs\":[">>, Req),
  Fun =
    fun
      (_DocId, _DocInfo, {ok, nil}, Acc) ->
        {ok, Acc};
      (DocId, _DocInfo, {ok, Doc}, {N, Pre}) ->
        Obj = #{ id => DocId, <<"val">> => Doc, <<"doc">> => Doc},
        Chunk = << Pre/binary, (jsx:encode(Obj))/binary >>,
        ok = cowboy_req:chunk(Chunk, Req),
        {ok, {N + 1, <<",">>}}
    end,
  {Count, _} = barrel:fold_by_id(Database, Fun, {0, <<"">>}, [{include_doc, true} | Options]),

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


start_chunked_response(Req0, #state{database=Database}) ->
  #{last_update_seq := Seq} = barrel:db_infos(Database),
  {ok, Req} = cowboy_req:chunked_reply(
    200,
    [{<<"Content-Type">>, <<"application/json">>},
      {<<"ETag">>,  <<"W/\"", (integer_to_binary(Seq))/binary, "\"" >>}],
    Req0
  ),
  Req.

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
  {max, binary_to_integer(MaxBin)};
param(<<"include_docs">>, IncludeDocs) ->
  {include_docs, barrel_lib:to_atom(IncludeDocs)};
param(<<"order_by">>, <<"$key">>) ->
  {order_by, order_by_key};
param(<<"order_by">>, <<"$value">>) ->
  {order_by, order_by_value};
param(<<"order_by">>, _) ->
  {order_by, order_by_key}.
  
