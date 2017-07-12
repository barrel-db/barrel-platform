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
-export([init/2]).

-record(state, {method, database, dbid, start_seq, end_seq, max, conn}).

init(Req, _Opts) ->
  Method = cowboy_req:method(Req),
  route(Req, #state{method=Method}).

route(Req, #state{method= <<"GET">>}=State) ->
  check_database(Req, State);
route(Req, State) ->
  barrel_http_reply:error(405, "method not allowed", Req, State).

check_database(Req, State) ->
  Database = cowboy_req:binding(database, Req),
  case barrel_http_lib:has_database(Database) of
    false ->
      barrel_http_reply:error(404, "db not found", Req, State);
    true ->
      State2 = State#state{database=Database},
      get_resource(Req, State2)
  end.

get_resource(Req0, State = #state{database=Database}) ->
  Path = cowboy_req:path(Req0),
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
  IncludeDocs = maps:get(include_docs, Options, false),
  Req = start_chunked_response(Req0, State),
  Fun =
    fun(Doc, Meta, {N, Pre}) ->
      Obj = case IncludeDocs of
              true ->
                #{ <<"id">> => maps:get(<<"id">>, Doc), <<"meta">> => Meta, <<"doc">> => Doc };
              false ->
                #{ <<"id">> => maps:get(<<"id">>, Doc), <<"meta">> => Meta}
            end,
      Chunk = << Pre/binary, (jsx:encode(Obj))/binary >>,
      ok = cowboy_req:stream_body(Chunk, nofin, Req),
      {ok, {N + 1, <<",">>}}
    end,
  %% start the initial chunk
  ok = cowboy_req:stream_body(<<"{\"docs\":[">>, nofin, Req),
  _ = lager:info("that's my options ~p~n", [Options]),
  {Count, _} = barrel:walk(Database, Path, Fun, {0, <<"">>}, Options),

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

fold_docs(Req0, State = #state{database=Database}) ->
  Options = parse_params(Req0),
  Req = start_chunked_response(Req0, State),
  IncludeDocs = proplists:get_value(include_docs, Options, false),

  %% start the initial chunk
  ok = cowboy_req:stream_body(<<"{\"docs\":[">>, nofin, Req),
  Fun =
    fun
      (Doc, Meta, {N, Pre}) ->
         Obj = case IncludeDocs of
              true ->
                #{ <<"id">> => maps:get(<<"id">>, Doc), <<"meta">> => Meta, <<"doc">> => Doc };
              false ->
                #{ <<"id">> => maps:get(<<"id">>, Doc), <<"meta">> => Meta}
            end,
        Chunk = << Pre/binary, (jsx:encode(Obj))/binary >>,
        ok = cowboy_req:stream_body(Chunk, nofin, Req),
        {ok, {N + 1, <<",">>}}
    end,
  {Count, _} = barrel:fold_by_id(Database, Fun, {0, <<"">>}, #{include_doc => true}),

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


start_chunked_response(Req0, #state{database=Database}=State) ->
  case barrel:database_infos(Database) of
    {ok, Infos} ->
      Seq = maps:get(last_update_seq, Infos),
      Req = cowboy_req:stream_reply(
              200,
              #{<<"Content-Type">> => <<"application/json">>,
                <<"ETag">> =>  <<"W/\"", (integer_to_binary(Seq))/binary, "\"" >>},
              Req0
             ),
      Req;
    _ ->
      barrel_http_reply:error(500, Req0, State)
  end.

parse_params(Req) ->
  Params = cowboy_req:parse_qs(Req),
  lists:foldl(fun parse_params_fun/2, #{}, Params).

parse_params_fun({<<"start_at">>, StartKey}, Options) ->
  Options#{start_at => decode_json(StartKey)};
parse_params_fun({<<"end_at">>, EndKey}, Options) ->
  Options#{end_at => decode_json(EndKey)};
parse_params_fun({<<"equal_to">>, EqualTo}, Options) ->
  Options#{equal_to => decode_json(EqualTo)};
parse_params_fun({<<"limit_to_last">>, Limit}, Options) ->
  Options#{equal_to => decode_json(Limit)};
parse_params_fun({<<"limit_to_first">>, Limit}, Options) ->
  Options#{limit_to_first => decode_json(Limit)};
parse_params_fun({<<"include_docs">>, <<"true">>}, Options) ->
  Options#{include_docs => true};
parse_params_fun({<<"order_by">>, <<"$key">>}, Options) ->
  Options#{order_by => order_by_key};
parse_params_fun({<<"order_by">>, <<"$value">>}, Options) ->
  Options#{order_by => order_by_value};
parse_params_fun(_, Options) ->
  Options.

decode_json(Bin) ->
  case jsx:is_json(Bin) of
    true -> jsx:decode(Bin);
    false -> Bin
  end.

