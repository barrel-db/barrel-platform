%% Copyright 2017, Bernard Notarianni
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

-module(barrel_http_rest_docs).
-author("Bernard Notarianni").

%% API
-export([init/2]).
-export([info/3]).
-export([terminate/3]).

-include("barrel_http_rest_docs.hrl").

accepted_feed(Req) ->
  case cowboy_req:header(<<"accept">>, Req) of
    undefined -> aim_feed(Req);
    Accept ->
      case hackney_bstr:to_lower(Accept) of
        <<"text/event-stream">> -> << "eventsource">>;
        _ ->
          aim_feed(Req)
      end
  end.

aim_feed(Req) ->
  case cowboy_req:header(<<"a-im">>, Req) of
    undefined -> param_feed(Req);
    AIM ->
      case hackney_bstr:to_lower(AIM) of
        <<"incremental feed">> -> <<"normal">>;
        _ -> param_feed(Req)
      end
  end.

param_feed(Req) ->
  case cowboy_req:qs(Req) of
    #{feed := Feed} -> hackney_bstr:to_lower(Feed);
    _ -> undefined
  end.

init(Req, _Opts) ->
  Path = cowboy_req:path(Req),
  Feed = accepted_feed(Req),
  IsChangesFeed = lists:member(Feed, [<<"normal">>, <<"eventsource">>]),
  Route = binary:split(Path, <<"/">>, [global]),
  S1 = #state{path=Path},
  case {Route,  IsChangesFeed} of
    {[<<>>,<<"dbs">>,_,<<"docs">>], false} ->
      S2 = S1#state{handler=list},
      {ok, Req2, S3} = barrel_http_rest_docs_id:init(Req, S2),
      handle(Req2, S3);
    {[<<>>,<<"dbs">>,_,<<"docs">>], true} ->
      S2 = S1#state{handler=changes},
      {Loop, Req2, S3} = barrel_http_rest_docs_changes:init(Req, S2),
      case Loop of
        cowboy_loop -> {cowboy_loop, Req2, S3};
        ok -> handle(Req2, S3)
      end;
    _ ->
      S2 = S1#state{handler=list},
      {ok, Req2, S3} = barrel_http_rest_docs_id:init(Req, S2),
      handle(Req2, S3)
  end.

handle(Req, #state{handler=changes}=State) ->
  barrel_http_rest_docs_changes:handle(Req, State);
handle(Req, State) ->
  check_database_db(Req, State).

check_database_db(Req, State) ->
  Database = cowboy_req:binding(database, Req),
  case barrel_http_lib:has_database(Database) of
    false ->
      barrel_http_reply:error(400, <<"database not found: ", Database/binary>>, Req, State);
    true ->
      Method = cowboy_req:method(Req),
      DocId = cowboy_req:binding(docid, Req),
      State2 =  State#state{
                  database=Database,
                  docid=DocId,
                  method=Method
                 },
      parse_headers(Req, State2)
  end.


parse_headers(Req, State) ->
  Headers = cowboy_req:headers(Req),
  F = fun
        (<<"etag">>, Etag, S) ->
          Opt = S#state.options,
          S#state{options=[{rev, Etag}|Opt]};
        (<<"x-barrel-id-match">>, IdsMatch, #state{idmatch=DocIds}=S) ->
          WithParsed = parse_header_match_id(IdsMatch, DocIds),
          S#state{idmatch=WithParsed};
        (_, _, S) -> S
      end,
  State2 = maps:fold(F, State, Headers),
  route_all_docs(Req, State2).

parse_header_match_id(Bin, undefined) ->
  parse_header_match_id(Bin, []);
parse_header_match_id(Bin, Acc) when is_binary(Bin) ->
  DocIds = binary:split(Bin, <<",">>, [global]),
  parse_header_match_id(DocIds, Acc);
parse_header_match_id([], Acc) ->
  lists:reverse(Acc);
parse_header_match_id([DocId|Tail], Acc) ->
  L = binary:split(DocId, <<" ">>, [global]),
  [Trimmed] = [ X || X <- L, X =/= <<>> ],
  parse_header_match_id(Tail, [Trimmed|Acc]).


route_all_docs(Req, #state{method= <<"GET">>, database=Database, docid=undefined}=State) ->
  barrel_http_rest_docs_list:get_resource(Database, Req, State);
route_all_docs(
  #{headers := #{<<"x-barrel-write-batch">> := <<"true">>}}=Req,
  #state{method= <<"POST">>, docid=undefined}=State
) ->
  barrel_http_rest_docs_list:handle_write_batch(Req, State);
route_all_docs(Req, State) ->
  barrel_http_rest_docs_id:handle(Req, State).

info(Message, Req, #state{handler=changes}=State) ->
  barrel_http_rest_docs_changes:info(Message, Req, State).

terminate(Reason, Req, #state{handler=changes}=State) ->
  barrel_http_rest_docs_changes:terminate(Reason, Req, State);
terminate(Reason, Req, State) ->
  barrel_http_rest_docs_id:terminate(Reason, Req, State).

