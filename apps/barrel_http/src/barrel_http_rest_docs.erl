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
-export([init/3]).
-export([handle/2]).
-export([info/3]).
-export([terminate/3]).

-include("barrel_http_rest_docs.hrl").

accepted_feed(Req) ->
  case cowboy_req:header(<<"accept">>, Req) of
    {undefined, _} -> aim_feed(Req);
    {Accept, _} ->
      case hackney_bstr:to_lower(Accept) of
        <<"text/event-stream">> -> << "eventsource">>;
        _ ->
          aim_feed(Req)
      end
  end.

aim_feed(Req) ->
  case cowboy_req:header(<<"a-im">>, Req) of
    {undefined, _} -> param_feed(Req);
    {AIM, _} ->
      case hackney_bstr:to_lower(AIM) of
        <<"incremental feed">> -> <<"normal">>;
        _ -> param_feed(Req)
      end
  end.

param_feed(Req) ->
  case cowboy_req:qs_val(<<"feed">>, Req) of
    {undefined, _} -> undefined;
    {Feed, _} -> hackney_bstr:to_lower(Feed)
  end.

init(Type, Req, []) ->
  {Path, Req2} = cowboy_req:path(Req),
  Feed = accepted_feed(Req2),
  IsChangesFeed = lists:member(Feed, [<<"normal">>, <<"eventsource">>]),
  Route = binary:split(Path, <<"/">>, [global]),
  S1 = #state{path=Path},
  case {Route,  IsChangesFeed} of
    {[<<>>,<<"dbs">>,_,<<"docs">>], false} ->
      barrel_http_rest_docs_id:init(Type, Req2, S1#state{handler=list});
    {[<<>>,<<"dbs">>,_,<<"docs">>], true} ->
      barrel_http_rest_docs_changes:init(Type, Req2, S1#state{handler=changes});
    _ ->
      barrel_http_rest_docs_id:init(Type, Req2, S1#state{handler=doc})
  end.

handle(Req, #state{handler=changes}=State) ->
  barrel_http_rest_docs_changes:handle(Req, State);
handle(Req, State) ->
  check_database_db(Req, State).

check_database_db(Req, State) ->
  {Database, Req2} = cowboy_req:binding(database, Req),
  case barrel_http_lib:has_database(Database) of
    false ->
      barrel_http_reply:error(400, <<"database not found: ", Database/binary>>, Req2, State);
    true ->
      {Method, Req3} = cowboy_req:method(Req2),
      {DocId, Req4} = cowboy_req:binding(docid, Req3),
      State2 =  State#state{
                  database=Database,
                  docid=DocId,
                  method=Method
                 },
      route_all_docs(Req4, State2)
  end.

route_all_docs(Req, #state{method= <<"GET">>, database=Database, docid=undefined}=State) ->
  barrel_http_rest_docs_list:get_resource(Database, Req, State);
route_all_docs(Req, State) ->
  barrel_http_rest_docs_id:handle(Req, State).

info(Message, Req, #state{handler=changes}=State) ->
  barrel_http_rest_docs_changes:info(Message, Req, State).

terminate(Reason, Req, #state{handler=changes}=State) ->
  barrel_http_rest_docs_changes:terminate(Reason, Req, State);
terminate(Reason, Req, State) ->
  barrel_http_rest_docs_id:terminate(Reason, Req, State).

