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

-module(barrel_http_metrics_SUITE).
-author("Bernard Notarianni").

-behaviour(barrel_stats_plugin).

-export([ all/0
        , end_per_suite/1
        , end_per_testcase/2
        , init_per_suite/1
        , init_per_testcase/2
        ]).

-export([ count_http/1
        ]).

-export([ init/3
        , increment/3
        , set_value/3
        , duration/3
        ]).


all() -> [ count_http
         ].

env() ->
  [{plugin, ?MODULE}].

init_per_suite(Config) ->
  application:stop(barrel_store),
  ok = application:set_env(barrel_store, metrics, env()),
  {ok, _} = application:ensure_all_started(barrel_http),
  {ok, _} = application:ensure_all_started(barrel_store),
  Config.

init_per_testcase(_, Config) ->
  true = register(test_metric_client, self()),
  _ = barrel_store:create_db(<<"testdb">>, #{}),
  Config.

end_per_testcase(_, Config) ->
  ok = barrel_local:delete_db(<<"testdb">>),
  Config.

end_per_suite(Config) ->
  application:stop(barrel_store),
  _ = (catch rocksdb:destroy("docs", [])),
  ok = application:set_env(barrel_store, metrics, undefined),
  Config.

r(Req) ->
  test_lib:req(Req).

count_http(_Config) ->
  Doc = #{<<"id">> => <<"acceptget">>, <<"name">> => <<"tom">>},
  {ok, _, _} = barrel_local:post(<<"testdb">>, Doc, []),

  #{code := 200} = r(#{method => get,
                        route => "/dbs/testdb/docs/acceptget"}),

  [ {increment, [<<"GET">>,<<"incoming">>]}
  , {increment, [<<"GET">>,<<"200">>]}
  ] = http_metrics(),

  #{code := 404} = r(#{method => get,
                       route => "/dbs/testdb/docs/doesnotexist"}),

  [ {increment, [<<"GET">>,<<"incoming">>]}
  , {increment, [<<"GET">>,<<"404">>]}
  ] = http_metrics(),

  #{code := 400} = r(#{method => put,
                       route => "/dbs/testdb/docs/acceptget"}),

  [ {increment, [<<"PUT">>,<<"incoming">>]}
  , {increment, [<<"PUT">>,<<"400">>]}
  ] = http_metrics(),

  ok.

http_metrics() ->
  [ {M,N} || {plugin, M, [<<"http">>|N] , _, _} <- collect_all_messages()].

collect_all_messages() ->
  collect_all_messages([]).
collect_all_messages(Acc) ->
  receive
    M ->
      collect_all_messages([M|Acc])
  after 0 ->
      lists:reverse(Acc)
  end.

%% =============================================================================
%% plugin callbacks
%% =============================================================================

init(Type, Name, Env) ->
  Pid = whereis(test_metric_client),
  Pid ! {plugin, init, {Type, Name}, Env},
  ok.

increment(Name, Value, Env) ->
  Pid = whereis(test_metric_client),
  Pid ! {plugin, increment, Name, Value, Env},
  ok.

set_value(Name, Value, Env) ->
  Pid = whereis(test_metric_client),
  Pid ! {plugin, set_value, Name, Value, Env},
  ok.

duration(Name, Value, Env) ->
  Pid = whereis(test_metric_client),
  Pid ! {plugin, duration, Name, Value, Env},
  ok.
