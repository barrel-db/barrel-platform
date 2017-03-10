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

-module(barrel_metrics_SUITE).
-author("Bernard Notarianni").

-behaviour(barrel_stats_plugin).

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ plugin/1
        , measures/1
        ]).


-export([init/3, increment/2]).

all() -> [ plugin
         , measures
         ].

env() ->
  [{plugin, ?MODULE}].

init_per_suite(Config) ->
  application:stop(barrel_store),
  ok = application:set_env(barrel_store, metrics, env()),
  {ok, _} = application:ensure_all_started(barrel_store),
  Config.

init_per_testcase(_, Config) ->
  true = register(test_metric_client, self()),
  {ok, _} = barrel_store:create_db(<<"testdb">>, #{}),
  [{db, <<"testdb">>} | Config].

end_per_testcase(_, _Config) ->
  timer:sleep(10),
  ok = barrel_store:delete_db(<<"testdb">>),
  timer:sleep(200),
  ok.

end_per_suite(Config) ->
  application:stop(barrel_store),
  _ = (catch rocksdb:destroy("docs", [])),
  ok = application:set_env(barrel_store, metrics, undefined),
  Config.

plugin(_Config) ->
  Name = [<<"replication">>, <<"repid">>, <<"doc_reads">>],
  barrel_metrics:init(counter, Name),
  barrel_metrics:increment(Name),

  Msgs = collect_messages(2),
  ExpectedEnv = env(),
  [ {plugin, init, {counter, Name}, ExpectedEnv},
    {plugin, increment, Name, ExpectedEnv} ] = Msgs,
  ok.

measures(_Config) ->
  {ok, _, _} = barrel_local:post(<<"testdb">>, #{<<"v">> => 42}, []),
  Msgs = collect_messages(1),
  ExpectedEnv = env(),
  [{plugin, increment
   , [ <<"dbs">>, <<"testdb">>, <<"doc_created">>]
   , ExpectedEnv}] = Msgs,
  ok.

collect_messages(N) ->
  lists:reverse(collect_messages(N,[])).

collect_messages(0, Acc) ->
  Acc;
collect_messages(N, Acc) ->
  receive
    M ->
      collect_messages(N-1, [M|Acc])
  after 2000 ->
      {error, timeout}
  end.

%% =============================================================================
%% plugin callbacks
%% =============================================================================

init(Type, Name, Env) ->
  Pid = whereis(test_metric_client),
  Pid ! {plugin, init, {Type, Name}, Env},
  ok.

increment(Name, Env) ->
  Pid = whereis(test_metric_client),
  Pid ! {plugin, increment, Name, Env},
  ok.

