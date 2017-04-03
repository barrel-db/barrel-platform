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
        ]).


-export([ init/3
        , increment/3
        , set_value/3
        , duration/3
        ]).

all() -> [ plugin
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
  InitMsgs = collect_all_messages(),
  CountersInit = [ C || {plugin, init, {counter, C}, _} <- InitMsgs],
  [ <<"testdb">> = Db || [_,Db,_] <- CountersInit ],
  6 = length(CountersInit),
  TimersInit = [ G || {plugin, init, {duration, G}, _} <- InitMsgs],
  [ <<"testdb">> = Db || [_,Db,_] <- TimersInit ],
  4 = length(TimersInit),
  GaugesInit = [ G || {plugin, init, {gauge, G}, _} <- InitMsgs],
  [ <<"testdb">> = Db || [_,Db,_] <- GaugesInit ],
  2 = length(GaugesInit),

  Name = [<<"replication">>, <<"repid">>, <<"doc_reads">>],
  barrel_metrics:init(counter, Name),
  barrel_metrics:increment(Name),
  barrel_metrics:set_value(Name, 42),
  barrel_metrics:duration(Name, 123),

  Msgs = collect_all_messages(),
  ExpectedEnv = env(),
  [ {plugin, init, {counter, Name}, ExpectedEnv}
  , {plugin, increment, Name, 1, ExpectedEnv}
  , {plugin, set_value, Name, 42, ExpectedEnv}
  , {plugin, duration, Name, 123, ExpectedEnv}
  ] = Msgs,

  Doc = #{ <<"id">> => <<"a">>, <<"v">> => 1},
  {ok, <<"a">>, _} = barrel_local:post(<<"testdb">>, Doc, []),

  %% ensure all metrics have been initialized with plugin function init
  MsgsPost = collect_all_messages(),
  Post = [ N || {plugin, _, N , _, _} <- MsgsPost],
  Init = CountersInit ++ TimersInit ++ GaugesInit,
  [ true = lists:member(N, Init) || N <- Post ],

  ok.

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
