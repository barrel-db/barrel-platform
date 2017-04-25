%% Copyright (c) 2017. Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(barrel_stats_SUITE).
-author("benoitc").

%% API
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/1
]).

-export([
  register_metric/1,
  register_bad_metric/1,
  register_metrics/1,
  register_bad_metrics/1,
  counter/1,
  tick/1,
  histogram/1
]).


all() ->
  [
    register_metric,
    register_bad_metric,
    register_metrics,
    register_bad_metrics,
    counter,
    tick,
    histogram
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_stats),
  Config.

end_per_suite(Config) ->
  application:stop(barrel_stats),
  Config.


init_per_testcase(_, Config) ->
  ok = barrel_stats:reset_metrics(),
  Config.

end_per_testcase(_Config) ->
  ok.

register_metric(_Config) ->
  [] = barrel_stats:list_metrics(),
  M = #{ name => "test", type => counter, help => "test counter"},
  ok = barrel_stats:register_metric(M),
  [M] = barrel_stats:list_metrics(),
  {error, already_exists} = barrel_stats:register_metric(M),
  [M] = barrel_stats:list_metrics(),
  ok = barrel_stats:unregister_metric("test"),
  [] = barrel_stats:list_metrics(),
  ok.

register_bad_metric(_Config) ->
  {error, bad_metric} =  barrel_stats:register_metric(#{}),
  {error, bad_metric} =  barrel_stats:register_metric(#{ name => "t"}),
  {error, bad_metric} =  barrel_stats:register_metric(#{ name => "t", type => counter}),
  {error, bad_metric} =  barrel_stats:register_metric(#{ name => "t"}),
  ok =  barrel_stats:register_metric(#{ name => "t", type => counter, help => "h"}),
  ok =  barrel_stats:register_metric(#{ name => "t2", type => histogram, help => "h"}),
  {error, bad_metric} = barrel_stats:register_metric(#{ name => "t3", type => h, help => "h"}),
  ok.
  
register_metrics(_Config) ->
  [] = barrel_stats:list_metrics(),
  Metrics = [#{ name => "t1", type => counter, help => "h1"},
             #{ name => "t2", type => counter, help => "h2"}],
  ok = barrel_stats:register_metric(Metrics),
  Metrics = lists:sort(barrel_stats:list_metrics()),
  ok = barrel_stats:unregister_metric("t1"),
  [#{ name := "t2", type := counter, help := "h2"}] = barrel_stats:list_metrics(),
  ok.

register_bad_metrics(_Config) ->
  [] = barrel_stats:list_metrics(),
  Metrics = [#{ name => "t1", type => h, help => "h1"},
             #{ name => "t2", type => counter, help => "h2"}],
  {error, bad_metric} = barrel_stats:register_metric(Metrics),
  [] = barrel_stats:list_metrics(),
  ok.

counter(_Config) ->
  ok = barrel_stats:set_update_interval(10),
  ok = barrel_stats:register_metric(#{ name => "c", type => counter, help => ""}),
  ok = barrel_stats:record_count("c", #{ tag => "tag"}),
  ok = barrel_stats:refresh(),
  1 = barrel_stats:get_count("c", #{ tag => "tag"}),
  undefined = barrel_stats:get_count("c", #{ tag => "tag1"}),
  ok = barrel_stats:record_count("c", #{ tag => "tag1"}),
  ok = barrel_stats:refresh(),
  1 = barrel_stats:get_count("c", #{ tag => "tag1"}),
  ok = barrel_stats:record_count("c", #{ tag => "tag"}),
  ok = barrel_stats:refresh(),
  1 = barrel_stats:get_count("c", #{ tag => "tag1"}),
  2 = barrel_stats:get_count("c", #{ tag => "tag"}).
  
  
tick(_Config) ->
  ok = barrel_stats:set_update_interval(10),
  ok = barrel_stats:register_metric(#{ name => "c", type => counter, help => ""}),
  ok = barrel_stats:record_count("c", #{ tag => "tag"}),
  timer:sleep(50),
  1 = barrel_stats:get_count("c", #{ tag => "tag"}),
  ok = barrel_stats:record_count("c", #{ tag => "tag"}),
  timer:sleep(50),
  2 = barrel_stats:get_count("c", #{ tag => "tag"}).


histogram(_Config) ->
  ok = barrel_stats:set_update_interval(30000),
  ok = barrel_stats:register_metric(#{ name => "h", type => histogram, help => ""}),
  _ = [barrel_stats:measure_time("h", rand:uniform(1000)) || _ <- [1, 1000 | lists:seq(1, 10000)]],
  _ = barrel_stats:refresh(),
  Hist = barrel_stats:get_measure_time("h"),
  true = is_list(Hist),
  1 = proplists:get_value(min, Hist),
  1000 = proplists:get_value(max, Hist),
  ok.
