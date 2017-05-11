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

-module(barrel_replicate_metrics).
-author("Bernard Notarianni").

-export([new/0]).
-export([inc/3]).
-export([update_times/3]).
-export([create_task/2]).
-export([update_task/1]).
-export([all/0]).


new() ->
  #{ docs_read => 0
   , doc_read_failures => 0
   , doc_read_times => new_time_stats()
   , docs_written => 0
   , doc_write_failures => 0
   , doc_write_times => new_time_stats()
   }.

new_time_stats() ->
  #{ values => []
   , period => 10
   , mean => 0
   }.

inc(CounterName, Stats, Number) ->
  Counter = maps:get(CounterName, Stats),
  maps:update(CounterName, Counter+Number, Stats).

update_times(MeasureName, NewMeasure, Stat) ->
  TimeStat = maps:get(MeasureName, Stat),
  Values = maps:get(values, TimeStat),
  Period = maps:get(period, TimeStat),
  {Mean, L2} = mean([NewMeasure|Values], Period),
  TimeStat2 = TimeStat#{values:=L2, mean:=Mean},
  maps:update(MeasureName, TimeStat2, Stat).

mean(List, Period) ->
  mean(List, [], 0, 0, Period).

mean([], Acc, Sum, Size, _Period) ->
  {Sum/Size, Acc};

mean([_|_], Acc, Sum, Size, Period) when Size > Period ->
  mean([], Acc, Sum, Size, Period);

mean([V|Others], Acc, Sum, Size, Period) ->
  mean(Others, [V|Acc], Sum+V, Size+1, Period).


%%==============================================================================
%% Storage of collected metrics
%%==============================================================================

create_task(Metrics, _Options) ->
  ok = barrel_task_status:add_task(barrel_stat_replication, Metrics),
  ok.

update_task(Metrics) ->
  barrel_task_status:update(barrel_stat_replication, Metrics).


all() ->
  barrel_task_status:all(barrel_stat_replication).
