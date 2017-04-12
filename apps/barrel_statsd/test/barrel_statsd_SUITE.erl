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

-module(barrel_statsd_SUITE).
-author("Bernard Notarianni").

-behaviour(barrel_stats_plugin).

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ plugin/1
        , send_to_statsd/1
        ]).


-export([ init/3
        , increment/3
        , set_value/3
        , duration/3]).

all() -> [ plugin
         ].

-define(STATSD_PORT, 8888).


init_per_suite(Config) ->
  _ = application:stop(barrel_store),
  _ = application:stop(barrel_statsd),
  ok = application:set_env(barrel_store, metrics,
                           [{plugin, barrel_statsd}]),
  ok = application:set_env(barrel_statsd, server,
                           {{127,0,0,1}, ?STATSD_PORT}),
  {ok, _} = application:ensure_all_started(barrel_store),
  {ok, _} = application:ensure_all_started(barrel_statsd),
  Config.

init_per_testcase(_, Config) ->
  true = register(test_metric_testd, self()),
  {ok, _} = barrel_store:create_db(<<"testdb">>, #{}),
  [{db, <<"testdb">>} | Config].

end_per_testcase(_, _Config) ->
  timer:sleep(10),
  ok = barrel_store:delete_db(<<"testdb">>),
  timer:sleep(200),
  ok.

end_per_suite(Config) ->
  _ = (catch rocksdb:destroy("docs", [])),
  ok = application:set_env(barrel_store, metrics, undefined),
  Config.

plugin(_Config) ->
  start_udp_server(?STATSD_PORT),
  Name = [<<"replication">>, <<"repid">>, <<"doc_reads">>],
  barrel_metrics:init(counter, Name),
  barrel_metrics:increment(Name),

  Msgs = collect_messages(1),
  [{statsd_message, {counter, Key, 1}}] = Msgs,
  "barrel.nohost.nonode.replication.repid.doc_reads" = Key,
  ok.


collect_messages(N) ->
  lists:reverse(collect_messages(N,[])).

collect_messages(0, Acc) ->
  Acc;
collect_messages(N, Acc) ->
  receive
    M ->
      collect_messages(N-1, [M|Acc])
  after 5000 ->
      [{error, timeout}|Acc]
  end.

%% =============================================================================
%% plugin callbacks
%% =============================================================================

init(Type, Name, _Env) ->
  Pid = whereis(test_metric_testd),
  Pid ! {plugin, init, {Type, Name}},
  ok.

increment(Name, _Value, _Env) ->
  Pid = whereis(test_metric_testd),
  Pid ! {plugin, increment, Name},
  ok.

set_value(Name, _Value, _Env) ->
  Pid = whereis(test_metric_testd),
  Pid ! {plugin, set_value, Name},
  ok.

duration(Name, _Value, _Env) ->
  Pid = whereis(test_metric_testd),
  Pid ! {plugin, duration, Name},
  ok.

%% =============================================================================
%% Statsd UDP server
%% Collecting internal metrics from a barrel node
%% =============================================================================

start_udp_server(Port) ->
  Parent = self(),
  spawn_link(fun() -> server(Parent, Port) end),
  ok.

server(Parent, Port) ->
  {ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
  loop(Parent, Socket).

loop(Parent, Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {udp, Socket, _Host, _Port, Bin} ->
      Msg = parse_statsd(Bin),
      Parent ! {statsd_message, Msg},
      loop(Parent, Socket);
    Other ->
      ct:fail("unexpected message=~p",[Other])
  end.

parse_statsd(Bin) ->
  [Bkey, R] = binary:split(Bin, <<":">>),
  [Bval, BType] = binary:split(R, <<"|">>),
  Key = binary_to_list(Bkey),
  Val = binary_to_integer(Bval),
  Type = case BType of
           <<"c">> -> counter;
           <<"g">> -> gauge
         end,
  {Type, Key, Val}.


%% =============================================================================
%% Helper to test with a real statsd server
%% =============================================================================
%%
%% This is a helper test to help and debug connection to a real statsd server.
%% You should start your statsd server before running it.
%%
%% Run with rebar:
%%
%% $ rebar3 ct --suite apps/barrel_store/test/barrel_statsd_SUITE --case=send_to_statsd
%%
%% Tested with this graphite docker image:
%% https://github.com/hopsoft/docker-graphite-statsd
%% =============================================================================

send_to_statsd(_Config) ->
  Name = [<<"replication">>, <<"repid">>, <<"doc_reads">>],

  barrel_metrics:init(counter, Name),
  CreateMetrics = fun() ->
                      N = rand:uniform(100),
                      barrel_metrics:increment(Name, N),
                      timer:sleep(1000)
                  end,
  [CreateMetrics() || _ <- lists:seq(1,100)],
  ok.
