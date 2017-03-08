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
        , measures/1
        ]).


-export([init/3, increment/2]).

all() -> [ plugin
         , measures
         ].

env() ->
  [{plugin, barrel_stats_statsd},
   {statsd_server, {{127,0,0,1}, 8888}}].

init_per_suite(Config) ->
  start_udp_server(8888),
  application:stop(barrel_store),
  ok = application:set_env(barrel_store, metrics, env()),
  {ok, _} = application:ensure_all_started(barrel_store),
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
  application:stop(barrel_store),
  _ = (catch rocksdb:destroy("docs", [])),
  Config.

plugin(_Config) ->
  Name = [<<"replication">>, <<"repid">>, <<"doc_reads">>],
  barrel_metrics:init(counter, Name),
  barrel_metrics:increment(Name),

  Msgs = collect_messages(2),
  [ {plugin, init, {counter, Name}},
    {plugin, increment, Name} ] = Msgs,
  ok.

measures(_Config) ->
  {ok, _, _} = barrel_local:post(<<"testdb">>, #{<<"v">> => 42}, []),
  Msgs = collect_messages(1),
  [{plugin, increment, [ <<"dbs">>, <<"testdb">>, <<"doc_created">>]} ] = Msgs,
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

init(Type, Name, _Env) ->
  Pid = whereis(test_metric_testd),
  Pid ! {plugin, init, {Type, Name}},
  ok.

increment(Name, _Env) ->
  Pid = whereis(test_metric_testd),
  Pid ! {plugin, increment, Name},
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
  lager:info("statsd udp server opened socket:~p~n",[Socket]),
  loop(Parent, Socket).

loop(Parent, Socket) ->
  inet:setopts(Socket, [{active, once}]),
  ct:print("loop ~p", [Socket]),
  receive
    {udp, Socket, _Host, _Port, Bin} ->
      ct:print("msg=~p",[Bin]),
      Msg = parse_statsd(Bin),
      Parent ! {statsd_message, Msg},
      loop(Parent, Socket);
    Other ->
      ct:print("other=~p",[Other]),
      ct:fail("unexpected message=~p",[Other])
  end.

parse_statsd(Bin) ->
  [_Bhost, R1] = binary:split(Bin, <<".">>),
  [Bkey, R2] = binary:split(R1, <<":">>),
  [Bval, _] = binary:split(R2, <<"|">>),
  Key = binary_to_list(Bkey),
  Val = binary_to_integer(Bval),
  {{Key, gauge}, Val}.

