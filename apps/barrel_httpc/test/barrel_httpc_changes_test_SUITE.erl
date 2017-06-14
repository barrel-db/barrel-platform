%% Copyright 2017, Benoit Chesneau
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


-module(barrel_httpc_changes_test_SUITE).
-author("benoitc").


-define(DB_URL,<<"http://localhost:7080/dbs/testdb">>).
-define(SAMPLE_SIZE, 100).

-export([
  collect_change/1,
  include_doc/1,
  collect_changes/1,
  changes_feed_callback/1,
  restart_when_server_timeout/1,
  heartbeat_and_timeout/1,
  heartbeat_collect_change/1,
  multiple_put/1
]).

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2
]).

all() ->
  [
    collect_change,
    include_doc,
    collect_changes,
    changes_feed_callback,
    heartbeat_collect_change,
    heartbeat_and_timeout,
  	multiple_put,
    restart_when_server_timeout
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_rest),
  Config.

init_per_testcase(_, Config) ->
  ok = barrel_httpc:create_database(?DB_URL),
  {ok, Conn} = barrel_httpc:connect(?DB_URL),
  [{db, Conn} | Config].

end_per_testcase(_, _Config) ->
  ok = barrel_httpc:delete_database(?DB_URL),
  ok.

end_per_suite(Config) ->
  _ = application:stop(barrel_rest),
  Config.

db(Config) -> proplists:get_value(db, Config).

collect_change(Config) ->
  {ok, Pid} = barrel_httpc_changes:start_link(db(Config), #{since => 0, mode => sse}),
  [] = barrel_httpc_changes:changes(Pid),
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_httpc:post(db(Config), Doc, []),
  timer:sleep(100),
  [#{ <<"seq">> := 1, <<"id">> := <<"aa">>}] = barrel_httpc_changes:changes(Pid),
  [] = barrel_httpc_changes:changes(Pid),
  ok = barrel_httpc_changes:stop(Pid).

include_doc(Config) ->
  {ok, Pid} = barrel_httpc_changes:start_link(db(Config), #{since => 0, mode => sse, include_doc => true}),
  [] = barrel_httpc_changes:changes(Pid),
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_httpc:post(db(Config), Doc, []),
  timer:sleep(100),
  [#{ <<"seq">> := 1, <<"id">> := <<"aa">>, <<"doc">> := Doc2}] = barrel_httpc_changes:changes(Pid),
  #{ <<"id">> := <<"aa">>, <<"v">> := 1 } =  Doc2,
  [] = barrel_httpc_changes:changes(Pid),
  ok = barrel_httpc_changes:stop(Pid).

collect_changes(Config) ->
  {ok, Pid} = barrel_httpc_changes:start_link(db(Config), #{since => 0, mode => sse}),
  [] = barrel_httpc_changes:changes(Pid),
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _} = barrel_httpc:post(db(Config), Doc, []),
  timer:sleep(100),
  [#{ <<"seq">> := 1, <<"id">> := <<"aa">>}] = barrel_httpc_changes:changes(Pid),
  [] = barrel_httpc_changes:changes(Pid),
  Doc2 = #{ <<"id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, _} = barrel_httpc:post(db(Config), Doc2, []),
  {ok, _, _} = barrel_httpc:get(db(Config), <<"bb">>, []),
  timer:sleep(100),
  [#{ <<"seq">> := 2, <<"id">> := <<"bb">>}] = barrel_httpc_changes:changes(Pid),
  [] = barrel_httpc_changes:changes(Pid),
  Doc3 = #{ <<"id">> => <<"cc">>, <<"v">> => 1},
  Doc4 = #{ <<"id">> => <<"dd">>, <<"v">> => 1},
  {ok, <<"cc">>, _} = barrel_httpc:post(db(Config), Doc3, []),
  {ok, <<"dd">>, _} = barrel_httpc:post(db(Config), Doc4, []),
  {ok, _, _} = barrel_httpc:get(db(Config), <<"cc">>, []),
  {ok, _, _} = barrel_httpc:get(db(Config), <<"dd">>, []),
  timer:sleep(100),
  [
    #{ <<"seq">> := 3, <<"id">> := <<"cc">>},
    #{ <<"seq">> := 4, <<"id">> := <<"dd">>}
  ] = barrel_httpc_changes:changes(Pid),
  ok = barrel_httpc_changes:stop(Pid).

changes_feed_callback(Config) ->
  Self = self(),
  Callback =
    fun(Change) ->
      Self ! {change, Change}
    end,
  Options = #{since => 0, mode => sse, changes_cb => Callback },
  {ok, Pid} = barrel_httpc_changes:start_link(db(Config), Options),
  Doc1 = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  Doc2 = #{ <<"id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"aa">>, _} = barrel_httpc:post(db(Config), Doc1, []),
  {ok, <<"bb">>, _} = barrel_httpc:post(db(Config), Doc2, []),
  {ok, _, _} = barrel_httpc:get(db(Config), <<"aa">>, []),
  {ok, _, _} = barrel_httpc:get(db(Config), <<"bb">>, []),
  timer:sleep(100),
  [
    #{ <<"seq">> := 1, <<"id">> := <<"aa">>},
    #{ <<"seq">> := 2, <<"id">> := <<"bb">>}
  ] = collect_changes(2, queue:new()),
  ok = barrel_httpc_changes:stop(Pid).


restart_when_server_timeout(Config) ->
  Self = self(),
  Callback =
    fun(Change) ->
        Self ! {change, Change}
    end,
	Val = <<"AACC">>,
  Options = #{since => 0, mode => sse, changes_cb => Callback, retry_timeout=>200},
  {ok, Pid} = barrel_httpc_changes:start_link(db(Config), Options),
  Doc1 = #{ <<"id">> => Val, <<"v">> => 1},
  {ok, Val, _} = barrel_httpc:post(db(Config), Doc1, []),

  ok = application:stop(barrel_rest),
  timer:sleep(500),
  ok = application:start(barrel_rest),
  timer:sleep(1500),

  Doc2 = #{ <<"id">> => <<"bb">>, <<"v">> => 1},
  {ok, <<"bb">>, _} = barrel_httpc:post(db(Config), Doc2, []),

	true =	test_util:wait_for_event(50, fun() ->
																			 case collect_changes(2, queue:new()) of

																					 [
																						#{ <<"seq">> := 1, <<"id">> := Val},
																						#{ <<"seq">> := 2, <<"id">> := <<"bb">>}
																					 ] -> ok;
																					 [{error,timeout}] ->
																							 false
																			 end
															 end, 50),
  ok = application:stop(barrel_rest),
  [{error, timeout}] = collect_changes(1, queue:new()),
  ok = barrel_httpc_changes:stop(Pid),
  ok = application:start(barrel_rest),
  ok.

heartbeat_collect_change(Config) ->
  Options = #{since => 0, heartbeat => 100, mode => sse, retry_timeout => 500},
  {ok, Pid} = barrel_httpc_changes:start_link(db(Config), Options),
  timer:sleep(500),
  [] = barrel_httpc_changes:changes(Pid),
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_httpc:post(db(Config), Doc, []),
  timer:sleep(100),
  [#{ <<"seq">> := 1, <<"id">> := <<"aa">>}] = barrel_httpc_changes:changes(Pid),
  [] = barrel_httpc_changes:changes(Pid),
  ok = barrel_httpc_changes:stop(Pid).

heartbeat_and_timeout(Config) ->
  io:format("Config = ~p.",[Config]),
  process_flag(trap_exit, true),

  %% httpc will timeout before receiving the heartbeat
  Options1 = #{since => 0, timeout => 50, heartbeat => 100, mode => sse, retry_timeout => 500 },
  {ok, Pid1} = barrel_httpc_changes:start_link(db(Config), Options1),
  ok = receive
         {'EXIT', Pid1, timeout} ->
           ok;
         Unexpected ->
           {unexpected, Unexpected}
       after 2000 ->
           {error, test_timeout}
       end,

  %% timeout is larger than heartbeat. We will keep receiving changes.
  Options2 = #{since => 0, timeout => 200, heartbeat => 100, mode => sse, retry_timeout => 1000 },
  {ok, Pid2} = barrel_httpc_changes:start_link(db(Config), Options2),
  [] = barrel_httpc_changes:changes(Pid2),
  Doc = #{ <<"id">> => <<"aa">>, <<"v">> => 1},
  {ok, <<"aa">>, _RevId} = barrel_httpc:post(db(Config), Doc, []),
  timer:sleep(100),
  [#{ <<"seq">> := 1, <<"id">> := <<"aa">>}] = barrel_httpc_changes:changes(Pid2),
  [] = barrel_httpc_changes:changes(Pid2),

  %% httpc did not timeout
  ok = receive
         {'EXIT', Pid1, timeout} ->
           {error, timeout_received};
         Other ->
           {unexpected, Other}
       after 0 ->
           ok
       end,
  ok = barrel_httpc_changes:stop(Pid2),
  ok.



multiple_put(Config) ->
  Self = self(),
  timer:sleep(500),
  lager:notice("Config = ~p.~n~n~n", [Config]),
  %% spawn a change listener
  spawn(
    fun() ->
      ChangePid = self(),
      Callback =
      fun(Change) ->
        ChangePid ! {change, Change}
      end,
      Options = #{since => 0, mode => sse, changes_cb => Callback },
      {ok, _Pid} = barrel_httpc_changes:start_link(db(Config), Options),
      Changes = collect_changes(?SAMPLE_SIZE, queue:new()),
      Self ! {changes, Changes}
    end
  ),

  %% write docs
  Pids = lists:foldl(
    fun(I, Acc) ->
      DocId = << "doc", (integer_to_binary(I))/binary >>,
      Doc = #{ <<"id">> => DocId, <<"val">> => I},
      Pid = spawn_link(
        fun() ->
          {ok, DocId, _} = barrel_httpc:post(db(Config), Doc, []),
          Self ! {ok, self()},
          timer:sleep(200)
        end
      ),
      [Pid | Acc]
    end,
    [],
    lists:seq(1, ?SAMPLE_SIZE)
  ),

  ?SAMPLE_SIZE = length(Pids),
  ok = wait_pids(Pids),

  #{ <<"docs_count">> := ?SAMPLE_SIZE } = barrel_httpc:database_infos(?DB_URL),
  receive
    {changes, Changes} ->
      case length(Changes) of
        ?SAMPLE_SIZE -> ok;
        Other ->
          ct:fail("bad changes count=~p",[Other])
      end
  end,
  ok.


collect_changes(0, Q) ->
  queue:to_list(Q);
collect_changes(I, Q) ->
  receive
    {change, Change} ->
      collect_changes(I-1, queue:in(Change, Q))
  after 500 ->
      collect_changes(0, queue:in({error, timeout}, Q))
  end.

wait_pids([]) -> ok;
wait_pids(Pids) ->
  receive
    {ok, Pid} -> wait_pids(Pids -- [Pid])
  after 5000 -> {error, receive_pids}
  end.
