%% Copyright 2016, Benoit Chesneau
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

-module(barrel_event_local).
-author("Benoit Chesneau").
-behaviour(gen_server).


%% API
-export([
  subscribe/2,
  unsubscribe/2,
  broadcast/2
]).


-export([
  start_link/1,
  poolsize/0,
  shard_name/1
]).

%% gen server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).


-include_lib("stdlib/include/ms_transform.hrl").

-define(DEFAULT_POOL_SIZE, 5).

broadcast(Topic, Event) ->
  Ref = erlang:make_ref(),
  Parent = self(),
  Pids = lists:map(
    fun(I) ->
      Shard = shard_name(I),
      spawn_link(
        fun() ->
          broadcast(Ref, Parent, Shard, Topic, Event)
        end
      )
    end,
    lists:seq(1, poolsize())
  ),
  [await(Pid, Ref) || Pid <- Pids],
  ok.




subscribe(Pid, Topic) ->
  gen_server:call(local_server(Pid), {subscribe, Pid, Topic}).


unsubscribe(Pid, Topic) ->
  unsubscribe(local_server(Pid), Topic).


%% internal API

local_server(Pid) ->
  shard_name(1 + erlang:phash2(Pid, poolsize())).


shard_name(I) ->
  barrel_lib:to_atom("barrel_event_local-" ++integer_to_list(I)).

poolsize() ->
  application:get_env(barrel, events_pool_size, ?DEFAULT_POOL_SIZE).


start_link(Name) ->
  IsNew = init_tab(Name),
  gen_server:start_link({local, Name}, ?MODULE, [Name, IsNew], []).

init_tab(Name) ->
  case ets:info(Name, name) of
    undefined ->
      ets:new(Name, [named_table, public, ordered_set]),
      true;
    _ ->
      false
  end.


init([Name, IsNew]) ->
  case IsNew of
    true -> ok;
    false -> init_monitors(Name)
  end,
  {ok, Name}.

terminate(_Reason, State) ->
  _ = ets:delete(State),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


handle_call({subscribe, Pid, Topic}, _From, State) ->
  do_subscribe(Pid, Topic, State),
  {reply, ok, State};
handle_call({unsubscribe, Pid, Topic}, _From, State) ->
  do_unsubscribe(Pid, Topic, State),
  {reply, ok, State};
handle_call(_Msg, _From, State) ->
  {reply, bad_call, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({'DOWN', _, _, Pid, _}, State) ->
  process_is_down(Pid, State),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.


do_subscribe(Pid, Topic, State) ->
  Key = {Topic, Pid},
  ets:insert(State, {Key, Pid}),
  _ = maybe_monitor(Pid, State),
  ok.

maybe_monitor(Pid, State) ->
  case ets:insert_new(State, {Pid, nil, 0}) of
    true ->
      MRef = erlang:monitor(process, Pid),
      ets:insert(State, {Pid, MRef, 1});
    false ->
      _ = ets:update_counter(State, Pid, {3, 1})
  end.


do_unsubscribe(Pid, Topic, State) ->
  Key = {Topic, Pid},
  ets:delete(State, Key),
  _ = maybe_demonitor(Pid, State),
  ok.

maybe_demonitor(Pid, State) ->
  case ets:lookup(State, Pid) of
    [{Pid, MRef, _N}] ->
      case ets:update_counter(State, Pid, {3, -1}) of
        0 -> erlang:demonitor(MRef, [flush]);
        _ -> ok
      end;
    [] -> ok
  end.

process_is_down(Pid, State) ->
  case ets:lookup(State, Pid) of
    [] -> ok;
    [_] ->
      ets:delete(State, Pid),
      Pattern = ets:fun2ms(fun({{Key, P}, _}) when P =:= Pid -> {Key, P} end),
      Subs = ets:select(State, Pattern),
      lists:foreach(fun(Sub) -> ets:delete(State, Sub) end, Subs)
  end.

init_monitors(State) ->
  Pattern = ets:fun2ms(fun({Pid, {_, N}}) when is_pid(Pid) -> {Pid, N} end),
  Pids = ets:select(State, Pattern),
  lists:foreach(
    fun({Pid, N}) ->
      MRef = erlang:monitor(process, Pid),
      ets:insert(State, {Pid, {MRef, N}})
    end, Pids).


await(Pid, Ref) ->
  MRef = erlang:monitor(process, Pid),
  receive
    {Ref, broadcast_done} ->
      erlang:demonitor(MRef, [flush]),
      ok;
    {'DOWN', MRef, _, Reason} ->
      exit(Reason)
  after 5000 ->
    exit(timeout)
  end.


broadcast(Ref, Parent, Shard, {db, Store, DbName}, Event) ->
  Pattern = ets:fun2ms(
    fun({{{db, S, Db}, P}, _})
         when (S =:= Store) andalso (Db =:= '*' orelse Db =:= DbName) ->
      P
    end
  ),
  Subs = ets:select(Shard, Pattern),
  lists:foreach(
    fun(Pid) -> Pid ! {barrel_db_event, {Event, Store, DbName}} end,
    Subs
  ),
  Parent ! {Ref, broadcast_done},
  ok.
