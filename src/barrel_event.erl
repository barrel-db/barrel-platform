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

%% Created by benoitc on 14/06/16.

-module(barrel_event).
-author("Benoit Chesneau").
-behaviour(gen_server).

%% API
-export([notify/2, notify/3]).

-export([reg/1]).
-export([unreg/0]).
-export([mreg/1]).
-export([reg_all/0]).
-export([reg_index/2]).
-export([mreg_index/1]).
-export([reg_all_index/0]).
-export([reg_other/2]).
-export([unreg_other/1]).
-export([mreg_other/2]).
-export([reg_all_other/1]).
-export([reg_index_other/3]).
-export([mreg_index_other/2]).
-export([reg_all_index_other/1]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(SERVER, ?MODULE).

-define(TAB, ?MODULE).

%%%===================================================================
%%% Types
%%%===================================================================

-type state() :: #{}.

%%%===================================================================
%%% API
%%%===================================================================

notify(DbName, Event) -> cast({notify, DbName, Event}).

notify(DbName, DDoc, Event) -> cast({notify, DbName, DDoc, Event}).

reg(DbName) -> mreg([DbName]).

mreg(DbNames) when is_list(DbNames) -> call({register_dbs, self(), DbNames}).

reg_all() -> mreg(['all_dbs']).

reg_index(DbName, DDoc) ->  mreg_index([{DbName, DDoc}]).

mreg_index(Indexes) -> call({register_indexes, self(), Indexes}).

reg_all_index() -> mreg_index(['all_indexes']).


reg_other(DbName, Pid) -> mreg_other([DbName], Pid).

mreg_other(DbNames, Pid) when is_list(DbNames), is_pid(Pid) -> call({register_dbs, Pid, DbNames}).

reg_all_other(Pid) ->mreg_other(['all_dbs'], Pid).

reg_index_other(DbName, DDoc, Pid) -> mreg_index_other([{DbName, DDoc}], Pid).

mreg_index_other(Indexes, Pid) when is_list(Indexes), is_pid(Pid) -> call({register_indexes, Pid, Indexes}).

reg_all_index_other(Pid) -> mreg_index_other(['all_indexes'], Pid).

unreg() -> call({unregister, self()}).

unreg_other(Pid) when is_pid(Pid) -> call({unregister, Pid}).


-spec start_link() -> {ok, pid()}.
start_link() ->
  IsNew = init_tab(),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [IsNew], []).

init_tab() ->
  case ets:info(?TAB, name) of
    undefined ->
      ets:new(?TAB, [named_table, public, ordered_set]),
      true;
    _ ->
      false
  end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%%
-spec init(term()) -> {ok, state()}.
init([IsNew]) ->
  case IsNew of
    true -> ok;
    false -> init_monitors()
  end,
  {ok, #{}}.

-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.
handle_call({register_dbs, Pid,  DbNames}, _From, State) ->
  do_register_dbs(Pid, DbNames),
  {reply, ok, State};

handle_call({register_indexes, Pid, Indexes}, _From, State) ->
  do_register_indexes(Pid, Indexes),
  {reply, ok, State};

handle_call({unregister, Pid}, _From, State) ->
  do_unregister(Pid),
  {reply, ok, State};

handle_call(_Msg, _From, State) ->
  {reply, bad_call, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast({notify, DbName, Event}, State) ->
  notify_listeners(DbName, Event),
  {noreply, State};

handle_cast({notify, DbName, DDoc, Event}, State) ->
  notify_listeners(DbName, DDoc, Event),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info({'DOWN', _, _, Pid, _}, State) ->
  process_is_down(Pid),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
  ets:delete(?TAB),
  ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

cast(Msg) -> gen_server:cast(?MODULE, Msg).
call(Msg) -> gen_server:call(?MODULE, Msg).


notify_listeners(DbName, Event) ->
  Msg = {'$barrel_event', DbName, Event},
  Pattern = ets:fun2ms(fun({{{db, Db}, _}, Pid}) when Db =:= DbName; Db =:= 'all_dbs' -> Pid end),
  Subs = ets:select(?TAB, Pattern),
  lists:foreach(fun(Pid) -> Pid ! Msg end, Subs).

notify_listeners(DbName, DDoc, Event) ->
  Msg = {'$barrel_event', DbName, DDoc, Event},
  Key = {g, {DbName, DDoc}},
  Pattern = ets:fun2ms(fun({{K, _}, Pid}) when K =:= Key; K =:= 'all_indexes' -> Pid end),
  Subs = ets:select(?TAB, Pattern),
  lists:foreach(fun(Pid) -> Pid ! Msg end, Subs).


do_register_dbs(Pid, DbNames) ->
  lists:foreach(fun(DbName) ->
      Key = {{db, DbName}, Pid},
      ets:insert(?TAB, {Key, Pid})
    end, DbNames),
  maybe_monitor(Pid).

maybe_monitor(Pid) ->
  %% only monitor once a process
  case ets:lookup(?TAB, Pid) of
    [] ->
      MRef = erlang:monitor(process, Pid),
      ets:insert(?TAB, {Pid, MRef}),
      ok;
    [_] ->
      ok
  end.

do_register_indexes(Pid, Indexes) ->
  lists:foreach(fun(Index) ->
      Key = {{g, Index}, Pid},
      ets:insert(?TAB, {Key, Pid})
    end, Indexes),
  maybe_monitor(Pid).

do_unregister(Pid) ->
  case ets:lookup(?TAB, Pid) of
    [] -> ok;
    [{Pid, MRef}] ->
      erlang:demonitor(process, MRef),
      Pattern = ets:fun2ms(fun({{K, P}, _}) when P =:= Pid -> {K, P} end),
      Subs = ets:select(?TAB, Pattern),
      lists:foreach(fun(Sub) -> ets:delete(?TAB, Sub) end, Subs)
  end.

process_is_down(Pid) ->
  case ets:lookup(?TAB, Pid) of
    [] -> ok;
    [_] ->
      ets:delete(?TAB, Pid),
      Pattern = ets:fun2ms(fun({{Key, P}, _}) when P =:= Pid -> {Key, P} end),
      Subs = ets:select(?TAB, Pattern),
      lists:foreach(fun(Sub) -> ets:delete(?TAB, Sub) end, Subs)
  end.

init_monitors() ->
  Pattern = ets:fun2ms(fun({Pid, _}) when is_pid(Pid) -> Pid end),
  Pids = ets:select(?TAB, Pattern),
  lists:foreach(fun(Pid) ->
      MRef = erlang:monitor(process, Pid),
      ets:insert(?TAB, {Pid, MRef})
    end, Pids).
