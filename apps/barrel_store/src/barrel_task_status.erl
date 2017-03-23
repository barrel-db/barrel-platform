
%% Copyright (c) 2016. Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% Created by benoitc on 17/06/16.

-module(barrel_task_status).
-author("Benoit Chesneau").
-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([all/0]).
-export([add_task/1, update/1, get/1]).
-export([is_task_added/0]).
-export([set_update_frequency/1]).

-export([start_link/0]).

%% internal
-export([stop/0]).

%% gen_server callbacks
-export([init/1,handle_call/3, handle_cast/2,  handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% Types
%%%===================================================================

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

all() ->
  AllTasks = ets:select(?MODULE, ets:fun2ms(fun({{t, Pid}, TaskProps}) -> {Pid, TaskProps} end)),
  [[{pid, list_to_binary(pid_to_list(Pid))} | TaskProps] || {Pid, TaskProps} <- AllTasks].

add_task(Props) ->
  put(task_status_update,{{0,0,0},0}),
  Ts = timestamp(),
  TaskProps = lists:ukeysort(1, [{started_on, Ts}, {updated_on, Ts} | Props]),
  erlang:put(task_status_props, TaskProps),

  case ets:insert_new(?MODULE, {{t, self()}, TaskProps}) of
    true ->
      gen_server:call(?MODULE, {monitor_me, self()}),
      ok;
    false ->
      {add_task_error, already_registered}
  end.


update(Props) ->
  MergeProps = lists:ukeysort(1, Props),
  TaskProps = lists:ukeymerge(1, MergeProps, erlang:get(task_status_props)),
  erlang:put(task_status_props, TaskProps),
  maybe_persist(TaskProps).

get(Props) when is_list(Props) ->
  [{_Pid, TaskProps}] = ets:lookup(?MODULE, {t, self()}),
  [proplists:get_value(K, TaskProps) || K <- Props];
get(Prop) ->
  [{_Pid, TaskProps}] = ets:lookup(?MODULE, {t, self()}),
  proplists:get_value(Prop, TaskProps).


is_task_added() ->
  is_list(erlang:get(task_status_props)).

set_update_frequency(MSecs) ->
  erlang:put(task_status_update, {{0, 0, 0}, MSecs * 1000}).


maybe_persist(TaskProps0) ->
  {LastUpdateTime, Frequency} = erlang:get(task_status_update),
  Now = erlang:timestamp(),
  UpdatedSince = timer:now_diff(Now, LastUpdateTime),
  if
    UpdatedSince >= Frequency ->
      erlang:put(task_status_update, {Now, Frequency}),
      TaskProps = lists:keystore(updated_on, 1, TaskProps0,
        {updated_on, timestamp(Now)}),
      ets:insert(?MODULE, {{t, self()}, TaskProps});
    true ->
      ok
  end.

-spec start_link() -> {ok, pid()}.
start_link() ->
  IsNew = create_tab(),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [IsNew], []).

create_tab() ->
  case ets:info(?MODULE, name) of
    undefined ->
      _ = ets:new(?MODULE, [named_table, public, ordered_set,
                            {read_concurrency, true},
                            {write_concurrency, true}]),
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
    true ->
      ok;
    false ->
      _ = init_monitors()
  end,
  {ok, #state{}}.

-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.
handle_call({monitor_me, Pid}, _From, State) ->
  erlang:monitor(process, Pid),
  ets:insert(?MODULE, {Pid, m}),
  {reply, ok, State};
handle_call(stop, _From, State) -> {stop, normal, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
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

  ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  ets:delete(?MODULE),
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

process_is_down(Pid) ->
  ets:delete(?MODULE, Pid),
  ets:delete(?MODULE, {t, Pid}).

init_monitors() ->
  Pids = ets:select(?MODULE, ets:fun2ms(fun({Pid, m}) -> Pid end)),
  [erlang:monitor(process, Pid) || Pid <- Pids].

timestamp() ->
  timestamp(erlang:timestamp()).

timestamp({Mega, Secs, _}) ->
  Mega * 1000000 + Secs.

stop() ->
  gen_server:call(?MODULE, stop).
