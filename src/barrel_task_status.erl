%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2016, Benoît Chesneau
%%% @doc A task is a process
%%%
%%% @end
%%% Created : 13. Apr 2016 10:24
%%%-------------------------------------------------------------------
-module(barrel_task_status).
-author("benoitc").

%% API
-export([all/0]).
-export([add_task/1, update/1, get/1]).
-export([is_task_added/0]).
-export([set_update_frequency/1]).


-define(STATUS_KEY, {p, l, task_status}).


all() ->
  AllTasks = gproc:select({local, props}, [{{'$1', '$2', '$3'},
    [{'==', {element, 3, '$1'}, task_status}], [{{'$2', '$3'}}]}]),
  [
    [{pid, list_to_binary(pid_to_list(Pid))} | TaskProps]
    ||
    {Pid, TaskProps} <- AllTasks
  ].

add_task(Props) ->
  put(task_status_update,{{0,0,0},0}),
  Ts = timestamp(),
  TaskProps = lists:ukeysort(1, [{started_on, Ts}, {updated_on, Ts} | Props]),
  erlang:put(task_status_props, TaskProps),

  case catch(gproc:reg(?STATUS_KEY, TaskProps)) of
    true -> ok;
    {'EXIT', {bararg, _}} -> {add_task_error, already_registered}
  end.

update(Props) ->
  MergeProps = lists:ukeysort(1, Props),
  TaskProps = lists:ukeymerge(1, MergeProps, erlang:get(task_status_props)),
  erlang:put(task_status_props, TaskProps),
  maybe_persist(TaskProps).

get(Props) when is_list(Props) ->
  TaskProps = gproc:get_value(?STATUS_KEY),
  [proplists:get_value(K, TaskProps) || K <- Props];
get(Prop) ->
  TaskProps = gproc:get_value(?STATUS_KEY),
  proplists:get_value(Prop, TaskProps).


is_task_added() ->
  is_list(erlang:get(task_status_props)).

set_update_frequency(MSecs) ->
  erlang:put(task_status_update, {{0, 0, 0}, MSecs * 1000}).



maybe_persist(TaskProps0) ->
  {LastUpdateTime, Frequency} = erlang:get(task_status_update),
  Now = os:timestamp(),
  UpdatedSince = timer:now_diff(Now, LastUpdateTime),
  if
    UpdatedSince >= Frequency ->
      erlang:put(task_status_update, {Now, Frequency}),
      TaskProps = lists:keystore(updated_on, 1, TaskProps0,
        {updated_on, timestamp(Now)}),
      gproc:set_value(?STATUS_KEY, TaskProps);
    true ->
      ok
  end.

timestamp() ->
  timestamp(os:timestamp()).

timestamp({Mega, Secs, _}) ->
  Mega * 1000000 + Secs.
