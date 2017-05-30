%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2017 11:29
%%%-------------------------------------------------------------------
-module(barrel_task_status).
-author("benoitc").

%% API
-export([
  add_task/2,
  update/2,
  set_update_interval/1,
  maybe_persists/2,
  all/1
]).

-define(DEFAULT_INTERVAL, 500).

-include_lib("gproc/include/gproc.hrl").


add_task(Name, Props) ->
  TS = erlang:monotonic_time(),
  UpdateInterval = application:get_env(barrel_stats, update_interval, ?DEFAULT_INTERVAL),
  erlang:put(task_status_update, {TS, UpdateInterval}),
  true = gproc:reg({p, l, Name}, Props#{ start_time => TS * 1000, last_update => TS * 1000 }),
  erlang:put(task_status_props, Props),
  ok.

update(Name, Props0) ->
  Props1 = maps:merge(erlang:get(task_status_props), Props0),
  erlang:put(task_status_props, Props1),
  maybe_persists(Name, Props1).
  
set_update_interval(Interval) ->
  TS = erlang:monotonic_time(),
  erlang:put(task_status_update, {TS, Interval * 1000}),
  ok.
  
maybe_persists(Name, Props) ->
  {T1, Interval} = erlang:get(task_status_update),
  T2 = erlang:monotonic_time(),
  Time = erlang:convert_time_unit(T2 - T1, native, microsecond),
  if
    (Time >= Interval) ->
      erlang:put(task_status_update, {T2, Interval}),
      true = gproc:set_value({p, l, Name}, Props#{ last_update => T2 * 1000 }),
      ok;
    true ->
      ok
  end.

all(Name) ->
  [Props#{ pid => Pid} || {Pid, Props} <- gproc:lookup_values({p, l, Name})].