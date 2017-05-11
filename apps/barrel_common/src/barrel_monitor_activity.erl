%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2017 05:29
%%%-------------------------------------------------------------------
-module(barrel_monitor_activity).
-author("benoitc").

%% API
-export([
  start/1,
  set_state/1,
  update/1
]).


%% different states:
%%
%% * idle
%% * active
%% * on_change


start(ClientInfo) ->
  Now = erlang:monotonic_time(),
  ok = barrel_task_status:add_task(barrel_stat_activity, ClientInfo#{ query_start => Now, state => idle }),
  barrel_task_status:set_update_interval(0).

set_state(NewState) ->
  barrel_task_status:update(barrel_stat_activity, #{ state => NewState }).

update(Props) ->
  barrel_task_status:update(barrel_stat_activity, Props).





