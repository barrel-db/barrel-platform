-module(barrel_replicate_manager).
-author("Benoit Chesneau").
-behaviour(gen_server).

-export([
  start_link/0,
  start_replication/2,
  stop_replication/1,
  delete_replication/1,
  where/1
]).

%% API
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  code_change/3,
  terminate/2
]).


-include_lib("stdlib/include/ms_transform.hrl").

-define(DEFAULT_CONFIG, "replication.config").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


start_replication(Name, Config) ->
  gen_server:call(?MODULE, {start_replication, Name, Config}).

stop_replication(Name) ->
  gen_server:call(?MODULE, {stop_replication, Name}).

delete_replication(Name) ->
  gen_server:call(?MODULE, {delete_replication, Name}).

where(Name) ->
  case find(Name) of
    {ok, {_RepId, Pid, _Persisted}} -> Pid;
    undefined -> undefined
  end.


find(Name) ->
  case ets:lookup(replication_names, Name) of
    [] -> undefined;
    [{Name, RepInfo}] -> {ok, RepInfo}
  end.

find_repid(RepId) ->
  case ets:lookup(replication_ids, RepId) of
    [] -> undefined;
    [{RepId, RepInfo}] -> {ok, RepInfo}
  end.


init([]) ->
  process_flag(trap_exit, true),
  _ = ets:new(replication_names, [ordered_set, named_table, public]),
  _ = ets:new(replication_ids, [set, named_table, public]),
  
  self() ! init_config,
  {ok, #{ config => maps:new() }}.


handle_call({start_replication, Name, Config}, _From, State) ->
  {Reply, State2} =  maybe_start_replication(Name, Config, State),
  {reply, Reply, State2};

handle_call({stop_replication, Name}, _From, State) ->
  Reply =  do_stop_replication(Name),
  {reply, Reply, State};
handle_call({delete_replication, Name}, _From, State) ->
  {Reply, State2} =  do_delete_replication(Name, State),
  {reply, Reply, State2}.


handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', _MRef, process, Pid, _Reason}, State) ->
  _ = task_is_down(Pid),
  {noreply, State};

handle_info(init_config, State) ->
  {ok, State2} = init_config(State),
  {noreply, State2};

handle_info(Info, State) ->
  lager:error("received an unknown message, exiting ~p~n", [Info]),
  {stop, normal, State}.

terminate(_Reason, _State) ->
  Spec = ets:fun2ms(fun({Pid, _}) when is_pid(Pid) -> Pid end),
  Pids = ets:select(replication_ids, Spec),
  lists:foreach(
    fun(Pid) -> supervisor:terminate_child(barrel_replicate_sup, Pid) end,
    Pids
  ),
  ets:delete(replication_names),
  ets:delete(replication_ids),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



config_file() ->
  case init:get_argument(replication_file) of
    {ok, [[P]]} -> P;
    _ ->
      case application:get_env(barrel, replication_file) of
        undefined -> filename:absname(?DEFAULT_CONFIG);
        {ok, P} -> P
      end
  end.

init_config(State) ->
  case read_file(config_file()) of
    {ok, [Config]} ->
      process_config(Config, State);
    {error, enoent} ->
      {ok, State};
    Error ->
      Error
  end.

read_file(Name) ->
  file:consult(Name).

process_config(Config, State) ->
  State2 = maps:fold(
    fun(Name, RepConfig, St) ->
      RepId = config_repid(RepConfig),
      {ok, St2} = do_start_replication(Name, RepId, RepConfig, St),
      St2
    end,
    State,
    Config
  ),
  {ok, State2}.


maybe_start_replication(Name, Config, State) ->
  RepId = config_repid(Config),
  case find(Name) of
    undefined ->
      case find_repid(RepId) of
        undefined ->
          do_start_replication(Name, RepId, Config, State);
        {ok, {Name, true, nil, nil}} ->
          do_start_replication(Name, RepId, Config, State);
        {ok, {Name, true, _Pid, _Ref}} ->
          {{error, {task_already_running, Name}}, State};
        {ok, {Other, _, _, _}} ->
          {{error, {task_already_registered, Other}}, State}
      end;
    {ok, {RepId, nil, true}} ->
      do_start_replication(Name, RepId, Config, State);
    {ok, {RepId, _Pid, _}} ->
      {ok, State};
    {ok, {_OtherRepId, Pid, _}} when is_pid(Pid) ->
      {{error, {task_already_registered, Pid}}, State}
  end.


config_repid(#{ source := Source, target := Target}) ->
  barrel_replicate:repid(Source, Target).

do_start_replication(Name, RepId, Config, State) ->
  #{ source := Source, target := Target, options := Options} = Config,
  Persisted = proplists:get_value(persist, Options, false),
  {ok, Pid} = supervisor:start_child(
    barrel_replicate_sup,
    [Name, Source, Target, Options]
  ),
  register_replication(Name, RepId, Pid, Persisted, Config, State).

register_replication(Name, RepId, Pid, Persisted, Config, #{ config := All} = State) ->
  MRef = erlang:monitor(process, Pid),
  ets:insert(replication_names, {Name, {RepId, Pid, Persisted}}),
  ets:insert(
    replication_ids,
    [{Pid, {Name, Persisted, RepId}}, {RepId, {Name, Persisted, Pid, MRef}}]
  ),
  
  State2 = case Persisted of
             true ->
               All2 = All#{ Name => Config},
               ok = file:write_file(config_file(), io_lib:fwrite("~p.\n",[All2])),
               State#{ config := All2};
             false ->
               State
           end,
  {ok, State2}.


do_stop_replication(Name) ->
  case find(Name) of
    undefined ->
      ok;
    {ok, {_RepId, nil, _Persisted}} ->
      ok;
    {ok, {RepId, Pid, Persisted}} ->
      ok = supervisor:terminate_child(barrel_replicate_sup, Pid),
      case Persisted of
        true ->
          [{RepId, {Name, _, _, MRef}}] = ets:lookup(replication_ids, RepId),
          erlang:demonitor(MRef, [flush]),
          ets:delete(replication_ids, Pid),
          ets:insert(replication_ids, {RepId, {Name, Persisted, nil, nil}}),
          ets:insert(replication_names, {Name, {RepId, nil, Persisted}});
        false ->
          [{RepId, {Name, _, Pid, MRef}}] = ets:take(replication_ids, RepId),
          erlang:demonitor(MRef, [flush]),
          ets:delete(replication_ids, Pid),
          ets:delete(replication_names, Name)
      end,
      ok
  end.


do_delete_replication(Name, #{ config := Config} = State) ->
  ok = do_stop_replication(Name),
  case ets:take(replication_names, Name) of
    [] -> {ok, State};
    [{Name, {RepId, _, _}}] ->
      ets:delete(replication_ids, RepId),
      Config2 = maps:remove(Name, Config),
      ok = file:write_file(config_file(), io_lib:fwrite("~p.\n",[Config2])),
      {ok, State#{ config => Config2} }
  end.

task_is_down(Pid) ->
  case ets:take(replication_ids, Pid) of
    [] -> ok;
    [{Pid, {Name, Persisted, RepId}}] ->
      case Persisted of
        true ->
          ets:insert(replication_ids, {RepId, {Name, Persisted, nil, nil}}),
          ets:insert(replication_names, {Name, {RepId, nil, Persisted}});
        false ->
          ets:delete(replication_names, Name),
          ets:delete(replication_ids, RepId)
      end,
      ok
  end.
