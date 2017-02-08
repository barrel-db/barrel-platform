-module(barrel_replicate).
-author("Benoit Chesneau").
-behaviour(gen_server).

-export([
  start_link/0,
  start_replication/2,
  stop_replication/1,
  delete_replication/1,
  where/1,
  replication_info/1
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

%% replication API
start_replication(Config, Options) when is_map(Config) ->
  Config2 = case maps:find(<<"replication_id">>, Config) of
              error ->
                RepId = barrel_lib:uniqid(),
                Config#{<<"replication_id">> => RepId};
              {ok, _} -> Config
            end,
  Config3 = Config2#{options => Options},
  case gen_server:call(?MODULE, {start_replication, Config3}) of
    ok -> {ok, Config2};
    Error -> Error
  end.

stop_replication(RepId) ->
  gen_server:call(?MODULE, {stop_replication, RepId}).

delete_replication(RepId) ->
  gen_server:call(?MODULE, {delete_replication, RepId}).


replication_info(RepId) ->
  case barrel_replicate:where(RepId) of
    Pid when is_pid(Pid) -> barrel_replicate_task:info(Pid);
    undefined -> {error, not_found}
  end.

where(RepId) ->
  case find_repid(RepId) of
    {ok, {_RepId, Pid, _Persisted}} -> Pid;
    undefined -> undefined
  end.


find_repid(RepId) ->
  case ets:lookup(replication_ids, RepId) of
    [] -> undefined;
    [{RepId, RepInfo}] -> {ok, RepInfo}
  end.


init([]) ->
  process_flag(trap_exit, true),
  %% _ = ets:new(replication_names, [ordered_set, named_table, public]),
  _ = ets:new(replication_ids, [set, named_table, public]),

  self() ! init_config,
  {ok, #{ config => maps:new() }}.


handle_call({start_replication, Config}, _From, State) ->
  {Reply, State2} =  maybe_start_replication(Config, State),
  {reply, Reply, State2};

handle_call({stop_replication, RepId}, _From, State) ->
  Reply =  do_stop_replication(RepId),
  {reply, Reply, State};
handle_call({delete_replication, RepId}, _From, State) ->
  {Reply, State2} =  do_delete_replication(RepId, State),
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
    fun(Pid) -> supervisor:terminate_child(barrel_replicate_task_sup, Pid) end,
    Pids
  ),
  ets:delete(replication_ids),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



config_file() ->
  case init:get_argument(replication_file) of
    {ok, [[P]]} -> P;
    _ ->
      FileName = case application:get_env(barrel_store, replication_file) of
                   undefined -> ?DEFAULT_CONFIG;
                   {ok, P} -> P
                 end,
      FullPath = filename:join(barrel_store:data_dir(), FileName),
      ok = filelib:ensure_dir(FullPath),
      FullPath
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
    fun(_RepId, RepConfig, St) ->
        {ok, St2} = do_start_replication(RepConfig, St),
        St2
    end,
    State,
    Config
  ),
  {ok, State2}.


maybe_start_replication(Config, State) ->
  #{<<"replication_id">> := RepId} = Config,
  case find_repid(RepId) of
    undefined ->
      do_start_replication(Config, State);
    {ok, {true, nil, nil}} ->
      do_start_replication(Config, State);
    {ok, {true, _Pid, _Ref}} ->
      {{error, {task_already_running, RepId}}, State}
  end.


do_start_replication(Config, State) ->
  #{<<"replication_id">> := RepId,
    <<"source">> := Source,
    <<"target">> := Target, options := Options} = Config,
  Persisted = proplists:get_value(persist, Options, false),
  {ok, Pid} = supervisor:start_child(
    barrel_replicate_task_sup,
    [RepId, Source, Target, Options]
  ),
  register_replication(RepId, Pid, Persisted, Config, State).

register_replication(RepId, Pid, Persisted, Config, #{ config := All} = State) ->
  MRef = erlang:monitor(process, Pid),
  ets:insert(
    replication_ids,
    [{Pid, {Persisted, RepId}},
     {RepId, {Persisted, Pid, MRef}}]
  ),

  State2 = case Persisted of
             true ->
               All2 = All#{ RepId => Config},
               ok = file:write_file(config_file(), io_lib:fwrite("~p.\n",[All2])),
               State#{ config := All2};
             false ->
               State
           end,
  {ok, State2}.


do_stop_replication(RepId) ->
  case find_repid(RepId) of
    undefined ->
      ok;
    {ok, {_, nil, _}} ->
      ok;
    {ok, {true, Pid, MRef}} ->
      ok = supervisor:terminate_child(barrel_replicate_task_sup, Pid),
      true = erlang:demonitor(MRef, [flush]),
      true = ets:delete(replication_ids, Pid),
      true = ets:insert(replication_ids, {RepId, {true, nil, nil}}),
      ok;
    {ok, {false, Pid, MRef}} ->
      ok = supervisor:terminate_child(barrel_replicate_task_sup, Pid),
      [{RepId, {_, Pid, MRef}}] = ets:take(replication_ids, RepId),
      true = erlang:demonitor(MRef, [flush]),
      true = ets:delete(replication_ids, Pid),
      ok
  end.


do_delete_replication(RepId, #{ config := Config} = State) ->
  ok = do_stop_replication(RepId),
  ets:delete(replication_ids, RepId),
  Config2 = maps:remove(RepId, Config),
  ok = file:write_file(config_file(), io_lib:fwrite("~p.\n",[Config2])),
  {ok, State#{ config => Config2} }.

task_is_down(Pid) ->
  case ets:take(replication_ids, Pid) of
    [] -> ok;
    [{Pid, {Name, Persisted, RepId}}] ->
      case Persisted of
        true ->
          ets:insert(replication_ids, {RepId, {Name, Persisted, nil, nil}});
        false ->
          ets:delete(replication_ids, RepId)
      end,
      ok
  end.
