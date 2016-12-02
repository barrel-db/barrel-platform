-module(barrel_replicate_manager).
-author("Benoit Chesneau").
-behaviour(gen_server).

-export([
  start_link/0,
  register_task/2,
  unregister_task/1,
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

-define(DEFAULT_CONFIG, "replication.config").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


register_task(Name, RepId) ->
  gen_server:call(?MODULE, {register, Name, RepId, self()}).

unregister_task(Name) ->
  gen_server:call(?MODULE, {unregister, Name, self()}).

where(Name) ->
  case find(Name) of
    {ok, {_RepId, Pid}} -> Pid;
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
  _ = ets:new(replication_names, [ordered_set, named_table, public]),
  _ = ets:new(replication_ids, [set, named_table, public]),
  {ok, #{}}.


handle_call({register, Name, RepId, Pid}, _From, State) ->
  Reply = maybe_register(Name, RepId, Pid),
  {reply, Reply, State};

handle_call({unregister, Name, Pid}, _From, State) ->
  Reply = do_unregister(Name, Pid),
  {reply, Reply, State}.


handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', _MRef, process, Pid, _Reason}, State) ->
  _ = task_is_down(Pid),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


maybe_register(Name, RepId, Pid) ->
  case find(Name) of
    undefined ->
      case find_repid(RepId) of
        undefined ->
          do_register(Name, RepId, Pid);
        {Name, _Pid, _Ref} ->
          {error, {task_already_running, Name}}
      end;
    {ok, {RepId, _Pid}} ->
      ok;
    {ok, {_RepId, Pid}} when is_pid(Pid) ->
      {error, {task_already_registered, Pid}}
  end.


do_register(Name, RepId, Pid) ->
  MRef = erlang:monitor(process, Pid),
  ets:insert(replication_names, {Name, {RepId, Pid}}),
  ets:insert(
    replication_ids,
    [{Pid, {Name, RepId, MRef}}, {RepId, {Name, Pid, MRef}}]
  ),
  ok.

do_unregister(Name, Pid) ->
  case ets:take(replication_names, Name) of
    [] -> ok;
    [{Name, {RepId, Pid}}] ->
      [{RepId, {Name, Pid, MRef}}] = ets:take(replication_ids, RepId),
      ets:delete(replication_ids, Pid),
      erlang:demonitor(MRef, [flush]),
      ok;
    [_] ->
      {error, not_owner}
  end.

task_is_down(Pid) ->
  case ets:take(replication_ids, Pid) of
    [] -> ok;
    [{Pid, {Name, RepId}}] ->
      ets:delete(replication_names, Name),
      ets:delete(replication_ids, RepId),
      ok
  end.
