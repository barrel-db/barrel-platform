%% Copyright 2016, Bernard Notarianni
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

-module(barrel_replicate).
-author("Bernard Notarianni").

-behaviour(gen_server).

%% specific API
-export([
  start_link/4,
  info/1
]).

%% gen_server API
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%% internal api
-export([
  repid/2,
  replication_key/1,
  clean/2
]).


-record(st, { source
            , target
            , id          ::binary()  % replication id
            , session_id  ::binary()  % replication session (history) id
            , start_seq=0 ::integer() % start seq for current repl session
            , last_seq=0  ::integer() % last received seq from source
            , target_seq  ::integer() % target seq for current repl session
            , metrics
            , options
            }).

start_link(RepId, Source, Target, Options) ->
  gen_server:start_link(
    {via, gproc, replication_key(RepId)}, ?MODULE,
    {Source, Target, Options}, []).


info(Pid) when is_pid(Pid)->
  gen_server:call(Pid, info).

clean(Source, Target) ->
  RepId = repid(Source, Target),
  delete_checkpoint_doc(Source, RepId),
  delete_checkpoint_doc(Target, RepId).

%% @doc Compute a unique ID for replication
%% function of Source, Target, and unique ID of the server
%% TODO compute unique server ID
repid(Source, Target) ->
  {ok, HostName} = inet:gethostname(),
  Term = {Source, Target, HostName},
  H = erlang:phash2(Term),
  Md5 = erlang:md5(integer_to_binary(H)),
  barrel_lib:to_hex(Md5).


replication_key(RepId) -> {n, l, {barrel_replicate, RepId}}.
  
init({Source, Target, Options}) ->
  RepId = repid(Source, Target),
  Metrics = barrel_metrics:new(),
  StartSeq = checkpoint_start_seq(Source, Target, RepId),
  {ok, LastSeq, Metrics2} = replicate_change(Source, Target, StartSeq, Metrics),
  ok = barrel_event:reg(Source),
  State = #st{source=Source,
              target=Target,
              id=RepId,
              session_id = barrel_lib:uniqid(binary),
              start_seq=StartSeq,
              last_seq=LastSeq,
              metrics=Metrics2,
              options=Options},
  State2 = compute_target_seq(State),
  ok = barrel_metrics:create_task(Metrics2, Options),
  barrel_metrics:update_task(Metrics2),
  {ok, State2}.

handle_call(info, _From, State) ->
  RepId = State#st.id,
  Source = State#st.source,
  History = case read_checkpoint_doc(Source, RepId) of
              {ok, Doc} ->
                maps:get(<<"history">>, Doc);
              _Other ->
                []
            end,
  Info = #{ id => State#st.id
          , source => State#st.source
          , target => State#st.target
          , last_seq => State#st.last_seq
          , metrics => State#st.metrics
          , checkpoints => History
          },

  {reply, Info, State};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State}.

handle_cast(shutdown, State) ->
  {stop, normal, State}.

handle_info({'$barrel_event', _, db_updated}, S) ->
  Source = S#st.source,
  Target = S#st.target,
  From = S#st.last_seq,
  Metrics = S#st.metrics,
  {ok, LastSeq, Metrics2} = replicate_change(Source, Target, From, Metrics),
  S2 = S#st{last_seq=LastSeq, metrics=Metrics2},
  S3 = maybe_write_checkpoint(S2),
  barrel_metrics:update_task(Metrics2),
  {noreply, S3}.

%% default gen_server callback
terminate(_Reason, State) ->
  barrel_metrics:update_task(State#st.metrics),
  ok = write_checkpoint(State),
  ok = barrel_event:unreg(),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% =============================================================================
%% Replicate changes from source to target
%% =============================================================================

replicate_change(Source, Target, StartSeq, Metrics) ->
  {LastSeq, Changes} = changes(Source, StartSeq),
  Results = maps:get(<<"results">>, Changes),
  Results = maps:get(<<"results">>, Changes) ,
  {ok, Metrics2} = lists:foldl(fun(C, {ok, Acc}) ->
                           sync_change(Source, Target, C, Acc)
                       end, {ok, Metrics}, Results),
  {ok, LastSeq, Metrics2}.

sync_change(Source, Target, Change, Metrics) ->
  Id = maps:get(id, Change),
  RevTree = maps:get(revtree, Change),
  CurrentRev = maps:get(current_rev, Change),
  History = history(CurrentRev, RevTree),

  {Doc, Metrics2} = read_doc(Source, Id, Metrics),
  Metrics3 = write_doc(Target, Id, Doc, History, Metrics2),

  {ok, Metrics3}.


read_doc(Source, Id, Metrics) ->
  Get = fun() -> get(Source, Id, []) end,
  case timer:tc(Get) of
    {Time, {ok, Doc}} ->
      Metrics2 = barrel_metrics:inc(docs_read, Metrics, 1),
      Metrics3 = barrel_metrics:update_times(doc_read_times, Time, Metrics2),
      {Doc, Metrics3};
    _ ->
      lager:error("replicate read error on dbid=~p for docid=~p", [Source, Id]),
      Metrics2 = barrel_metrics:inc(doc_read_failures, Metrics, 1),
      {undefined, Metrics2}
    end.

write_doc(_, _, undefined, _, Metrics) ->
  Metrics;
write_doc(Target, Id, Doc, History, Metrics) ->
  PutRev = fun() -> put_rev(Target, Id, Doc, History, []) end,
  case timer:tc(PutRev) of
    {Time, {ok, _, _}} ->
      Metrics2 = barrel_metrics:inc(docs_written, Metrics, 1),
      Metrics3 = barrel_metrics:update_times(doc_write_times, Time, Metrics2),
      Metrics3;
    _ ->
      lager:error("replicate write error on dbid=~p for docid=~p", [Target, Id]),
      barrel_metrics:inc(doc_write_failures, Metrics, 1)
  end.

changes(Source, Since) ->
  Fun = fun(Seq, DocInfo, _Doc, {PreviousLastSeq, DocInfos}) ->
            LastSeq = max(Seq, PreviousLastSeq),
            {ok, {LastSeq, [DocInfo|DocInfos]}}
        end,
  {LastSeq, Changes} = changes_since(Source, Since, Fun, {Since, []}),
  {LastSeq, #{<<"last_seq">> => LastSeq,
              <<"results">> => Changes}}.

get({Mod,_Db}=DbRef, Id, Opts) ->
  Mod:get(DbRef, Id, Opts);
get(Db, Id, Opts) when is_binary(Db) ->
  barrel_db:get(Db, Id, Opts).

%% put({Mod,_Db}=DbRef, Id, Doc, Opts) ->
%%   Mod:put(DbRef, Id, Doc, Opts);
%% put(Db, Id, Doc, Opts) when is_binary(Db) ->
%%   barrel_db:put(Db, Id, Doc, Opts).

put_rev({Mod,_Db}=DbRef, Id, Doc, History, Opts) ->
  Mod:put_rev(DbRef, Id, Doc, History, Opts);
put_rev(Db, Id, Doc, History, Opts) when is_binary(Db) ->
  barrel_db:put_rev(Db, Id, Doc, History, Opts).

changes_since({Mod,_Db}=DbRef, Since, Fun, Acc) ->
  Mod:changes_since(DbRef, Since, Fun, Acc);
changes_since(Db, Since, Fun, Acc) when is_binary(Db) ->
  barrel_db:changes_since(Db, Since, Fun, Acc).

%% =============================================================================
%% Checkpoints management: when, where and what
%% =============================================================================

%% @doc Decide it we should write checkpoints, and do it.
maybe_write_checkpoint(#st{last_seq=L, target_seq=T}=S) when L >= T ->
  write_checkpoint(S),
  compute_target_seq(S);
maybe_write_checkpoint(State) ->
  State.

compute_target_seq(State) ->
  LastSeq = State#st.last_seq,
  CheckpointSize = proplists:get_value(checkpoint_size, State#st.options, 10),
  TargetSeq = LastSeq + CheckpointSize,
  State#st{target_seq=TargetSeq}.


%% @doc Write checkpoint information on both source and target databases.
write_checkpoint(State) ->
  StartSeq = State#st.start_seq,
  LastSeq = State#st.last_seq,
  Source = State#st.source,
  Target = State#st.target,
  Checkpoint = #{<<"source_last_seq">>   => LastSeq
                ,<<"source_start_seq">>  => StartSeq
                ,<<"session_id">> => State#st.session_id
                ,<<"end_time">> => timestamp()
                ,<<"end_time_microsec">> => erlang:system_time(micro_seconds)
                },
  RepId = State#st.id,
  add_checkpoint(Source, RepId, Checkpoint),
  add_checkpoint(Target, RepId, Checkpoint),
  ok.

add_checkpoint(Db, RepId, Checkpoint) ->
  Doc = case read_checkpoint_doc(Db, RepId) of
          {ok, PreviousDoc} ->
            H = maps:get(<<"history">>, PreviousDoc),
            PreviousDoc#{<<"history">> => [Checkpoint|H]};
          _ ->
            #{<<"history">> => [Checkpoint]}
        end,
  write_checkpoint_doc(Db, RepId, Doc).

%% @doc Compute replication starting seq from checkpoints history
checkpoint_start_seq(Source, Target, RepId) ->
  LastSeqSource = read_last_seq(Source, RepId),
  LastSeqTarget = read_last_seq(Target, RepId),
  min(LastSeqTarget, LastSeqSource).

read_last_seq(Db, RepId) ->
  case read_checkpoint_doc(Db, RepId) of
    {ok, Doc} ->
      History = maps:get(<<"history">>, Doc),
      Sorted = lists:sort(fun(H1,H2) ->
                              T1 = maps:get(<<"end_time_microsec">>, H1),
                              T2 = maps:get(<<"end_time_microsec">>, H2),
                              T1 > T2
                          end, History),
      LastHistory = hd(Sorted),
      maps:get(<<"source_last_seq">>, LastHistory);
    {error, not_found} ->
      0;
    Other ->
      lager:error("replication cannot read checkpoint on ~p: ~p", [Db, Other]),
      0
  end.

write_checkpoint_doc(Db, RepId, Checkpoint) ->
  barrel_db:write_system_doc(Db, checkpoint_docid(RepId), Checkpoint).

read_checkpoint_doc(Db, RepId) ->
  barrel_db:read_system_doc(Db, checkpoint_docid(RepId)).

delete_checkpoint_doc(Db, RepId) ->
  barrel_db:delete_system_doc(Db, checkpoint_docid(RepId)).

checkpoint_docid(RepId) ->
  <<"replication-checkpoint-", RepId/binary>>.

%% =============================================================================
%% Helpers
%% =============================================================================

history(Id, RevTree) ->
  history(Id, RevTree, []).
history(<<>>, _RevTree, History) ->
  lists:reverse(History);
history(Rev, RevTree, History) ->
  DocInfo = maps:get(Rev, RevTree),
  Parent = maps:get(parent, DocInfo),
  history(Parent, RevTree, [Rev|History]).


%% RFC3339 timestamps.
%% Note: doesn't include the time seconds fraction (RFC3339 says it's optional).
timestamp() ->
  {{Year, Month, Day}, {Hour, Min, Sec}} =
    calendar:now_to_local_time(erlang:timestamp()),
  UTime = erlang:universaltime(),
  LocalTime = calendar:universal_time_to_local_time(UTime),
  DiffSecs = calendar:datetime_to_gregorian_seconds(LocalTime) -
    calendar:datetime_to_gregorian_seconds(UTime),
  zone(DiffSecs div 3600, (DiffSecs rem 3600) div 60),
  iolist_to_binary(
    io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w~s",
                  [Year, Month, Day, Hour, Min, Sec,
                   zone(DiffSecs div 3600, (DiffSecs rem 3600) div 60)])).

zone(Hr, Min) when Hr >= 0, Min >= 0 ->
  io_lib:format("+~2..0w:~2..0w", [Hr, Min]);
zone(Hr, Min) ->
  io_lib:format("-~2..0w:~2..0w", [abs(Hr), abs(Min)]).
