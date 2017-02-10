%% Copyright 2017, Bernard Notarianni
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

-module(barrel_replicate_checkpoint).
-author("Bernard Notarianni").

%% gen_server API
-export([ new/4
        , set_last_seq/2
        , get_last_seq/1
        , get_start_seq/1
        , maybe_write_checkpoint/1
        , write_checkpoint/1
        , read_checkpoint_doc/2
        ]).


-record(st, { repid       ::binary()
            , session_id  ::binary()  % replication session (history) id
            , source
            , target
            , start_seq=0 ::integer() % start seq for current repl session
            , last_seq=0  ::integer() % last received seq from source
            , target_seq  ::integer() % target seq for current repl session
            , options
            }).

%% =============================================================================
%% Checkpoints management: when, where and what
%% =============================================================================

%% @doc Create a checkpoint object
new(RepId, Source, Target, Options) ->
  StartSeq = checkpoint_start_seq(Source, Target, RepId),
  State = #st{ repid=RepId
             , source=Source
             , target=Target
             , session_id=barrel_lib:uniqid(binary)
             , start_seq=StartSeq
             , options=Options
             },
  set_next_target_seq(State).

set_last_seq(LastSeq, State) ->
  State#st{last_seq=LastSeq}.
get_last_seq(State) ->
  State#st.last_seq.
get_start_seq(State) ->
  State#st.start_seq.

%% @doc Decide it we should write checkpoints, and do it.
maybe_write_checkpoint(#st{last_seq=LastSeq, target_seq=TargetSeq}=State)
  when LastSeq >= TargetSeq ->
  ok = write_checkpoint(State),
  set_next_target_seq(State);
maybe_write_checkpoint(State) ->
  State.

set_next_target_seq(State) ->
  LastSeq = State#st.last_seq,
  CheckpointSize = proplists:get_value(checkpoint_size, State#st.options, 10),
  TargetSeq = LastSeq + CheckpointSize,
  State#st{target_seq=TargetSeq}.


%% @doc Write checkpoint information on both source and target databases.
write_checkpoint(State)  ->
  RepId = State#st.repid,
  StartSeq = State#st.start_seq,
  LastSeq = State#st.last_seq,
  Source = State#st.source,
  Target = State#st.target,
  SessionId = State#st.session_id,
  HistorySize = proplists:get_value(checkpoint_max_history, State#st.options, 20),
  Checkpoint = #{<<"source_last_seq">>   => LastSeq
                ,<<"source_start_seq">>  => StartSeq
                ,<<"session_id">> => State#st.session_id
                ,<<"end_time">> => timestamp()
                ,<<"end_time_microsec">> => erlang:system_time(micro_seconds)
                },
  [add_checkpoint(Db, RepId, SessionId, HistorySize, Checkpoint) ||
    Db <- [Source, Target]],
  ok.

add_checkpoint(Db, RepId, SessionId, HistorySize, Checkpoint) ->
  Doc = case read_checkpoint_doc(Db, RepId) of
          {ok, #{<<"history">> := H}=PreviousDoc} ->
            H2 = case hd(H) of
                   #{<<"session_id">> := SessionId} ->
                     [Checkpoint|tl(H)];
                   _ ->
                     case length(H) >= HistorySize of
                       true ->
                         S = length(H) - HistorySize + 1,
                         T = lists:reverse(lists:nthtail(S, lists:reverse(H))),
                         [Checkpoint|T];
                       false ->
                         [Checkpoint|H]
                     end
                 end,
            PreviousDoc#{<<"history">> => H2};
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
  case catch read_checkpoint_doc(Db, RepId) of
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
  write_system_doc(Db, checkpoint_docid(RepId), Checkpoint).

write_system_doc({Mod, ModState}, Id, Doc) ->
  Mod:write_system_doc(ModState, Id, Doc);
write_system_doc(Db, Id, Doc) when is_binary(Db) ->
  barrel_db:write_system_doc(Db, Id, Doc).

read_checkpoint_doc(Db, RepId) ->
  read_system_doc(Db, checkpoint_docid(RepId)).

read_system_doc({Mod, ModState}, Id) ->
  Mod:read_system_doc(ModState, Id);
read_system_doc(Db, Id) when is_binary(Db) ->
  barrel_db:read_system_doc(Db, Id).

delete_checkpoint_doc(Db, RepId) ->
  delete_system_doc(Db, checkpoint_docid(RepId)).

delete_system_doc({Mod, ModState}, Id) ->
  Mod:delete_system_doc(ModState, Id);
delete_system_doc(Db, Id) when is_binary(Db) ->
  barrel_db:delete_system_doc(Db, Id).

checkpoint_docid(RepId) ->
  <<"replication-checkpoint-", RepId/binary>>.

%% =============================================================================
%% Helpers
%% =============================================================================

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
