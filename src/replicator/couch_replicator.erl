%% Copyright 2016, Benoit Chesneau
%% Copyright 2009-2014 The Apache Software Foundation
%%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_replicator).
-behaviour(gen_server).

% public API
-export([replicate/1]).

% meant to be used only by the replicator database listener
-export([async_replicate/1]).
-export([cancel_replication/1]).

% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("couch_db.hrl").
-include("couch_replicator_api_wrap.hrl").
-include("couch_replicator.hrl").

-import(couch_replicator_utils, [
    start_db_compaction_notifier/2,
    stop_db_compaction_notifier/1
]).

-record(rep_state, {
    rep_details,
    source_name,
    target_name,
    source,
    target,
    history,
    checkpoint_history,
    start_seq,
    committed_seq,
    current_through_seq,
    seqs_in_progress = [],
    highest_seq_done = {0, ?LOWEST_SEQ},
    source_log,
    target_log,
    rep_starttime,
    src_starttime,
    tgt_starttime,
    timer, % checkpoint timer
    changes_queue,
    changes_manager,
    changes_reader,
    workers,
    stats = #rep_stats{},
    session_id,
    db_compaction_notifier = false,
    source_monitor = nil,
    target_monitor = nil,
    source_seq = nil,
    use_checkpoints = true,
    checkpoint_interval = 5000,
    type = db,
    view = nil
}).


replicate(#rep{id = RepId, options = Options, user_ctx = UserCtx} = Rep) ->
    case proplists:get_value(cancel, Options, false) of
    true ->
        case proplists:get_value(id, Options, nil) of
        nil ->
            cancel_replication(RepId);
        RepId2 ->
            cancel_replication(RepId2, UserCtx)
        end;
    false ->
        {ok, Listener} = rep_result_listener(RepId),
        Result = do_replication_loop(Rep),
        couch_replicator_notifier:stop(Listener),
        Result
    end.


do_replication_loop(#rep{id = {BaseId, Ext} = Id, options = Options} = Rep) ->
    case async_replicate(Rep) of
    {ok, _Pid} ->
        case proplists:get_value(continuous, Options, false) of
        true ->
            {ok, {continuous, list_to_binary(BaseId ++ Ext)}};
        false ->
            wait_for_result(Id)
        end;
    Error ->
        Error
    end.


async_replicate(#rep{id = {BaseId, Ext}, source = Src, target = Tgt} = Rep) ->
    RepChildId = BaseId ++ Ext,
    Source = couch_replicator_api_wrap:db_uri(Src),
    Target = couch_replicator_api_wrap:db_uri(Tgt),
    Timeout = proplists:get_value(connection_timeout, Rep#rep.options),
    ChildSpec = {
        RepChildId,
        {gen_server, start_link, [?MODULE, Rep, [{timeout, Timeout}]]},
        temporary,
        250,
        worker,
        [?MODULE]
    },
    % All these nested cases to attempt starting/restarting a replication child
    % are ugly and not 100% race condition free. The following patch submission
    % is a solution:
    %
    % http://erlang.2086793.n4.nabble.com/PATCH-supervisor-atomically-delete-child-spec-when-child-terminates-td3226098.html
    %
    case supervisor:start_child(couch_replicator_job_sup, ChildSpec) of
    {ok, Pid} ->
        lager:info("starting new replication `~s` at ~p (`~s` -> `~s`)",
            [RepChildId, Pid, Source, Target]),
        {ok, Pid};
    {error, already_present} ->
        case supervisor:restart_child(couch_replicator_job_sup, RepChildId) of
        {ok, Pid} ->
            lager:info("restarting replication `~s` at ~p (`~s` -> `~s`)",
                [RepChildId, Pid, Source, Target]),
            {ok, Pid};
        {error, running} ->
            %% this error occurs if multiple replicators are racing
            %% each other to start and somebody else won. Just grab
            %% the Pid by calling start_child again.
            {error, {already_started, Pid}} =
                supervisor:start_child(couch_replicator_job_sup, ChildSpec),
            lager:info("replication `~s` already running at ~p (`~s` -> `~s`)",
                [RepChildId, Pid, Source, Target]),
            {ok, Pid};
        {error, {'EXIT', {badarg,
            [{erlang, apply, [gen_server, start_link, undefined]} | _]}}} ->
            % Clause to deal with a change in the supervisor module introduced
            % in R14B02. For more details consult the thread at:
            %     http://erlang.org/pipermail/erlang-bugs/2011-March/002273.html
            _ = supervisor:delete_child(couch_replicator_job_sup, RepChildId),
            async_replicate(Rep);
        {error, _} = Error ->
            Error
        end;
    {error, {already_started, Pid}} ->
        lager:info("replication `~s` already running at ~p (`~s` -> `~s`)",
            [RepChildId, Pid, Source, Target]),
        {ok, Pid};
    {error, {Error, _}} ->
        {error, Error}
    end.


rep_result_listener(RepId) ->
    ReplyTo = self(),
    {ok, _Listener} = couch_replicator_notifier:start_link(
        fun({_, RepId2, _} = Ev) when RepId2 =:= RepId ->
                ReplyTo ! Ev;
            (_) ->
                ok
        end).


wait_for_result(RepId) ->
    receive
    {finished, RepId, RepResult} ->
        {ok, RepResult};
    {error, RepId, Reason} ->
        {error, Reason}
    end.


cancel_replication({BaseId, Extension}) ->
    FullRepId = BaseId ++ Extension,
    lager:info("Canceling replication `~s`...", [FullRepId]),
    case supervisor:terminate_child(couch_replicator_job_sup, FullRepId) of
    ok ->
        lager:info("Replication `~s` canceled.", [FullRepId]),
        case supervisor:delete_child(couch_replicator_job_sup, FullRepId) of
            ok ->
                {ok, {cancelled, list_to_binary(FullRepId)}};
            {error, not_found} ->
                {ok, {cancelled, list_to_binary(FullRepId)}};
            Error ->
                Error
        end;
    Error ->
        lager:error("Error canceling replication `~s`: ~p", [FullRepId, Error]),
        Error
    end.

cancel_replication(RepId, UserCtx) ->
    [Name, Roles] = barrel_lib:userctx_get([name, roles], UserCtx),
    case lists:member(<<"_admin">>, Roles) of
    true ->
        cancel_replication(RepId);
    false ->
        {BaseId, Ext} = RepId,
        case lists:keysearch(
            BaseId ++ Ext, 1, supervisor:which_children(couch_replicator_job_sup)) of
        {value, {_, Pid, _, _}} when is_pid(Pid) ->
            case (catch gen_server:call(Pid, get_details, infinity)) of
            {ok, #rep{user_ctx = UserCtx}} ->
                case barrel_lib:userctx_get(name, UserCtx) of
                    Name -> cancel_replication(RepId);
                    _ ->
                        throw({unauthorized,
                                <<"Can't cancel a replication triggered by another user">>})
                end;
            {'EXIT', {noproc, {gen_server, call, _}}} ->
                {error, not_found};
            Error ->
                throw(Error)
            end;
        _ ->
            {error, not_found}
        end
    end.

init(InitArgs) ->
    try
        do_init(InitArgs)
    catch
    throw:{unauthorized, DbUri} ->
        {stop, {unauthorized,
            <<"unauthorized to access or create database ", DbUri/binary>>}};
    throw:{db_not_found, DbUri} ->
        {stop, {db_not_found, <<"could not open ", DbUri/binary>>}};
    throw:Error ->
        {stop, Error}
    end.

do_init(#rep{options = Options, id = {BaseId, Ext}} = Rep) ->
    process_flag(trap_exit, true),

    #rep_state{
        source = Source,
        target = Target,
        source_name = SourceName,
        target_name = TargetName,
        start_seq = {_Ts, StartSeq},
        source_seq = SourceCurSeq,
        committed_seq = {_, CommittedSeq},
        checkpoint_interval = CheckpointInterval,
        type = Type
    } = State = init_state(Rep),

    NumWorkers = proplists:get_value(worker_processes, Options),
    BatchSize = proplists:get_value(worker_batch_size, Options),
    {ok, ChangesQueue} = couch_work_queue:new([
        {max_items, BatchSize * NumWorkers * 2},
        {max_size, 100 * 1024 * NumWorkers}
    ]),
    % This starts the _changes reader process. It adds the changes from
    % the source db to the ChangesQueue.
    ChangesReader = spawn_changes_reader(StartSeq, Source, ChangesQueue, Options, Type),
    % Changes manager - responsible for dequeing batches from the changes queue
    % and deliver them to the worker processes.
    ChangesManager = spawn_changes_manager(self(), ChangesQueue, BatchSize),
    % This starts the worker processes. They ask the changes queue manager for a
    % a batch of _changes rows to process -> check which revs are missing in the
    % target, and for the missing ones, it copies them from the source to the target.
    MaxConns = proplists:get_value(http_connections, Options),
    Workers = lists:map(
        fun(_) ->
            {ok, Pid} = couch_replicator_worker:start_link(
                self(), Source, Target, ChangesManager, MaxConns),
            Pid
        end,
        lists:seq(1, NumWorkers)),

    barrel_task_status:add_task([
        {type, replication},
        {replication_id, list_to_binary(BaseId ++ Ext)},
        {doc_id, Rep#rep.doc_id},
        {source, list_to_binary(SourceName)},
        {target, list_to_binary(TargetName)},
        {continuous, proplists:get_value(continuous, Options, false)},
        {revisions_checked, 0},
        {missing_revisions_found, 0},
        {docs_read, 0},
        {docs_written, 0},
        {doc_write_failures, 0},
        {source_seq, SourceCurSeq},
        {checkpointed_source_seq, CommittedSeq},
        {progress, 0},
        {checkpoint_interval, CheckpointInterval}
    ]),
    barrel_task_status:set_update_frequency(1000),

    % Until OTP R14B03:
    %
    % Restarting a temporary supervised child implies that the original arguments
    % (#rep{} record) specified in the MFA component of the supervisor
    % child spec will always be used whenever the child is restarted.
    % This implies the same replication performance tunning parameters will
    % always be used. The solution is to delete the child spec (see
    % cancel_replication/1) and then start the replication again, but this is
    % unfortunately not immune to race conditions.

    lager:info("Replication `~p` is using:~n"
        "~c~p worker processes~n"
        "~ca worker batch size of ~p~n"
        "~c~p HTTP connections~n"
        "~ca connection timeout of ~p milliseconds~n"
        "~c~p retries per request~n"
        "~csocket options are: ~s~s",
        [BaseId ++ Ext, $\t, NumWorkers, $\t, BatchSize, $\t,
            MaxConns, $\t, proplists:get_value(connection_timeout, Options),
            $\t, proplists:get_value(retries, Options),
            $\t, io_lib:format("~p", [proplists:get_value(socket_options, Options)]),
            case StartSeq of
            ?LOWEST_SEQ ->
                "";
            _ ->
                io_lib:format("~n~csource start sequence ~p", [$\t, StartSeq])
            end]),

    lager:debug("Worker pids are: ~p", [Workers]),

    couch_replicator_manager:replication_started(Rep),

    {ok, State#rep_state{
            changes_queue = ChangesQueue,
            changes_manager = ChangesManager,
            changes_reader = ChangesReader,
            workers = Workers
        }
    }.


handle_info(shutdown, St) ->
    {stop, shutdown, St};

handle_info({'$barrel_event', DbName, compacted},
            #rep_state{source = #db{name = DbName} = Source} = State) ->
    {ok, NewSource} = couch_db:reopen(Source),
    {noreply, State#rep_state{source = NewSource}};

handle_info({'$barrel_event', DbName, compacted},
            #rep_state{target = #db{name = DbName} = Target} = State) ->
    {ok, NewTarget} = couch_db:reopen(Target),
    {noreply, State#rep_state{target = NewTarget}};

handle_info({'DOWN', Ref, _, _, Why}, #rep_state{source_monitor = Ref} = St) ->
    lager:error("Source database is down. Reason: ~p", [Why]),
    {stop, source_db_down, St};

handle_info({'DOWN', Ref, _, _, Why}, #rep_state{target_monitor = Ref} = St) ->
    lager:error("Target database is down. Reason: ~p", [Why]),
    {stop, target_db_down, St};

handle_info({'EXIT', Pid, normal}, #rep_state{changes_reader=Pid} = State) ->
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, #rep_state{changes_reader=Pid} = State) ->
    lager:error("ChangesReader process died with reason: ~p", [Reason]),
    {stop, changes_reader_died, cancel_timer(State)};

handle_info({'EXIT', Pid, normal}, #rep_state{changes_manager = Pid} = State) ->
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, #rep_state{changes_manager = Pid} = State) ->
    lager:error("ChangesManager process died with reason: ~p", [Reason]),
    {stop, changes_manager_died, cancel_timer(State)};

handle_info({'EXIT', Pid, normal}, #rep_state{changes_queue=Pid} = State) ->
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, #rep_state{changes_queue=Pid} = State) ->
    lager:error("ChangesQueue process died with reason: ~p", [Reason]),
    {stop, changes_queue_died, cancel_timer(State)};

handle_info({'EXIT', Pid, normal}, #rep_state{workers = Workers} = State) ->
    case Workers -- [Pid] of
    Workers ->
        lager:error("unknown pid bit the dust ~p ~n",[Pid]),
        {noreply, State#rep_state{workers = Workers}};
        %% not clear why a stop was here before
        %{stop, {unknown_process_died, Pid, normal}, State};
    [] ->
        catch unlink(State#rep_state.changes_manager),
        catch exit(State#rep_state.changes_manager, kill),
        do_last_checkpoint(State);
    Workers2 ->
        {noreply, State#rep_state{workers = Workers2}}
    end;

handle_info({'EXIT', Pid, Reason}, #rep_state{workers = Workers} = State) ->
    State2 = cancel_timer(State),
    case lists:member(Pid, Workers) of
    false ->
        {stop, {unknown_process_died, Pid, Reason}, State2};
    true ->
        lager:error("Worker ~p died with reason: ~p", [Pid, Reason]),
        {stop, {worker_died, Pid, Reason}, State2}
    end.

handle_call(get_details, _From, #rep_state{rep_details = Rep} = State) ->
    {reply, {ok, Rep}, State};

handle_call({add_stats, Stats}, From, State) ->
    gen_server:reply(From, ok),
    NewStats = couch_replicator_utils:sum_stats(State#rep_state.stats, Stats),
    {noreply, State#rep_state{stats = NewStats}};

handle_call({report_seq_done, Seq, StatsInc}, From,
    #rep_state{seqs_in_progress = SeqsInProgress, highest_seq_done = HighestDone,
        current_through_seq = ThroughSeq, stats = Stats} = State) ->
    gen_server:reply(From, ok),
    {NewThroughSeq0, NewSeqsInProgress} = case SeqsInProgress of
    [] ->
        {Seq, []};
    [Seq | Rest] ->
        {Seq, Rest};
    [_ | _] ->
        {ThroughSeq, ordsets:del_element(Seq, SeqsInProgress)}
    end,
    NewHighestDone = lists:max([HighestDone, Seq]),
    NewThroughSeq = case NewSeqsInProgress of
    [] ->
        lists:max([NewThroughSeq0, NewHighestDone]);
    _ ->
        NewThroughSeq0
    end,
    lager:debug("Worker reported seq ~p, through seq was ~p, "
        "new through seq is ~p, highest seq done was ~p, "
        "new highest seq done is ~p~n"
        "Seqs in progress were: ~p~nSeqs in progress are now: ~p",
        [Seq, ThroughSeq, NewThroughSeq, HighestDone,
            NewHighestDone, SeqsInProgress, NewSeqsInProgress]),
    SourceCurSeq = source_cur_seq(State),
    NewState = State#rep_state{
        stats = couch_replicator_utils:sum_stats(Stats, StatsInc),
        current_through_seq = NewThroughSeq,
        seqs_in_progress = NewSeqsInProgress,
        highest_seq_done = NewHighestDone,
        source_seq = SourceCurSeq
    },
    update_task(NewState),
    {noreply, NewState}.


handle_cast({db_compacted, DbName},
    #rep_state{source = #db{name = DbName} = Source} = State) ->
    {ok, NewSource} = couch_db:reopen(Source),
    {noreply, State#rep_state{source = NewSource}};

handle_cast({db_compacted, DbName},
    #rep_state{target = #db{name = DbName} = Target} = State) ->
    {ok, NewTarget} = couch_db:reopen(Target),
    {noreply, State#rep_state{target = NewTarget}};

handle_cast(checkpoint, State) ->
    case do_checkpoint(State) of
    {ok, NewState} ->
        {noreply, NewState#rep_state{timer = start_timer(State)}};
    Error ->
        {stop, Error, State}
    end;

handle_cast({report_seq, Seq},
    #rep_state{seqs_in_progress = SeqsInProgress} = State) ->
    NewSeqsInProgress = ordsets:add_element(Seq, SeqsInProgress),
    {noreply, State#rep_state{seqs_in_progress = NewSeqsInProgress}}.


code_change(OldVsn, OldState, Extra) when tuple_size(OldState) =:= 30 ->
    code_change(OldVsn, erlang:append_element(OldState, true), Extra);
code_change(OldVsn, OldState, Extra) when tuple_size(OldState) =:= 31 ->
    code_change(OldVsn, erlang:append_element(OldState, 5000), Extra);
code_change(_OldVsn, #rep_state{}=State, _Extra) ->
    {ok, State}.


terminate(normal, #rep_state{rep_details = #rep{id = RepId} = Rep,
    checkpoint_history = CheckpointHistory} = State) ->
    terminate_cleanup(State),
    couch_replicator_notifier:notify({finished, RepId, CheckpointHistory}),
    couch_replicator_manager:replication_completed(Rep, rep_stats(State));

terminate(shutdown, #rep_state{rep_details = #rep{id = RepId}} = State) ->
    % cancelled replication throught ?MODULE:cancel_replication/1
    couch_replicator_notifier:notify({error, RepId, <<"cancelled">>}),
    terminate_cleanup(State);

terminate(shutdown, {error, Class, Error, Stack, InitArgs}) ->
    #rep{id=RepId} = InitArgs,
    lager:error("~p:~p: Replication failed to start for args ~p: ~p",
               [Class, Error, InitArgs, Stack]),
    case Error of
    {unauthorized, DbUri} ->
        NotifyError = {unauthorized, <<"unauthorized to access or create database ", DbUri/binary>>};
    {db_not_found, DbUri} ->
        NotifyError = {db_not_found, <<"could not open ", DbUri/binary>>};
    _ ->
        NotifyError = Error
    end,
    couch_replicator_notifier:notify({error, RepId, NotifyError}),
    couch_replicator_manager:replication_error(InitArgs, NotifyError);
terminate(Reason, State) ->
    #rep_state{
        source_name = Source,
        target_name = Target,
        rep_details = #rep{id = {BaseId, Ext} = RepId} = Rep
    } = State,
    lager:error("Replication `~s` (`~s` -> `~s`) failed: ~s",
        [BaseId ++ Ext, Source, Target, barrel_lib:to_binary(Reason)]),
    terminate_cleanup(State),
    couch_replicator_notifier:notify({error, RepId, Reason}),
    couch_replicator_manager:replication_error(Rep, Reason).


terminate_cleanup(State) ->
    update_task(State),
    stop_db_compaction_notifier(State#rep_state.db_compaction_notifier),
    couch_replicator_api_wrap:db_close(State#rep_state.source),
    couch_replicator_api_wrap:db_close(State#rep_state.target),
    ok.


do_last_checkpoint(#rep_state{seqs_in_progress = [],
    highest_seq_done = {_Ts, ?LOWEST_SEQ}} = State) ->
    {stop, normal, cancel_timer(State)};
do_last_checkpoint(#rep_state{seqs_in_progress = [],
    highest_seq_done = Seq} = State) ->
    case do_checkpoint(State#rep_state{current_through_seq = Seq}) of
    {ok, NewState} ->
        {stop, normal, cancel_timer(NewState)};
    Error ->
        {stop, Error, State}
    end.


start_timer(State) ->
    After = State#rep_state.checkpoint_interval,
    case timer:apply_after(After, gen_server, cast, [self(), checkpoint]) of
    {ok, Ref} ->
        Ref;
    Error ->
        lager:error("Replicator, error scheduling checkpoint:  ~p", [Error]),
        nil
    end.


cancel_timer(#rep_state{timer = nil} = State) ->
    State;
cancel_timer(#rep_state{timer = Timer} = State) ->
    {ok, cancel} = timer:cancel(Timer),
    State#rep_state{timer = nil}.


init_state(Rep) ->
    #rep{
        source = Src, target = Tgt,
        options = Options, user_ctx = UserCtx,
        type = Type, view = View
    } = Rep,
    {ok, Source} = couch_replicator_api_wrap:db_open(Src, [{user_ctx, UserCtx}]),
    {ok, Target} = couch_replicator_api_wrap:db_open(Tgt, [{user_ctx, UserCtx}],
                                                     proplists:get_value(create_target, Options, false)),

    {ok, SourceInfo} = couch_replicator_api_wrap:get_db_info(Source),
    {ok, TargetInfo} = couch_replicator_api_wrap:get_db_info(Target),

    [SourceLog, TargetLog] = find_replication_logs([Source, Target], Rep),

    {StartSeq0, History} = compare_replication_logs(SourceLog, TargetLog),
    StartSeq1 = proplists:get_value(since_seq, Options, StartSeq0),
    StartSeq = {0, StartSeq1},

    SourceSeq = case Type of
        db -> maps:get(<<"update_seq">>, SourceInfo, ?LOWEST_SEQ);
        view ->
            {DDoc, VName} = View,
            {ok, VInfo} = couch_replicator_api_wrap:get_view_info(Source, DDoc,
                                                                  VName),
            proplists:get_value(<<"last_seq">>, VInfo, ?LOWEST_SEQ)
    end,


    #doc{body=CheckpointHistory} = SourceLog,
    State = #rep_state{
        rep_details = Rep,
        source_name = couch_replicator_api_wrap:db_uri(Source),
        target_name = couch_replicator_api_wrap:db_uri(Target),
        source = Source,
        target = Target,
        history = History,
        checkpoint_history = CheckpointHistory#{<<"no_changes">> => true},
        start_seq = StartSeq,
        current_through_seq = StartSeq,
        committed_seq = StartSeq,
        source_log = SourceLog,
        target_log = TargetLog,
        rep_starttime = barrel_lib:rfc1123_date(),
        src_starttime = maps:get(<<"instance_start_time">>, SourceInfo),
        tgt_starttime = maps:get(<<"instance_start_time">>, TargetInfo),
        session_id = barrel_uuids:random(),
        db_compaction_notifier = start_db_compaction_notifier(Source, Target),
        source_monitor = db_monitor(Source),
        target_monitor = db_monitor(Target),
        source_seq = SourceSeq,
        use_checkpoints = proplists:get_value(use_checkpoints, Options, true),
        checkpoint_interval = proplists:get_value(checkpoint_interval, Options,
                                        5000),
        type = Type,
        view = View
    },
    State#rep_state{timer = start_timer(State)}.


find_replication_logs(DbList, #rep{id = {BaseId, _}} = Rep) ->
    LogId = list_to_binary(?LOCAL_DOC_PREFIX ++ BaseId),
    fold_replication_logs(DbList, ?REP_ID_VERSION, LogId, LogId, Rep, []).


fold_replication_logs([], _Vsn, _LogId, _NewId, _Rep, Acc) ->
    lists:reverse(Acc);

fold_replication_logs([Db | Rest] = Dbs, Vsn, LogId, NewId, Rep, Acc) ->
    case couch_replicator_api_wrap:open_doc(Db, LogId, [ejson_body]) of
    {error, <<"not_found">>} when Vsn > 1 ->
        OldRepId = couch_replicator_utils:replication_id(Rep, Vsn - 1),
        fold_replication_logs(Dbs, Vsn - 1,
            list_to_binary(?LOCAL_DOC_PREFIX ++ OldRepId), NewId, Rep, Acc);
    {error, <<"not_found">>} ->
        fold_replication_logs(
            Rest, ?REP_ID_VERSION, NewId, NewId, Rep, [#doc{id = NewId} | Acc]);
    {ok, Doc} when LogId =:= NewId ->
        fold_replication_logs(
            Rest, ?REP_ID_VERSION, NewId, NewId, Rep, [Doc | Acc]);
    {ok, Doc} ->
        MigratedLog = #doc{id = NewId, body = Doc#doc.body},
        fold_replication_logs(
            Rest, ?REP_ID_VERSION, NewId, NewId, Rep, [MigratedLog | Acc])
    end.


spawn_changes_reader(StartSeq, #httpdb{} = Db, ChangesQueue, Options, Type) ->
    spawn_link(fun() ->
        put(last_seq, StartSeq),
        put(retries_left, Db#httpdb.retries),
        read_changes(StartSeq, Db#httpdb{retries = 0}, ChangesQueue, Options, Type)
    end);
spawn_changes_reader(StartSeq, Db, ChangesQueue, Options, Type) ->
    spawn_link(fun() ->
        read_changes(StartSeq, Db, ChangesQueue, Options, Type)
    end).

last_seq(view, _Seq, ChangeSeq) -> ChangeSeq;
last_seq(_, Seq, _ChangeSeq) -> Seq.

read_changes(StartSeq, Db, ChangesQueue, Options, Type) ->
    try
        couch_replicator_api_wrap:changes_since(Db, all_docs, StartSeq, fun
            ({#doc_info{high_seq = Seq, revs = []}, ChangeSeq}) ->
                put(last_seq, last_seq(Type, Seq, ChangeSeq));
            ({#doc_info{high_seq = Seq, id = Id} = DocInfo, ChangeSeq}) ->
                LastSeq = last_seq(Type, Seq, ChangeSeq),
                case Id of
                <<>> ->
                    % Previous CouchDB releases had a bug which allowed a doc
                    % with an empty ID to be inserted into databases. Such doc
                    % is impossible to GET.
                    lager:error("Replicator: ignoring document with empty ID in "
                        "source database `~s` (_changes sequence ~p)",
                        [couch_replicator_api_wrap:db_uri(Db), LastSeq]);
                _ ->
                    ok = couch_work_queue:queue(ChangesQueue, {DocInfo, LastSeq})
                end,
                put(last_seq, LastSeq)
            end, Options),
        couch_work_queue:close(ChangesQueue)
    catch exit:{http_request_failed, _, _, _} = Error ->
        case get(retries_left) of
        N when N > 0 ->
            put(retries_left, N - 1),
            LastSeq = get(last_seq),
            Db2 = case LastSeq of
            StartSeq ->
                lager:info("Retrying _changes request to source database ~s"
                    " with since=~p in ~p seconds",
                    [couch_replicator_api_wrap:db_uri(Db), LastSeq, Db#httpdb.wait / 1000]),
                ok = timer:sleep(Db#httpdb.wait),
                Db#httpdb{wait = 2 * Db#httpdb.wait};
            _ ->
                lager:info("Retrying _changes request to source database ~s"
                    " with since=~p", [couch_replicator_api_wrap:db_uri(Db), LastSeq]),
                Db
            end,
            read_changes(LastSeq, Db2, ChangesQueue, Options, Type);
        _ ->
            exit(Error)
        end
    end.


spawn_changes_manager(Parent, ChangesQueue, BatchSize) ->
    spawn_link(fun() ->
        changes_manager_loop_open(Parent, ChangesQueue, BatchSize, 1)
    end).

changes_manager_loop_open(Parent, ChangesQueue, BatchSize, Ts) ->
    receive
    {get_changes, From} ->
        case couch_work_queue:dequeue(ChangesQueue, BatchSize) of
        closed ->
            From ! {closed, self()};
        {ok, Changes0} ->
            {_DocInfo, Seq} = lists:last(Changes0),
            Changes = [DocInfo || {DocInfo, _Seq} <- Changes0],
            ReportSeq = {Ts, Seq},
            ok = gen_server:cast(Parent, {report_seq, ReportSeq}),
            From ! {changes, self(), Changes, ReportSeq}
        end,
        changes_manager_loop_open(Parent, ChangesQueue, BatchSize, Ts + 1)
    end.


do_checkpoint(#rep_state{use_checkpoints=false} = State) ->
    NewState = State#rep_state{checkpoint_history = #{<<"use_checkpoints">> => false} },
    {ok, NewState};
do_checkpoint(#rep_state{current_through_seq=Seq, committed_seq=Seq} = State) ->
    SourceCurSeq = source_cur_seq(State),
    NewState = State#rep_state{source_seq = SourceCurSeq},
    update_task(NewState),
    {ok, NewState};
do_checkpoint(State) ->
    #rep_state{
        source_name=SourceName,
        target_name=TargetName,
        source = Source,
        target = Target,
        history = OldHistory,
        start_seq = {_, StartSeq},
        current_through_seq = {_Ts, NewSeq} = NewTsSeq,
        source_log = SourceLog,
        target_log = TargetLog,
        rep_starttime = ReplicationStartTime,
        src_starttime = SrcInstanceStartTime,
        tgt_starttime = TgtInstanceStartTime,
        stats = Stats,
        rep_details = #rep{options = Options},
        session_id = SessionId
    } = State,
    case commit_to_both(Source, Target) of
    {source_error, Reason} ->
         {checkpoint_commit_failure,
             <<"Failure on source commit: ", (barrel_lib:to_binary(Reason))/binary>>};
    {target_error, Reason} ->
         {checkpoint_commit_failure,
             <<"Failure on target commit: ", (barrel_lib:to_binary(Reason))/binary>>};
    {SrcInstanceStartTime, TgtInstanceStartTime} ->
        lager:info("recording a checkpoint for `~s` -> `~s` at source update_seq ~p",
            [SourceName, TargetName, NewSeq]),
        StartTime = list_to_binary(ReplicationStartTime),
        EndTime = list_to_binary(barrel_lib:rfc1123_date()),
        NewHistoryEntry =  #{
          <<"session_id">> => SessionId,
          <<"start_time">> => StartTime,
          <<"end_time">> => EndTime,
          <<"start_last_seq">> => StartSeq,
          <<"end_last_seq">> => NewSeq,
          <<"recorded_seq">> => NewSeq,
          <<"missing_checked">> => Stats#rep_stats.missing_checked,
          <<"missing_found">> => Stats#rep_stats.missing_found,
          <<"docs_read">> => Stats#rep_stats.docs_read,
          <<"docs_written">> => Stats#rep_stats.docs_written,
          <<"doc_write_failures">> => Stats#rep_stats.doc_write_failures},

        BaseHistory0 = #{<<"session_id">> => SessionId,
                         <<"source_last_seq">> => NewSeq,
                         <<"replication_id_version">> => ?REP_ID_VERSION},

        BaseHistory = case proplists:get_value(doc_ids, Options) of
            undefined -> BaseHistory0;
            _DocIds ->
                % backwards compatibility with the result of a replication by
                % doc IDs in versions 0.11.x and 1.0.x
                % TODO: deprecate (use same history format, simplify code)
                BaseHistory0#{<<"start_time">> => StartTime,
                              <<"end_time">> => EndTime,
                              <<"docs_read">> => Stats#rep_stats.docs_read,
                              <<"docs_written">> => Stats#rep_stats.docs_written,
                              <<"doc_write_failures">> => Stats#rep_stats.doc_write_failures}
        end,

        % limit history to 50 entries
        NewRepHistory = BaseHistory#{<<"history">> => lists:sublist([NewHistoryEntry | OldHistory], 50)},

        try
            {SrcRevPos, SrcRevId} = update_checkpoint(
                Source, SourceLog#doc{body = NewRepHistory}, source),
            {TgtRevPos, TgtRevId} = update_checkpoint(
                Target, TargetLog#doc{body = NewRepHistory}, target),
            SourceCurSeq = source_cur_seq(State),
            NewState = State#rep_state{
                source_seq = SourceCurSeq,
                checkpoint_history = NewRepHistory,
                committed_seq = NewTsSeq,
                source_log = SourceLog#doc{revs={SrcRevPos, [SrcRevId]}},
                target_log = TargetLog#doc{revs={TgtRevPos, [TgtRevId]}}
            },
            update_task(NewState),
            {ok, NewState}
        catch throw:{checkpoint_commit_failure, _} = Failure ->
            Failure
        end;
    {SrcInstanceStartTime, _NewTgtInstanceStartTime} ->
        {checkpoint_commit_failure, <<"Target database out of sync. "
            "Try to increase max_dbs_open at the target's server.">>};
    {_NewSrcInstanceStartTime, TgtInstanceStartTime} ->
        {checkpoint_commit_failure, <<"Source database out of sync. "
            "Try to increase max_dbs_open at the source's server.">>};
    {_NewSrcInstanceStartTime, _NewTgtInstanceStartTime} ->
        {checkpoint_commit_failure, <<"Source and target databases out of "
            "sync. Try to increase max_dbs_open at both servers.">>}
    end.


update_checkpoint(Db, Doc, DbType) ->
    try
        update_checkpoint(Db, Doc)
    catch throw:{checkpoint_commit_failure, Reason} ->
        throw({checkpoint_commit_failure,
            <<"Error updating the ", (barrel_lib:to_binary(DbType))/binary,
                " checkpoint document: ", (barrel_lib:to_binary(Reason))/binary>>})
    end.

update_checkpoint(Db, #doc{id = LogId, body = LogBody} = Doc) ->
    try
        case couch_replicator_api_wrap:update_doc(Db, Doc, [delay_commit]) of
        {ok, PosRevId} ->
            PosRevId;
        {error, Reason} ->
            throw({checkpoint_commit_failure, Reason})
        end
    catch throw:conflict ->
        case (catch couch_replicator_api_wrap:open_doc(Db, LogId, [ejson_body])) of
        {ok, #doc{body = LogBody, revs = {Pos, [RevId | _]}}} ->
            % This means that we were able to update successfully the
            % checkpoint doc in a previous attempt but we got a connection
            % error (timeout for e.g.) before receiving the success response.
            % Therefore the request was retried and we got a conflict, as the
            % revision we sent is not the current one.
            % We confirm this by verifying the doc body we just got is the same
            % that we have just sent.
            {Pos, RevId};
        _ ->
            throw({checkpoint_commit_failure, conflict})
        end
    end.


commit_to_both(Source, Target) ->
    % commit the src async
    ParentPid = self(),
    SrcCommitPid = spawn_link(
        fun() ->
            Result = (catch couch_replicator_api_wrap:ensure_full_commit(Source)),
            ParentPid ! {self(), Result}
        end),

    % commit tgt sync
    TargetResult = (catch couch_replicator_api_wrap:ensure_full_commit(Target)),

    SourceResult = receive
    {SrcCommitPid, Result} ->
        unlink(SrcCommitPid),
        receive {'EXIT', SrcCommitPid, _} -> ok after 0 -> ok end,
        Result;
    {'EXIT', SrcCommitPid, Reason} ->
        {error, Reason}
    end,
    case TargetResult of
    {ok, TargetStartTime} ->
        case SourceResult of
        {ok, SourceStartTime} ->
            {SourceStartTime, TargetStartTime};
        SourceError ->
            {source_error, SourceError}
        end;
    TargetError ->
        {target_error, TargetError}
    end.


compare_replication_logs(SrcDoc, TgtDoc) ->
    #doc{body=RepRecProps} = SrcDoc,
    #doc{body=RepRecPropsTgt} = TgtDoc,
    case maps:get(<<"session_id">>, RepRecProps, undefined) ==
            maps:get(<<"session_id">>, RepRecPropsTgt, undefined) of
    true ->
        % if the records have the same session id,
        % then we have a valid replication history
        OldSeqNum = maps:get(<<"source_last_seq">>, RepRecProps, ?LOWEST_SEQ),
        OldHistory = maps:get(<<"history">>, RepRecProps, []),
        {OldSeqNum, OldHistory};
    false ->
        SourceHistory = maps:get(<<"history">>, RepRecProps, []),
        TargetHistory = maps:get(<<"history">>, RepRecPropsTgt, []),
        lager:info("Replication records differ. "
                "Scanning histories to find a common ancestor.", []),
        lager:debug("Record on source:~p~nRecord on target:~p~n",
                [RepRecProps, RepRecPropsTgt]),
        compare_rep_history(SourceHistory, TargetHistory)
    end.

compare_rep_history(S, T) when S =:= [] orelse T =:= [] ->
    lager:info("no common ancestry -- performing full replication", []),
    {?LOWEST_SEQ, []};
compare_rep_history([S | SourceRest], [T | TargetRest] = Target) ->
    SourceId = maps:get(<<"session_id">>, S, undefined),
    case has_session_id(SourceId, Target) of
    true ->
        RecordSeqNum = maps:get(<<"recorded_seq">>, S, ?LOWEST_SEQ),
        lager:info("found a common replication record with source_seq ~p",
            [RecordSeqNum]),
        {RecordSeqNum, SourceRest};
    false ->
        TargetId = maps:get(<<"session_id">>, T, undefined),
        case has_session_id(TargetId, SourceRest) of
        true ->
            RecordSeqNum = maps:get(<<"recorded_seq">>, T, ?LOWEST_SEQ),
            lager:info("found a common replication record with source_seq ~p",
                [RecordSeqNum]),
            {RecordSeqNum, TargetRest};
        false ->
            compare_rep_history(SourceRest, TargetRest)
        end
    end.


has_session_id(_SessionId, []) ->
    false;
has_session_id(SessionId, [Props | Rest]) ->
    case maps:get(<<"session_id">>, Props, nil) of
        SessionId ->
            true;
        _Else ->
            has_session_id(SessionId, Rest)
    end.


db_monitor(#db{} = Db) ->
    couch_db:monitor(Db);
db_monitor(_HttpDb) ->
    nil.

source_cur_seq(#rep_state{source = #httpdb{} = Db, source_seq = Seq,
                          type = view, view = {DDoc, VName}}) ->
    case (catch couch_replicator_api_wrap:get_view_info(
                Db#httpdb{retries = 3}, DDoc, VName)) of
    {ok, Info} ->
        proplists:get_value(<<"last_seq">>, Info, Seq);
    _ ->
        Seq
    end;

source_cur_seq(#rep_state{source = Db, source_seq = Seq,
                          type = view, view = {DDoc, VName}}) ->
    {ok, Info} = couch_replicator_api_wrap:get_view_info(Db, DDoc, VName),
    proplists:get_value(<<"last_seq">>, Info, Seq);

source_cur_seq(#rep_state{source = #httpdb{} = Db, source_seq = Seq}) ->
    case (catch couch_replicator_api_wrap:get_db_info(Db#httpdb{retries = 3})) of
    {ok, Info} ->
        maps:get(<<"update_seq">>, Info, Seq);
    _ ->
        Seq
    end;
source_cur_seq(#rep_state{source = Db, source_seq = Seq}) ->
    {ok, Info} = couch_replicator_api_wrap:get_db_info(Db),
    maps:get(<<"update_seq">>, Info, Seq).


update_task(State) ->
    #rep_state{
        current_through_seq = {_, CurSeq},
        source_seq = SourceCurSeq
    } = State,
    barrel_task_status:update(
        rep_stats(State) ++ [
        {source_seq, SourceCurSeq},
        case is_number(CurSeq) andalso is_number(SourceCurSeq) of
        true ->
            case SourceCurSeq of
            0 ->
                {progress, 0};
            _ ->
                {progress, (CurSeq * 100) div SourceCurSeq}
            end;
        false ->
            {progress, null}
        end
    ]).


rep_stats(State) ->
    #rep_state{
        committed_seq = {_, CommittedSeq},
        stats = Stats
    } = State,
    [
        {revisions_checked, Stats#rep_stats.missing_checked},
        {missing_revisions_found, Stats#rep_stats.missing_found},
        {docs_read, Stats#rep_stats.docs_read},
        {docs_written, Stats#rep_stats.docs_written},
        {doc_write_failures, Stats#rep_stats.doc_write_failures},
        {checkpointed_source_seq, CommittedSeq}
    ].
