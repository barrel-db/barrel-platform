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

-module(couch_index_indexer).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {index,
                dbname,
                threshold,
                refresh_interval,
                refresh_pid=nil,
                db_updates=0,
                tref=nil,
                locks}).


start_link(Index, DbName) ->
    gen_server:start_link(?MODULE, {Index, DbName}, []).

init({Index, DbName}) ->
    process_flag(trap_exit, true),
    %% register to config events
    Self = self(),
    ok  = couch_config:register(fun
                ("couch_index", "threshold") ->
                    gen_server:cast(Self, config_threshold);
                ("couch_index", "refresh_interval") ->
                    gen_server:cast(Self, config_refresh)
            end),

    %% get defaults
    Threshold = get_db_threshold(),
    Refresh = get_refresh_interval(),

    %% delay background index indexing
    self() ! start_indexing,
    {ok, #state{index=Index,
                dbname=DbName,
                threshold=Threshold,
                refresh_interval=Refresh,
                locks=dict:new()}}.

handle_call({acquire, Pid}, _From, #state{locks=Locks}=State) ->
    NLocks = case dict:find(Pid, Locks) of
        error ->
            dict:store(Pid, {erlang:monitor(process, Pid), 1}, Locks);
        {ok, {MRef, Refc}} ->
             dict:store(Pid, {MRef, Refc+1}, Locks)
    end,
    {reply, ok, State#state{locks=NLocks}};

handle_call({release, Pid}, _From, #state{locks=Locks}=State) ->
     NLocks = case dict:find(Pid, Locks) of
        {ok, {MRef, 1}} ->
            erlang:demonitor(MRef, [flush]),
            dict:erase(Pid, Locks);
        {ok, {MRef, Refc}} ->
            dict:store(Pid, {MRef, Refc-1}, Locks);
        error ->
            Locks
    end,

    NState = State#state{locks=NLocks},

    case should_close() of
        true -> {stop, normal, ok, NState};
        false -> {reply, ok, NState}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.


handle_cast(config_threshold, State) ->
    Threshold = get_db_threshold(),
    {noreply, State#state{threshold=Threshold}};
handle_cast(config_refresh, #state{tref=TRef}=State) ->
    R = get_refresh_interval(),
    %% stop the old timee
    if TRef /= nil ->
            timer:cancel(TRef);
        true -> ok
    end,
    %% start the new timer
    {ok, NTRef}  = timer:send_interval(R, self(), refresh_index),
    {noreply, State#state{refresh_interval=R, tref=NTRef}};

handle_cast(updated, State) ->
    NState = do_update(State),
    {noreply, NState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start_indexing, #state{index=Index,
                                   dbname=DbName,
                                   refresh_interval=R}=State) ->


    %% start the db notifier to watch db update events
    _ = couch_event:subscribe_cond(db_updated, [{{DbName, '$1'},
                                                 [{'==', '$1', updated}],
                                                 [true]}]),

    %% start the timer
    {ok, TRef} = timer:send_interval(R, self(), refresh_index),

    %% start to index immediately
    Pid = spawn_link(fun() ->
                             refresh_index(DbName, Index)
                     end),
    {noreply, State#state{refresh_pid=Pid, tref=TRef}};
handle_info(refresh_index, #state{refresh_pid=Pid}=State) when is_pid(Pid) ->
    {noreply, State};
handle_info(refresh_index, #state{index=Index,
                                  dbname=DbName,
                                  db_updates=N}=State) ->

    %% only refresh the index if an update happened
    case N > 0 of
        true ->
            refresh_index(DbName, Index);
        false ->
            ok
    end,
    {noreply, State#state{db_updates=0}};
handle_info({'EXIT', Pid, normal}, #state{refresh_pid=Pid}=State) ->
    {noreply, State#state{refresh_pid=nil}};
handle_info({'EXIT', Pid, Reason}, #state{refresh_pid=Pid}=State) ->
    couch_log:info("Unexpected exit while refreshing index: ~p~n", [Reason]),
    {noreply, State#state{refresh_pid=nil}};
handle_info({'DOWN', MRef, _, Pid, _}, #state{locks=Locks}=State) ->
    NLocks = case dict:find(Pid, Locks) of
        {ok, {MRef, _}} ->
            dict:erase(Pid, Locks);
        error ->
            Locks
    end,

    NState = State#state{locks=NLocks},

    case should_close() of
        true -> {stop, normal, NState};
        false -> {noreply, NState}
    end;

handle_info({couch_event, db_updated, _}, State) ->
    NState = do_update(State),
    {noreply, NState}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{refresh_pid=RPid, tref=TRef}) ->
    if TRef /= nil ->
            timer:cancel(TRef);
        true -> ok
    end,

    case is_pid(RPid) of
        true -> couch_util:shutdown_sync(RPid);
        _ -> ok
    end,
    ok.

%% refresh the index to trigger updates.
refresh_index(Db, Index) ->
    UpdateSeq = couch_util:with_db(Db, fun(WDb) ->
                    couch_db:get_update_seq(WDb)
            end),

    case catch couch_index:get_state(Index, UpdateSeq) of
        {ok, _} -> ok;
        Error -> {error, Error}
    end.

%% if none has acquired us, we could stop the server.
should_close() ->
    case process_info(self(), monitors) of
        {monitors, []} ->   true;
        _ ->                false
    end.


%% number of max updates before refreshing the index. We don't
%% update the index on each db update. Instead we are waiting for a
%% minimum. If the minimum is not acchieved, the update will happen
%% in the next interval.
get_db_threshold() ->
    list_to_integer(
            couch_config:get("couch_index", "threshold", "200")
    ).

%% refresh interval in ms, the interval in which the index will be
%% updated
get_refresh_interval() ->
    list_to_integer(
            couch_config:get("couch_index", "refresh_interval", "1000")
    ).


do_update(#state{index=Index, dbname=DbName,
                 threshold=Threshold,
                 db_updates=Updates}=State) ->

    NUpdates = Updates + 1,

    %% we only update if the number of updates is greater than the
    %% threshold.
    case NUpdates =:= Threshold of
        true ->
            refresh_index(DbName, Index),
            State#state{db_updates=0};
        false ->
            State#state{db_updates=NUpdates}

    end.
