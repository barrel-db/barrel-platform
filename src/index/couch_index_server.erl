%% Copyright 2015-2016, Benoit Chesneau
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

-module(couch_index_server).
-behaviour(gen_server).

-export([start_link/0, get_index/4, get_index/3, get_index/2]).
-export([acquire_indexer/3, release_indexer/3]).
-export([config_change/3]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% hook
-export([index_update/3, index_reset/3]).

-include_lib("couch_db.hrl").

-define(BY_SIG, couchdb_indexes_by_sig).
-define(BY_PID, couchdb_indexes_by_pid).
-define(BY_DB, couchdb_indexes_by_db).


-record(st, {root_dir}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_index(Module, DbName, DDoc) ->
    get_index(Module, DbName, DDoc, nil).


get_index(Module, DbName, DDoc, Fun) when is_binary(DbName) ->
    barrel_lib:with_db(DbName, fun(Db) ->
        get_index(Module, Db, DDoc, Fun)
    end);
get_index(Module, Db, DDoc, Fun) when is_binary(DDoc) ->
    case couch_db:open_doc(Db, DDoc, [ejson_body]) of
        {ok, Doc} -> get_index(Module, Db, Doc, Fun);
        Error -> Error
    end;
get_index(Module, Db, DDoc, Fun) when is_function(Fun, 1) ->
    {ok, InitState} = Module:init(Db, DDoc),
    {ok, FunResp} = Fun(InitState),
    {ok, Pid} = get_index(Module, InitState),
    {ok, Pid, FunResp};
get_index(Module, Db, DDoc, _Fun) ->
    {ok, InitState} = Module:init(Db, DDoc),
    get_index(Module, InitState).


get_index(Module, IdxState) ->
    DbName = Module:get(db_name, IdxState),
    Sig = Module:get(signature, IdxState),
    case ets:lookup(?BY_SIG, {DbName, Sig}) of
        [{_, Pid}] when is_pid(Pid) ->
            {ok, Pid};
        _ ->
            Args = {Module, IdxState, DbName, Sig},
            gen_server:call(?MODULE, {get_index, Args}, infinity)
    end.

acquire_indexer(Module, DbName, DDoc) ->
    case get_index(Module, DbName, DDoc) of
        {ok, Pid} ->
            couch_index:acquire_indexer(Pid);
        Error ->
            Error
    end.

release_indexer(Module, DbName, DDoc) ->
    case get_index(Module, DbName, DDoc) of
        {ok, Pid} ->
            couch_index:release_indexer(Pid);
        Error ->
            Error
    end.


index_update(DbName, DDocId, Mod) ->
    barrel_event:notify(DbName, DDocId, {updated, Mod}).

index_reset(DbName, DDocId, Mod) ->
    barrel_event:notify(DbName, DDocId, {reset, Mod}).

init([]) ->
    process_flag(trap_exit, true),
    hooks:reg(config_index_update, ?MODULE, config_change, 3),
    ets:new(?BY_SIG, [protected, set, named_table]),
    ets:new(?BY_PID, [private, set, named_table]),
    ets:new(?BY_DB, [protected, bag, named_table]),

    %% register to db changes (only created and deleted events)
    barrel_event:reg_all(),

    %% initiase index hooks
    %%
    hooks:reg(index_update, ?MODULE, index_update, 3),
    hooks:reg(index_reset, ?MODULE, index_reset, 3),

    RootDir = couch_index_util:root_dir(),
    couch_file:init_delete_dir(RootDir),
    {ok, #st{root_dir=RootDir}}.


terminate(_Reason, _State) ->
    %% unregister hooks
    hooks:unreg(index_update, ?MODULE, index_update, 3),
    hooks:unreg(index_reset, ?MODULE, index_reset, 3),
    %% kil index processes
    Pids = [Pid || {Pid, _} <- ets:tab2list(?BY_PID)],
    lists:map(fun barrel_lib:shutdown_sync/1, Pids),
    ok.


handle_call({get_index, {_Mod, _IdxState, DbName, Sig}=Args}, From, State) ->
    case ets:lookup(?BY_SIG, {DbName, Sig}) of
        [] ->
            spawn_link(fun() -> new_index(Args) end),
            ets:insert(?BY_SIG, {{DbName, Sig}, [From]}),
            {noreply, State};
        [{_, Waiters}] when is_list(Waiters) ->
            ets:insert(?BY_SIG, {{DbName, Sig}, [From | Waiters]}),
            {noreply, State};
        [{_, Pid}] when is_pid(Pid) ->
            {reply, {ok, Pid}, State}
    end;
handle_call({async_open, {DbName, DDocId, Sig}, {ok, Pid}}, _From, State) ->
    [{_, Waiters}] = ets:lookup(?BY_SIG, {DbName, Sig}),
    [gen_server:reply(From, {ok, Pid}) || From <- Waiters],
    link(Pid),
    add_to_ets(DbName, Sig, DDocId, Pid),
    {reply, ok, State};
handle_call({async_error, {DbName, _DDocId, Sig}, Error}, _From, State) ->
    [{_, Waiters}] = ets:lookup(?BY_SIG, {DbName, Sig}),
    [gen_server:reply(From, Error) || From <- Waiters],
    ets:delete(?BY_SIG, {DbName, Sig}),
    {reply, ok, State};
handle_call({reset_indexes, DbName}, _From, State) ->
    reset_indexes(DbName, State#st.root_dir),
    {reply, ok, State}.


handle_cast({reset_indexes, DbName}, State) ->
    reset_indexes(DbName, State#st.root_dir),
    {noreply, State}.

handle_info({'$barrel_event', DbName, Event}, Server) ->
    case lists:member(Event, [created, deleted]) of
      true -> reset_indexes(DbName, Server#st.root_dir);
      false -> ok
    end,
    {noreply, Server};

handle_info({'EXIT', Pid, Reason}, Server) ->
    case ets:lookup(?BY_PID, Pid) of
        [{Pid, {DbName, Sig}}] ->
            [{DbName, {DDocId, Sig}}] =
                ets:match_object(?BY_DB, {DbName, {'$1', Sig}}),
            rem_from_ets(DbName, Sig, DDocId, Pid);
        [] when Reason /= normal ->
            lager:warning("unhandled error ~p~n", [Reason]),
            ok;
        _Else ->
            ok
    end,
    {noreply, Server};
handle_info(Msg, State) ->
    lager:warn("~p did not expect ~p", [?MODULE, Msg]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


new_index({Mod, IdxState, DbName, Sig}) ->
    DDocId = Mod:get(idx_name, IdxState),
    case couch_index:start_link({Mod, IdxState}) of
        {ok, Pid} ->
            ok = gen_server:call(
                ?MODULE, {async_open, {DbName, DDocId, Sig}, {ok, Pid}}),
            unlink(Pid);
        Error ->
            ok = gen_server:call(
                ?MODULE, {async_error, {DbName, DDocId, Sig}, Error})
    end.


reset_indexes(DbName, Root) ->
    % shutdown all the updaters and clear the files, the db got changed
    Fun = fun({_, {DDocId, Sig}}) ->
        [{_, Pid}] = ets:lookup(?BY_SIG, {DbName, Sig}),
        MRef = erlang:monitor(process, Pid),
        gen_server:cast(Pid, delete),
        receive {'DOWN', MRef, _, _, _} -> ok end,
        rem_from_ets(DbName, Sig, DDocId, Pid)
    end,
    lists:foreach(Fun, ets:lookup(?BY_DB, DbName)),
    Path = couch_index_util:index_dir("", DbName),
    catch couch_file:nuke_dir(Root, Path).


add_to_ets(DbName, Sig, DDocId, Pid) ->
    ets:insert(?BY_SIG, {{DbName, Sig}, Pid}),
    ets:insert(?BY_PID, {Pid, {DbName, Sig}}),
    ets:insert(?BY_DB, {DbName, {DDocId, Sig}}).


rem_from_ets(DbName, Sig, DDocId, Pid) ->
    ets:delete(?BY_SIG, {DbName, Sig}),
    ets:delete(?BY_PID, Pid),
    ets:delete_object(?BY_DB, {DbName, {DDocId, Sig}}).


config_change("barrel", "view_index_dir", _) ->
    exit(whereis(?MODULE), config_change).
