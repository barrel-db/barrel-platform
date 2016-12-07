%% Copyright 2016, Benoit Chesneau
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

-module(barrel_db).
-author("Benoit Chesneau").
-behaviour(gen_server).

%% API
-export([
  start/3,
  stop/1,
  clean/1,
  infos/1,
  put/4,
  put_rev/5,
  get/3,
  delete/4,
  post/3,
  fold_by_id/4,
  changes_since/5,
  revsdiff/3,
  write_system_doc/3,
  read_system_doc/2,
  delete_system_doc/2
]).

-export([
  start_link/3,
  where/1,
  get_state/1,
  call/2
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%% internal processes
-define(default_timeout, 5000).

-define(IMAX1, 16#ffffFFFFffffFFFF).

%%%===================================================================
%%% Types
%%%===================================================================

-type state() :: #{}.

%%%===================================================================
%%% API
%%%===================================================================

start(Name, Store, Options) when is_binary(Name)->
  case whereis(Store) of
    PidStore when is_pid(PidStore) ->
      case ets:lookup(?MODULE, {Store, Name}) of
        [{{Store, Name}, DbPid}]  when is_pid(DbPid) ->
          {false, make_conn(DbPid)};
        [] ->
          case barrel_db_sup:start_db(Name, Store, Options) of
            {ok, DbPid} ->  {true, make_conn(DbPid)};
            {error, {already_started, DbPid}} ->   {false, make_conn(DbPid)};
            {error, {{error, not_found}, _}} -> {error, not_found};
            Error -> Error
          end
      end;
    undefined ->
      {error, {unknown_store, Store}}
  end;
start(_, _, _) ->
  erlang:error(bad_db).

stop(#{ name := Name, store := Store}) ->
  barrel_db_sup:stop_db({Store, Name}).

clean(#{name := Name, store := Store, id := DbId}=Conn) ->
  barrel_store:clean_db(Store, Name, DbId),
  stop(Conn).

infos(Conn) -> call(Conn, get_infos).

get_state(Pid) when is_pid(Pid) ->
  gen_server:call(Pid, get_state);
get_state(Conn) when is_map(Conn) ->
  call(where(Conn), get_state);
get_state(_) -> erlang:error(bad_conn).

make_conn(DbPid) ->
  #{ id := DbId, name := Name, store := Store } = get_state(DbPid),
  #{ id => DbId, name => Name, store => Store }.

where(#{ store := Store, name := Name}) ->
  case ets:lookup(?MODULE, {Store, Name}) of
    [] -> undefined;
    [{{Store, Name}, Pid}] -> Pid
  end.

call(Conn, Msg) -> gen_server:call(where(Conn), Msg).

%% TODO: handle attachment
get(#{store := Store, id := DbId}, DocId, Options) ->
  Rev = proplists:get_value(rev, Options, <<"">>),
  WithHistory = proplists:get_value(history, Options, false),
  MaxHistory = proplists:get_value(max_history, Options, ?IMAX1),
  Ancestors = proplists:get_value(ancestors, Options, []),
  barrel_store:get_doc(Store, DbId, DocId, Rev, WithHistory, MaxHistory, Ancestors).


put(Conn, DocId, Body, Options) when is_map(Body) ->
  ok = check_docid(DocId, Body),
  Rev = barrel_doc:rev(Body),
  {Gen, _} = barrel_doc:parse_revision(Rev),
  Deleted = barrel_doc:deleted(Body),
  Lww = proplists:get_value(lww, Options, false),

  update_doc(
    Conn,
    DocId,
    fun(DocInfo) ->
      #{ current_rev := CurrentRev, revtree := RevTree } = DocInfo,
      Res = case {Lww, Rev} of
              {true, _} ->
                if
                  CurrentRev /= <<>> ->
                    {CurrentGen, _} = barrel_doc:parse_revision(CurrentRev),
                    {ok, CurrentGen + 1, CurrentRev};
                  true ->
                    {ok, Gen + 1, <<>>}
                end;
              {false, <<>>} ->
                if
                  CurrentRev /= <<>> ->
                    case maps:get(CurrentRev, RevTree) of
                      #{deleted := true} ->
                        {CurrentGen, _} = barrel_doc:parse_revision(CurrentRev),
                        {ok, CurrentGen + 1, CurrentRev};
                      _ ->
                        {conflict, doc_exists}
                    end;
                  true ->
                    {ok, Gen + 1, Rev}
                end;
              {false, _} ->
                case barrel_revtree:is_leaf(Rev, RevTree) of
                  true -> {ok, Gen + 1, Rev};
                  false -> {conflict, revision_conflict}
                end
            end,
      case Res of
        {ok, NewGen, ParentRev} ->
          NewRev = barrel_doc:revid(NewGen, Rev, Body),
          RevInfo = #{  id => NewRev,  parent => ParentRev,  deleted => Deleted},
          RevTree2 = barrel_revtree:add(RevInfo, RevTree),
          Body2 = Body#{<<"_rev">> => NewRev},
          %% update the doc infos
          {WinningRev, Branched, Conflict} = barrel_revtree:winning_revision(RevTree2),
          DocInfo2 = DocInfo#{
            id => DocId,
            current_rev => WinningRev,
            branched => Branched,
            conflict => Conflict,
            revtree => RevTree2
          },
          {ok, DocInfo2, Body2, NewRev};
        Conflict ->
          Conflict
      end
    end,
    Options);
put(_, _, _, _) ->
  erlang:error(badarg).

put_rev(Conn, DocId, Body, History, Options) when is_map(Body) ->
  ok = check_docid(DocId, Body),
  [NewRev |_] = History,
  Deleted = barrel_doc:deleted(Body),
  Res = update_doc(
    Conn,
    DocId,
    fun(DocInfo) ->
      #{revtree := RevTree} = DocInfo,
      {Idx, Parent} = find_parent(History, RevTree, 0),
      if
        Idx =:= 0 -> ok;
        true ->
          ToAdd = lists:sublist(History, Idx),
          RevTree2 = edit_revtree(ToAdd, Parent, Deleted, RevTree),
          {WinningRev, Branched, Conflict} = barrel_revtree:winning_revision(RevTree2),
          DocInfo2 = DocInfo#{
            id => DocId,
            current_rev => WinningRev,
            branched => Branched,
            conflict => Conflict,
            revtree => RevTree2
          },
          Body2 = Body#{ <<"_rev">> => NewRev },
          {ok, DocInfo2, Body2, NewRev}
      end
    end,
    Options
  ),
  case Res of
    {ok, _, _} -> ok;
    Error -> Error
  end;
put_rev(_, _, _, _, _) ->
  erlang:error(badarg).

edit_revtree([RevId], Parent, Deleted, Tree) ->
  case Deleted of
    true ->
      barrel_revtree:add(#{ id => RevId, parent => Parent, deleted => true}, Tree);
    false ->
      barrel_revtree:add(#{ id => RevId, parent => Parent}, Tree)
  end;
edit_revtree([RevId | Rest], Parent, Deleted, Tree) ->
  Tree2 = barrel_revtree:add(#{ id => RevId, parent => Parent}, Tree),
  edit_revtree(Rest, Parent, Deleted, Tree2);
edit_revtree([], _Parent, _Deleted, Tree) ->
  Tree.

find_parent([RevId | Rest], RevTree, I) ->
  case barrel_revtree:contains(RevId, RevTree) of
    true -> {I, RevId};
    false -> find_parent(Rest, RevTree, I+1)
  end;
find_parent([], _RevTree, I) ->
  {I, <<"">>}.

delete(Conn, DocId, RevId, Options) ->
  put(Conn, DocId, #{ <<"id">> => DocId, <<"_rev">> => RevId, <<"_deleted">> => true }, Options).


post(_Conn, #{<<"_rev">> := _Rev}, _Options) -> {error, not_found};
post(Conn, Doc, Options) ->
  DocId = case barrel_doc:id(Doc) of
            undefined -> barrel_lib:uniqid();
            Id -> Id
          end,
  put(Conn, DocId, Doc#{<<"id">> => DocId}, Options).


fold_by_id(#{store := Store, id := DbId}, Fun, Acc, Opts) ->
  barrel_store:fold_by_id(Store, DbId, Fun, Acc, Opts).

changes_since(#{store := Store, id := DbId}, Since0, Fun, Acc, Opts) when is_integer(Since0) ->
  Since = if
            Since0 > 0 -> Since0 + 1;
            true -> Since0
          end,
  barrel_store:changes_since(Store, DbId, Since, Fun, Acc, Opts).

revsdiff(#{store := Store, id := DbId}, DocId, RevIds) ->
  case barrel_store:get_doc_info(Store, DbId, DocId) of
    {ok, #{revtree := RevTree}} -> revsdiff1(RevTree, RevIds);
    {error, not_found} -> {ok, RevIds, []};
    Error -> Error
  end.

revsdiff1(RevTree, RevIds) ->
  {Missing, PossibleAncestors} = lists:foldl(
    fun(RevId, {M, A} = Acc) ->
      case barrel_revtree:contains(RevId, RevTree) of
        true -> Acc;
        false ->
          M2 = [RevId | M],
          {Gen, _} = barrel_doc:parse_revision(RevId),
          A2 = barrel_revtree:fold_leafs(
            fun(#{ id := Id}=RevInfo, A1) ->
              Parent = maps:get(parent, RevInfo, <<"">>),
              case lists:member(Id, RevIds) of
                true ->
                  {PGen, _} = barrel_doc:parse_revision(Id),
                  if
                    PGen < Gen -> [Id | A1];
                    PGen =:= Gen, Parent =/= <<"">> -> [Parent | A1];
                    true -> A1
                  end;
                false -> A1
              end
            end, A, RevTree),
          {M2, A2}
      end
    end, {[], []}, RevIds),
  {ok, lists:reverse(Missing), lists:usort(PossibleAncestors)}.



update_doc(#{id := DbId}, DocId, Fun, Options) ->
  barrel_transactor:update_doc(DbId, DocId, Fun, Options).

write_system_doc(#{store := Store, id := DbId}, DocId, Doc) ->
  barrel_store:write_system_doc(Store, DbId, DocId, Doc).

read_system_doc(#{store := Store, id := DbId}, DocId) ->
  barrel_store:read_system_doc(Store, DbId, DocId).

delete_system_doc(#{store := Store, id := DbId}, DocId) ->
  barrel_store:delete_system_doc(Store, DbId, DocId).


-spec start_link(barrel:dbname(), barrel:store(), barrel:db_options()) -> {ok, pid()}.
start_link(Name, Store, Options) ->
  gen_server:start_link(?MODULE, [Name, Store, Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%%
-spec init(term()) -> {ok, state()}.
init([Name, Store, Options]) ->
  case init_db(Name, Store, Options) of
    {ok, State} ->
      process_flag(trap_exit, true),
      ets:insert(?MODULE, {{Store, Name}, self()}),
      {ok, State};
    Error ->
      {stop, Error}
  end.

-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.
handle_call(get_infos, _From, State) ->
  Infos = get_infos(State),
  {reply, {ok, Infos}, State};

handle_call(get_state, _From, State) ->
  {reply, State, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({updated, Seq}, State = #{ store := Store, name := Name, waiters := Waiters }) ->
  barrel_db_event:notify({Store, Name}, db_updated),
  Waiters2 = process_waiters(Waiters, Seq),
  {noreply, State#{waiters => Waiters2}};

handle_info({wait_changes, Pid, Since}, State = #{ waiters := Waiters, update_seq := Seq}) ->
  State2 = if
    Seq > Since ->
      Pid ! {updated, Since},
      State;
    true ->
      Waiters2 = queue:in({Pid, Since}, Waiters),
      State#{ waiters => Waiters2 }
  end,
  {noreply, State2};

handle_info({'EXIT', Pid, Reason},State) ->
  #{id := DbId,
    name := Name,
    store := Store,
    writer := WriterPid,
    updater := Updater,
    options := Options}=State,
  case Pid of
    WriterPid ->
      lager:info("~p writer crashed: ~p~n", [Name, Reason]),
      %% the writer crashed, respawn it
      UpdateSeq = barrel_store:last_update_seq(Store, DbId),
      {ok, NewWriter} = barrel_transactor:start_link(self(), DbId, Store, Options),
      lager:info("~p new writer spawned: dbid=~p store=~p~n", [Name, DbId, Store]),
      {noreply, State#{update_seq => UpdateSeq, writer => NewWriter}};
    Updater ->
      lager:info("~p updater crashed: ~p~n", [Name, Reason]),
      Indexer = barrel_indexer:start_link(self(), DbId, Store),
      {noreply, State#{ indexer => Indexer }};
    _ ->
      {noreply, State}
  end;
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #{name := Name, store := Store, writer := Writer}) ->
  ets:delete(?MODULE, {Store, Name}),
  exit(Writer, normal),
  ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

init_db(Name, Store, Options) ->
  case barrel_store:open_db(Store, Name, Options) of
    {ok, {DbId, UpdateSeq}} ->
      %% spawn writer actor
      {ok, WriterPid} = barrel_transactor:start_link(self(), DbId, Store, UpdateSeq),
      %% spawn indexer
      Indexer = barrel_indexer:start_link(self(), DbId, Store),
      %% return state
      {ok, #{
        id => DbId,
        store => Store,
        writer => WriterPid,
        indexer => Indexer,
        name => Name,
        update_seq => UpdateSeq,
        options => Options,
        waiters => queue:new()
      }};
    Error ->
      Error
  end.

process_waiters(W, Seq) -> process_waiters_1(W, Seq, queue:new()).

process_waiters_1(Waiters, Seq, NW) ->
  case queue:out(Waiters) of
    {{value, {Pid, Since}}, Waiters2} when Seq > Since ->
      catch Pid ! {updated, Since},
      process_waiters_1(Waiters2, Seq, NW);
    {{value, Waiter}, Waiters2} ->
      process_waiters_1(Waiters2, Seq, queue:in(Waiter, NW));
    {empty, _} ->
      NW
  end.

%% TODO: retrieve status from the store
get_infos(State) ->
  #{ id := Id, name := Name, store := Store} = State,
  #{
    id => Id,
    name => Name,
    store => Store
  }.

check_docid(DocId, #{ <<"id">> := DocId }) -> ok;
check_docid(_, _) -> erlang:error({bad_doc, invalid_docid}).
