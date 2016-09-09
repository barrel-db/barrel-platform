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
  start/2,
  stop/1,
  clean/1,
  infos/1,
  put/4,
  put_rev/5,
  get/3,
  delete/4,
  post/3,
  fold_by_id/4,
  changes_since/4,
  revsdiff/3
]).

-export([start_link/2]).

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
-export([init_loop/4]).

-define(default_timeout, 5000).

-define(IMAX1, 16#ffffFFFFffffFFFF).

%%%===================================================================
%%% Types
%%%===================================================================

-type state() :: #{}.

-type dbname() :: binary() | list() | atom().
-type doc() :: map().
-type rev() :: binary().
-type docid() :: binary().
-type read_options() :: [{rev, rev()} | {history, boolean()}
  | {max_history, integer()} | {ancestors, [rev()]}].
-type write_options() :: [{async, boolean()} | {timeout, integer()}].

-export_type([
  dbname/0,
  doc/0,
  rev/0,
  docid/0,
  read_options/0,
  write_options/0
]).

%%%===================================================================
%%% API
%%%===================================================================

start(Name, Store) when is_binary(Name)->
  Key = db_key(Name),
  case gproc:where(Key) of
    Pid when is_pid(Pid) -> ok;
    undefined ->
      _ = barrel_dbs_sup:start_db(Name, Store),
      barrel_dbs_sup:await_db(Name)
  end;
start(_, _) -> erlang:error(bad_db).

stop(Name) -> barrel_dbs_sup:stop_db(Name).

clean(Name) ->
  #{store := Store, id := DbId} = call(Name, get_state),
  barrel_store:clean_db(Store, Name, DbId),
  stop(Name).

infos(Name) -> call(Name, get_infos).

%% TODO: handle attachment
%% @doc get a document from its if
-spec get(dbname(), docid(), read_options())
    -> {ok, doc()} | {error, not_found} | {error, any()}.
get(Db, DocId, Options) ->
  Rev = proplists:get_value(rev, Options, <<"">>),
  WithHistory = proplists:get_value(history, Options, false),
  MaxHistory = proplists:get_value(max_history, Options, ?IMAX1),
  Ancestors = proplists:get_value(ancestors, Options, []),
  #{store := Store, id := DbId} = call(Db, get_state),
  barrel_store:get_doc(Store, DbId, DocId, Rev, WithHistory, MaxHistory, Ancestors).


%% @doc create or update a document. Return the new created revision
%% with the docid or a conflict.
-spec put(dbname(), docid(), doc(), write_options())
    -> {ok, docid(), rev()} | {error, {conflict, atom()}} | {error, any()}.
put(Db, DocId, Body, Options) when is_map(Body) ->
  Rev = barrel_doc:rev(Body),
  {Gen, _} = barrel_doc:parse_revision(Rev),
  Deleted = barrel_doc:deleted(Body),

  update_doc(
    Db,
    DocId,
    fun(DocInfo) ->
      #{ current_rev := CurrentRev, revtree := RevTree } = DocInfo,
      Res = case Rev of
              <<>> ->
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
              _ ->
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
  error(bad_doc).

%% @doc insert a specific revision to a a document. Useful for the replication.
%% It takes the document id, the doc to edit and the revision history (list of ancestors).
-spec put_rev(dbname(), docid(), doc(), [rev()], write_options())
    -> {ok, docid(), rev()} | {error, {conflict, atom()}} | {error, any()}.
put_rev(Db, DocId, Body, History, Options) ->
  [NewRev |_] = History,
  Deleted = barrel_doc:deleted(Body),
  update_doc(
    Db,
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
  ).

%% @doc delete a document
-spec delete(dbname(), docid(), rev(), write_options())
    -> {ok, docid(), rev()} | {error, {conflict, atom()}} | {error, any()}.
delete(Db, DocId, RevId, Options) ->
  put(Db, DocId, #{ <<"_id">> => DocId, <<"_rev">> => RevId, <<"_deleted">> => true }, Options).

%% @doc create a document . Like put but only create a document without updating the old one.
%% A doc shouldn't have revision. Optionnaly the document ID can be set in the doc.
-spec post(dbname(),  doc(), write_options())
    -> {ok, docid(), rev()} | {error, {conflict, atom()}} | {error, any()}.
post(_Db, #{<<"_rev">> := _Rev}, _Options) -> {error, not_found};
post(Db, Doc, Options) ->
  DocId = case barrel_doc:id(Doc) of
            undefined -> barrel_lib:uniqid();
            Id -> Id
          end,
  put(Db, DocId, Doc#{<<"_id">> => DocId}, Options).

%% @doc fold all docs by Id
fold_by_id(Db, Fun, Acc, Opts) ->
  #{store := Store, id := DbId} = call(Db, get_state),
  barrel_store:fold_by_id(Store, DbId, Fun, Acc, Opts).

%% @doc fold all changes since last sequence
changes_since(Db, Since, Fun, Acc) ->
  #{store := Store, id := DbId} = call(Db, get_state),
  barrel_store:changes_since(Store, DbId, Since, Fun, Acc).

revsdiff(Db, DocId, RevIds) ->
  #{store := Store, id := DbId} = call(Db, get_state),

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

update_doc(Db, DocId, Fun, Options) ->
  Async = proplists:get_value(async, Options, false),
  Timeout = proplists:get_value(timeout, Options, ?default_timeout),

  case Async of
    true -> spawn(fun() -> update_doc1(Db, DocId, Fun, Options,Timeout) end);
    false -> update_doc1(Db, DocId, Fun, Options, Timeout)
  end.

update_doc1(Db, DocId, Fun, Options, Timeout) ->
  #{writer := Writer} = call(Db, get_state),
  Mref = erlang:monitor(process, Writer),
  Writer ! {update_doc, {self(), Mref}, DocId, Fun, Options},
  receive
    {Mref, Reply} ->
      erlang:demonitor(Mref, [flush]),
      Reply;
    {'DOWN', Mref, _, _, Reason} ->
      exit(Reason)
  after Timeout ->
    erlang:demonitor(Mref, [flush]),
    error(timeout)
  end.

-spec start_link(dbname(), atom()) -> {ok, pid()}.
start_link(Name, Store) ->
  gen_server:start_link(via(Name), ?MODULE, [Name, Store], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%%
-spec init(term()) -> {ok, state()}.
init([Name, Store]) ->
  process_flag(trap_exit, true),
  State = init_db(Name, Store),
  {ok, State}.

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
handle_info({update_seq, Seq}, State = #{ name := Name }) ->
  barrel_db_event:notify(Name, db_updated),
  {noreply, State#{update_seq => Seq}};

handle_info({'EXIT', Pid, Reason}, State) ->
  #{dbid := DbId,
    name := Name,
    store := Store,
    writer := WriterPid}=State,
  if
    Pid =:= WriterPid ->
      lager:info("~p writer crashed: ~p~n", [Name, Reason]),
      
      %% the writer crashed, respawn it
      UpdateSeq = barrel_store:last_update_seq(Store, DbId),
      NewWriter = spawn_writer(Store, DbId, UpdateSeq),
      {noreply, #{update_seq => UpdateSeq, writer => NewWriter}};
    true ->
      {noreply, State}
  end;
handle_info(_Info, State) ->
{noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #{writer := Writer}) ->
  exit(Writer, normal),

  ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

via(Name) ->
  {via, gproc, db_key(Name)}.

db_key(Name) -> {n, l, {db, Name}}.

call(Name, Req) -> gen_server:call(via(Name), Req).

init_db(Name, Store) ->
  {DbId, UpdateSeq} = barrel_store:open_db(Store, Name),
  %% spawn writer actor
  WriterPid = spawn_writer(DbId, Store, UpdateSeq),

  %% return state
  #{
    id => DbId,
    store => Store,
    writer => WriterPid,
    name => Name,
    update_seq => UpdateSeq
  }.

%% TODO: retrieve status from the store
get_infos(State) ->
  #{ id := Id, name := Name, store := Store} = State,
  #{
    id => Id,
    name => Name,
    store => Store
  }.

spawn_writer(DbId, Store, UpdateSeq) ->
  spawn_link(?MODULE, init_loop, [self(), DbId, Store, UpdateSeq]).


init_loop(Parent, Dbid, Store, UpdateSeq) ->
  State = #{
    parent => Parent,
    dbid => Dbid,
    store => Store,
    update_seq => UpdateSeq},
  write_loop(State).

write_loop(State = #{dbid := DbId, store := Store}) ->
  receive
    {update_doc, {Pid, Tag}, DocId, Fun, Options} ->
      {Reply, NewState} = (catch write_doc(DbId, Store, DocId, Fun, Options, State)),
      catch Pid ! {Tag, Reply},
      write_loop(NewState)
  end.

empty_doc_info() ->
  #{ current_rev => <<>>, revtree => #{}}.

write_doc(DbId, Store, DocId, Fun, _Options, State) ->
  #{ parent := Parent, update_seq := Seq} = State,

  DocInfo = case barrel_store:get_doc_info(Store, DbId, DocId) of
              {ok, DI} -> DI;
              {error, not_found} -> empty_doc_info();
              Error -> throw(Error)
            end,

  case Fun(DocInfo) of
    {ok, DocInfo2, Body, NewRev} ->
      NewSeq = Seq + 1,
      LastSeq = maps:get(update_seq, DocInfo2, undefined),
      case barrel_store:write_doc(Store, DbId, DocId, LastSeq, DocInfo2#{update_seq => NewSeq}, Body) of
        ok ->
          Parent ! {update_seq, NewSeq},
          {{ok, DocId, NewRev}, State#{update_seq => NewSeq}};
        WriteError ->
          lager:error("db error: error writing ~p on ~p", [DocId, DbId]),
          %% NOTE: should we retry?
          {WriteError, State}
      end;
    ok ->
      %% NOTE: should we return a cancel message instead?
      #{ current_rev := Rev } = DocInfo,
      {{ok, DocId, Rev}, State};
    Conflict ->
      {{error, Conflict}, State}
  end.

