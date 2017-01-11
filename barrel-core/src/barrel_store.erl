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

-module(barrel_store).
-author("Benoit Chesneau").


%% API
-export([
  infos/1,
  put/3,
  post/3,
  put_rev/3,
  get/3,
  delete/3,
  fold_by_id/4,
  changes_since/5,
  revsdiff/3,
  write_system_doc/3,
  read_system_doc/2,
  delete_system_doc/2,
  query/5,
  query/6
]).


%% internal processes
-define(default_timeout, 5000).

-define(IMAX1, 16#ffffFFFFffffFFFF).

%%%===================================================================
%%% API
%%%===================================================================

infos(StoreName) ->
  Store = store_mod(StoreName),
  Store:infos(StoreName).

get_id(#{ <<"id">> := DocId }) -> DocId;
get_id(_) -> erlang:error({bad_doc, invalid_docid}).

post(StoreName, Doc, Options) when is_map(Doc) ->
  DocId = get_id(Doc),
  Upsert = proplists:get_value(upsert, Options) =:= true,
  update_doc(
    StoreName,
    DocId,
    fun(DocInfo) ->
      #{ current_rev := CurrentRev, revtree := RevTree } = DocInfo,
      Res = if
              CurrentRev /= <<>>, Upsert =:= false ->
                {conflict, doc_exists};
              CurrentRev =:=  <<>> ->
                {ok, 1, <<>>};
              true ->
                {CurrentGen, _} = barrel_doc:parse_revision(CurrentRev),
                {ok, CurrentGen + 1, CurrentRev}
            end,
      case Res of
        {ok, NewGen, ParentRev} ->
          NewRev = barrel_doc:revid(NewGen, ParentRev, Doc),
          RevInfo = #{  id => NewRev,  parent => ParentRev},
          RevTree2 = barrel_revtree:add(RevInfo, RevTree),
          Doc2 = Doc#{<<"_rev">> => NewRev},
          %% update the doc infos
          {WinningRev, Branched, Conflict} = barrel_revtree:winning_revision(RevTree2),
          DocInfo2 = DocInfo#{
            id => DocId,
            current_rev => WinningRev,
            branched => Branched,
            conflict => Conflict,
            revtree => RevTree2
          },
          {ok, DocInfo2, Doc2, NewRev};
        Conflict ->
          Conflict
      end
    end);
post(_, _, _) ->
  erlang:error(badarg).

get_rev(undefined, #{ << "_rev">> := Rev }) -> Rev;
get_rev(Rev, #{ << "_rev">> := Rev }) when is_binary(Rev) -> Rev;
get_rev(Rev, _) when is_binary(Rev) -> Rev;
get_rev(undefined, _) -> <<>>;
get_rev(_, _) -> erlang:error(invalid_rev).

put(StoreName, Doc, Options) when is_map(Doc) ->
  DocId = get_id(Doc),
  Rev = get_rev(proplists:get_value(rev, Options), Doc),
  {Gen, _} = barrel_doc:parse_revision(Rev),
  Deleted = barrel_doc:deleted(Doc),
  update_doc(
    StoreName,
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
          NewRev = barrel_doc:revid(NewGen, Rev, Doc),
          RevInfo = #{  id => NewRev,  parent => ParentRev,  deleted => Deleted},
          RevTree2 = barrel_revtree:add(RevInfo, RevTree),
          Doc2 = Doc#{<<"_rev">> => NewRev},
          %% update the doc infos
          {WinningRev, Branched, Conflict} = barrel_revtree:winning_revision(RevTree2),
          DocInfo2 = DocInfo#{
            id => DocId,
            current_rev => WinningRev,
            branched => Branched,
            conflict => Conflict,
            revtree => RevTree2
          },
          {ok, DocInfo2, Doc2, NewRev};
        Conflict ->
          Conflict
      end
    end);
put(_, _, _) ->
  erlang:error(badarg).

put_rev(StoreName, Doc, History) when is_map(Doc) ->
  DocId = get_id(Doc),
  [NewRev |_] = History,
  Deleted = barrel_doc:deleted(Doc),
  update_doc(
    StoreName,
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
          Doc2 = Doc#{ <<"_rev">> => NewRev },
          {ok, DocInfo2, Doc2, NewRev}
      end
    end);
put_rev(_, _, _) ->
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

%% TODO: handle attachment
get(StoreName, DocId, Options) ->
  Store = store_mod(StoreName),
  Rev = proplists:get_value(rev, Options, <<"">>),
  WithHistory = proplists:get_value(history, Options, false),
  MaxHistory = proplists:get_value(max_history, Options, ?IMAX1),
  Ancestors = proplists:get_value(ancestors, Options, []),
  Store:get_doc(StoreName, DocId, Rev, WithHistory, MaxHistory, Ancestors).

delete(StoreName, DocId, Options) ->
  Rev = proplists:get_value(rev, Options, <<>>),
  update_doc(
    StoreName,
    DocId,
    fun(DocInfo) ->
      #{ current_rev := CurrentRev, revtree := RevTree } = DocInfo,
      Res = case Rev of
              <<>> ->
                if
                  CurrentRev /= <<>> ->
                    case maps:get(CurrentRev, RevTree) of
                      #{deleted := true} -> {error, not_found};
                      _ ->
                        {CurrentGen, _} = barrel_doc:parse_revision(CurrentRev),
                        {ok, CurrentGen + 1, CurrentRev}
                    end;
                  true ->
                    {error, not_found}
                end;
              _ ->
                {Gen, _} = barrel_doc:parse_revision(Rev),
                case barrel_revtree:is_leaf(Rev, RevTree) of
                  true -> {ok, Gen + 1, Rev};
                  false -> {conflict, revision_conflict}
                end
            end,
      case Res of
        {ok, NewGen, ParentRev} ->
          Doc = #{<<"id">> => DocId, <<"_rev">> => ParentRev, <<"_deleted">> => true},
          NewRev = barrel_doc:revid(NewGen, Rev, Doc),
          RevInfo = #{  id => NewRev,  parent => ParentRev,  deleted => true},
          RevTree2 = barrel_revtree:add(RevInfo, RevTree),
          Doc2 = Doc#{<<"_rev">> => NewRev},
          %% update the doc infos
          {WinningRev, Branched, Conflict} = barrel_revtree:winning_revision(RevTree2),
          DocInfo2 = DocInfo#{
            id => DocId,
            current_rev => WinningRev,
            branched => Branched,
            conflict => Conflict,
            revtree => RevTree2
          },
          {ok, DocInfo2, Doc2, NewRev};
        Error ->
          Error
      end
    end).

fold_by_id(StoreName, Fun, Acc, Opts) ->
  Store = store_mod(StoreName),
  Store:fold_by_id(StoreName, Fun, Acc, Opts).

changes_since(StoreName, Since0, Fun, Acc, Opts) when is_integer(Since0) ->
  Since = if
            Since0 > 0 -> Since0 + 1;
            true -> Since0
          end,
  Store = store_mod(StoreName),
  Store:changes_since(StoreName, Since, Fun, Acc, Opts).

revsdiff(StoreName, DocId, RevIds) ->
  Store = store_mod(StoreName),
  case Store:get_doc_info(StoreName, DocId, []) of
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


update_doc(StoreName, DocId, Fun) ->
  Store = store_mod(StoreName),
  Store:update_doc(StoreName, DocId, Fun).

write_system_doc(StoreName, DocId, Doc) ->
  Store = store_mod(StoreName),
  Store:write_system_doc(StoreName, DocId, Doc).

read_system_doc(StoreName, DocId) ->
  Store = store_mod(StoreName),
  Store:read_system_doc(StoreName, DocId).

delete_system_doc(StoreName, DocId) ->
  Store = store_mod(StoreName),
  Store:delete_system_doc(StoreName, DocId).


query(StoreName, Path, Fun, AccIn, Options) ->
  barrel_query:query(StoreName, Path, Fun, AccIn, Options).

query(StoreName, Path, Fun, AccIn, OrderBy, Options) ->
  barrel_query:query(StoreName, Path, Fun, AccIn, OrderBy, Options).

store_mod(Name) ->
  case catch ets:lookup_element(barrel_stores, Name, 2) of
    {'EXIT', _} -> error(bad_store);
    Store -> Store
  end.
