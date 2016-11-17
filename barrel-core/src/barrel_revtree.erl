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

-module(barrel_revtree).

-export([
  new/0, new/1
  , add/2
  , contains/2
  , info/2
  , parent/2
  , history/2
  , fold_leafs/3
  , leaves/1
  , is_leaf/2
  , missing_revs/2
  , winning_revision/1
  , prune/2, prune/3]).

-export([is_deleted/1]).

-define(IMAX1, 16#ffffFFFFffffFFFF).

new() -> #{}.

new(#{ id := Id } = RevInfo) -> #{ Id => RevInfo }.

add(RevInfo, Tree) ->
  #{id := Id, parent := Parent} = RevInfo,

  case maps:is_key(Id, Tree) of
    true -> error({badrev, already_exists});
    false -> ok
  end,

  case maps:is_key(Parent, Tree) of
    true -> ok;
    false when Parent =:= <<"">> -> ok;
    false -> error({badrev, missing_parent})
  end,

  Tree#{ Id => RevInfo }.


contains(RevId, Tree) ->
  maps:is_key(RevId, Tree).


info(RevId, Tree) ->
  case maps:find(RevId, Tree) of
    error -> {error, not_found};
    {ok, RevInfo} -> {ok, RevInfo}
  end.


parent(RevId, Tree) ->
  case maps:find(RevId, Tree) of
    error -> <<"">>;
    {ok, RevInfo} -> maps:get(parent, RevInfo, <<"">>)
  end.

history(RevId, Tree) ->
  history1(maps:get(RevId, Tree, nil), Tree, []).

history1(nil, _Tree, History) ->
  lists:reverse(History);
history1(#{id := Id, parent := Parent}, Tree, History) ->
  history1(maps:get(Parent, Tree, nil), Tree, [Id | History]).

fold_leafs(Fun, AccIn, Tree) ->
  Parents = maps:fold(fun
                        (_Id, #{ parent := Parent }, Acc) when Parent /= <<"">> ->
                          [Parent | Acc];
                        (_Id, _RevInfo, Acc) ->
                          Acc
                      end, [], Tree),


  maps:fold(fun(RevId, RevInfo, Acc) ->
                case lists:member(RevId, Parents) of
                  true -> Acc;
                  false -> Fun(RevInfo, Acc)
                end
            end, AccIn, Tree).

leaves(Tree) ->
  fold_leafs(fun(#{id := RevId}, Acc) ->
                 [ RevId | Acc ]
             end, [], Tree).

missing_revs(Revs, RevTree) ->
  Leaves = barrel_revtree:fold_leafs(
    fun(#{ id := Id}, Acc) ->
      case lists:member(Id, Revs) of
        true -> [Id | Acc];
        false -> Acc
      end
    end, [], RevTree),
  Revs -- Leaves.
  

is_leaf(RevId, Tree) ->
  case maps:is_key(RevId, Tree) of
    true ->
      try
        maps:fold(fun(_, #{parent := Parent}, _) ->
                      if
                        Parent =:= RevId -> throw(has_parent);
                        true -> true
                      end
                  end, true, Tree)
      catch
        has_parent -> false
      end;
    false ->
      false
  end.


is_deleted(#{deleted := Del}) -> Del;
is_deleted(_) -> false.

winning_revision(Tree) ->
  {Winner, _WinnerExists, LeafCount, ActiveLeafCount} =
  fold_leafs(fun(#{id := Id} = RevInfo, {W, WE, LC, ALC}) ->
                 Deleted = is_deleted(RevInfo),
                 Exists = Deleted =:= false,
                 LC2 = LC + 1,
                 ALC2 = case Exists of
                          true -> ALC + 1;
                          false -> ALC
                        end,

                 {W2, WE2} = case {Exists, WE} of
                               {true, false} -> {Id, Exists};
                               {WE, _} ->
                                 case barrel_doc:compare(Id, W) of
                                   R when R > 0 -> {Id, Exists};
                                   _ -> {W, WE}
                                 end;
                               _ -> {W, WE}
                             end,

                 {W2, WE2, LC2, ALC2}
             end, {<<"">>, false, 0, 0}, Tree),
  Branched = (LeafCount > 1),
  Conflict = (ActiveLeafCount > 1),
  {Winner, Branched, Conflict}.


prune(Depth, Tree) ->
	prune(Depth, <<"">>, Tree).

prune(Depth, KeepRev, Tree) ->
  Sz = maps:size(Tree),
  if
    Sz =< Depth -> {0, Tree};
    true -> do_prune(Depth, KeepRev, Tree)
  end.


do_prune(Depth, KeepRev, Tree) ->
  {MinPos0, MaxDeletedPos} = fold_leafs(fun(#{id := RevId}=RevInfo, {MP, MDP}) ->
                                            Deleted = is_deleted(RevInfo),
                                            {Pos, _} = barrel_doc:parse_revision(RevId),
                                            case Deleted of
                                              true when Pos > MDP ->
                                                {MP, Pos};
                                              _ when Pos > 0, Pos < MP ->
                                                {Pos, MDP};
                                              _ ->
                                                {MP, MDP}
                                            end
                                        end, {?IMAX1, 0}, Tree),
  MinPos = if
             MinPos0 =:= ?IMAX1 -> MaxDeletedPos;
             true -> MinPos0
           end,
  MinPosToKeep0 = MinPos - Depth + 1,
  {PosToKeep, _} = barrel_doc:parse_revision(KeepRev),
  MinPosToKeep = if
                   PosToKeep > 0,  PosToKeep < MinPosToKeep0 -> PosToKeep;
                   true -> MinPosToKeep0
                 end,
  if
    MinPosToKeep > 1 ->
      maps:fold(fun(RevId, RevInfo, {N, NewTree}) ->
                    {Pos, _} = barrel_doc:parse_revision(RevId),
                    if
                      Pos < MinPosToKeep ->
                        {N + 1, maps:remove(RevId, NewTree)};
                      Pos =:= MinPosToKeep ->
                        RevInfo2 = RevInfo#{parent => <<"">>},
                        {N, NewTree#{RevId => RevInfo2}};
                      true ->
                        {N, NewTree}
                    end
                end, {0, Tree}, Tree);
    true ->
      {0, Tree}
  end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% 1-one -> 2-two -> 3-three
-define(FLAT_TREE, #{<<"1-one">> => #{ id => <<"1-one">> },
                     <<"2-two">> => #{ id => <<"2-two">>, parent => <<"1-one">>},
                     <<"3-three">> => #{ id => <<"3-three">>, parent => <<"2-two">>}}).

%%                  3-three
%%                /
%% 1-one -> 2-two
%%                \
%%                  3-three-2
-define(BRANCHED_TREE, #{<<"1-one">> => #{ id => <<"1-one">> },
                         <<"2-two">> => #{ id => <<"2-two">>, parent => <<"1-one">> },
                         <<"3-three">> => #{ id => <<"3-three">>, parent => <<"2-two">> },
                         <<"3-three-2">> => #{ id => <<"3-three-2">>, parent => <<"2-two">> }}).

contains_test() ->
  ?assert(barrel_revtree:contains(<<"2-two">>, ?FLAT_TREE)).


parent_test() ->
  ?assertEqual(<<"">>, barrel_revtree:parent(<<"1-one">>, ?FLAT_TREE)),
  ?assertEqual(<<"1-one">>, barrel_revtree:parent(<<"2-two">>, ?FLAT_TREE)),
  ?assertEqual(<<"2-two">>, barrel_revtree:parent(<<"3-three">>, ?FLAT_TREE)),
  ?assertEqual(<<"2-two">>, barrel_revtree:parent(<<"3-three">>, ?BRANCHED_TREE)),
  ?assertEqual(<<"2-two">>, barrel_revtree:parent(<<"3-three-2">>, ?BRANCHED_TREE)),
  ok.

add_test() ->
  NewRev = #{ id => <<"4-four">>, parent => <<"3-three">> },
  NewTree = barrel_revtree:add(NewRev, ?FLAT_TREE),

  ?assert(barrel_revtree:contains(<<"4-four">>, NewTree)),
  ?assertEqual(<<"3-three">>, barrel_revtree:parent(<<"4-four">>, NewTree)),
  ?assertError({badrev, already_exists}, barrel_revtree:add(NewRev, NewTree)),
  ?assertError({badrev, missing_parent},
               barrel_revtree:add(#{ id => <<"6-six">>, parent => <<"5-five">>}, NewTree)).

leafs_test() ->
    ?assertEqual([<<"3-three">>, <<"3-three-2">>], lists:sort(barrel_revtree:leaves(?BRANCHED_TREE))),
    NewTree = barrel_revtree:add(#{ id => <<"4-four">>, parent => <<"3-three">>},?BRANCHED_TREE),
    ?assertEqual([<<"3-three-2">>, <<"4-four">>], lists:sort(barrel_revtree:leaves(NewTree))),
    NewTree2 = barrel_revtree:add(#{ id => <<"5-five">>, parent => <<"4-four">>, deleted => true }, NewTree),
    ?assertEqual([<<"3-three-2">>, <<"5-five">>],  lists:sort(barrel_revtree:leaves(NewTree2))).

winning_revision_test() ->
    ?assertEqual({<<"3-three-2">>, true, true}, barrel_revtree:winning_revision(?BRANCHED_TREE)),
    NewTree = barrel_revtree:add(#{ id => <<"4-four">>, parent => <<"3-three">>},?BRANCHED_TREE),
    ?assertEqual({<<"4-four">>, true, true}, barrel_revtree:winning_revision(NewTree)),
    NewTree2 = barrel_revtree:add(#{ id => <<"5-five">>, parent => <<"4-four">>, deleted => true }, NewTree),
    ?assertEqual({<<"3-three-2">>, true, false},  barrel_revtree:winning_revision(NewTree2)).

prune_test() ->
  Tree = barrel_revtree:add(#{ id => <<"4-four">>, parent => <<"3-three-2">> }, ?BRANCHED_TREE),
  {0, Tree} = barrel_revtree:prune(1000, <<"">>, Tree),
  {0, Tree} = barrel_revtree:prune(3, <<"">>, Tree),
  {1, Tree1} = barrel_revtree:prune(2, <<"">>, Tree),
  ?assertEqual(4, maps:size(Tree1)),
  ?assertEqual(false, barrel_revtree:contains(<<"1-one">>, Tree1)),
  ?assertEqual(<<"">>, maps:get(parent, maps:get(<<"2-two">>, Tree1))),

  %% make sure merged conflicts don't prevevent prunint
  {1, Tree2} = barrel_revtree:prune(1, "", Tree1),
  ?assertEqual(3, maps:size(Tree2)),
  ?assertEqual(true, barrel_revtree:contains(<<"3-three">>, Tree2)),
  ?assertEqual(<<"">>, maps:get(parent, maps:get(<<"3-three">>, Tree2))),
  ?assertEqual(true, barrel_revtree:contains(<<"4-four">>, Tree2)),
  ?assertEqual(<<"3-three-2">>, maps:get(parent, maps:get(<<"4-four">>, Tree2))),


  TreeB = lists:foldl(fun(Rev, T) ->
                          barrel_revtree:add(Rev, T)
                      end,
                      ?BRANCHED_TREE,
                      [#{ id => <<"4-four-2">>, parent => <<"3-three-2">>, deleted => true},
                       #{ id => <<"4-four">>, parent => <<"3-three">>},
                       #{ id => <<"5-five">>, parent => <<"4-four">> },
                       #{ id => <<"6-six">>, parent => <<"5-five">> }]),

  {0, TreeB} = barrel_revtree:prune(3, <<"1-one">>, TreeB),
  {1, TreeB1} = barrel_revtree:prune(3, <<"2-two">>, TreeB),
  {3, TreeB2} = barrel_revtree:prune(3, <<"">>, TreeB1),
  ?assertEqual(4, maps:size(TreeB2)),
  {2, TreeB3} = barrel_revtree:prune(2, <<"">>, TreeB2),
  ?assertEqual(<<"">>, maps:get(parent, maps:get(<<"5-five">>, TreeB3))),
  ?assertEqual(<<"5-five">>, maps:get(parent, maps:get(<<"6-six">>, TreeB3))),
  
  TreeC = maps:map(fun(RevId, RevInfo) ->
                       case lists:member(RevId, [<<"3-three">>, <<"3-three-2">>]) of
                         true ->
                           RevInfo#{deleted => true};
                         false ->
                           RevInfo
                       end
                   end, ?BRANCHED_TREE),

  {0, TreeC} = barrel_revtree:prune(3, <<"">>, TreeC),
  {1, _} = barrel_revtree:prune(2, <<"">>, TreeC),
  ok.

-endif.
