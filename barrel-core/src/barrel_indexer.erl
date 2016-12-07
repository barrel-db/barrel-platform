-module(barrel_indexer).
-author("Benoit Chesneau").

%% API
-export([
  start_link/3,
  init/3
]).

-define(DEFAULT_CHANGES_SIZE, 10).

-define(n, 3). %% partial depth


start_link(Parent, DbId, Store) ->
  proc_lib:spawn_link(?MODULE, init, [Parent, DbId, Store]).

init(Parent, DbId, Store) ->
  process_flag(trap_exit, true),
  UpdateSeq = barrel_store:index_seq(Store, DbId),
  
  State = #{
    parent => Parent,
    dbid => DbId,
    store => Store,
    update_seq => UpdateSeq,
    changes => [],
    pids => []
  },
  
  loop(State).


loop(State) ->
  #{ parent := Parent, dbid := DbId, update_seq := LastSeq } = State,
  LastUpdatedSeq  = barrel_transactor:last_update_seq(DbId),
  
  if
    LastUpdatedSeq > LastSeq ->
      Changes = fetch_changes(State, LastUpdatedSeq),
      process_changes(Changes, State);
    true ->
      Parent ! {wait_change, self(), LastSeq},
      wait_changes_loop(State)
  end.

wait_changes_loop(State = #{ parent := Parent, dbid := DbId }) ->
  receive
    {updated, Since} ->
      Changes = fetch_changes(State, Since),
      process_changes(Changes, State);
    {'EXIT', Parent, Reason} ->
      exit(Reason);
    {system, From, Request} ->
      sys:handle_system_msg(Request, From, Parent, ?MODULE, [], State);
    Msg ->
      error_logger:error_msg(
        "Barrel indexer ~p received unexpected message ~p~n",
        [DbId, Msg]
      )
  end.

process_changes(Changes, State0 = #{ store := Store, dbid := DbId}) ->
  State2 = lists:foldl(
    fun(Change = #{ seq := Seq }, State) ->
      {ToAdd, ToDel, DocId, FullPaths} = analyze(Change, State),
      
      ForwardOps = merge_forward_paths(ToAdd, ToDel, DocId, State),
      ReverseOps = merge_reverse_paths(ToAdd, ToDel, DocId, State),
      
      ok = barrel_store:update_index(
        Store, DbId, ForwardOps, ReverseOps, DocId, Seq, FullPaths
      ),
      
      State#{ update_seq := Seq }
    end,
    State0,
    Changes
  ),
  loop(State2).

fetch_changes(#{ store := Store, dbid := DbId, changes := OldChanges}, Since) ->
  Max = changes_size() - length(OldChanges),
  Fun = fun
          (_Seq, Change, {_Last, N, Acc}) ->
            N2 = N + 1,
            if
              N2 < Max -> {ok, {N2, [Change | Acc]}};
              true -> {stop, {N2, [Change | Acc]}}
            end
        end,
  {_, Changes} = barrel_store:changes_since(
    Store, DbId, Since, Fun, {Since, 0, OldChanges}, [{include_docs, true}]
  ),
  Changes.

merge_forward_paths(ToAdd, ToDel, DocId, St) ->
  Ops0 = merge(
    partial_forward_paths(ToAdd, []), DocId, add, index_get_forward_path, St, []
  ),
  
  merge(
    partial_forward_paths(ToDel, []), DocId, del, index_get_forward_path, St, Ops0
  ).

merge_reverse_paths(ToAdd, ToDel, DocId, St) ->
  Ops0 = merge(
    partial_reverse_paths(ToAdd, []), DocId, add, index_get_forward_path, St, []
  ),
  
  merge(
    partial_reverse_paths(ToDel, []), DocId, del, index_get_forward_path, St, Ops0
  ).


merge([{Path, Sel} | Rest], DocId, Op, Fun, St = #{ store := Store, dbid := DbId}, Acc) ->
  case barrel_store:Fun(Store, DbId, Path) of
    {ok, Entries} ->
      Entry = maps:get(Sel, Entries, []),
      Entries2 = case Op of
                   add ->
                     Entries#{ Sel => lists:usort([DocId | Entry]) };
                   del ->
                     case Entry -- [DocId] of
                       [] ->
                         maps:remove(Sel, Entries);
                       Entry2 ->
                         Entries#{ Sel => Entry2 }
                     end
                 end,
      Sz = maps:size(Entries2),
      Acc2 = if
               Sz > 0 -> [{put, Path, Entries2} | Acc];
               true -> [{delete, Path}]
             end,
      merge(Rest, DocId, Op, Fun, St, Acc2);
    not_found ->
      Acc2 = case Op of
               add ->
                 [{put, Path, #{ Sel => [DocId]}} | Acc];
               del ->
                 Acc
    
             end,
      merge(Rest, DocId, Op, Fun, St, Acc2)
  end.

analyze(Change, #{ store := Store, dbid := DbId}) ->
  #{ deleted := Del, doc := Doc} = Change,
  DocId = barrel_doc:id(Doc),
  OldPaths = barrel_store:index_get_last_doc(Store, DbId, DocId),
  
  case Del of
    true ->
      {[], OldPaths, DocId, []};
    false ->
      Paths = flatten(Doc),
      Removed = OldPaths -- Paths,
      Added = Paths -- OldPaths,
      {Added, Removed, DocId, Paths}
  end.

partial_reverse_paths([PathParts | Rest], Acc) ->
  PartialPathParts = partial(PathParts),
  Paths = [{barrel_lib:join(Part, <<"/">>), I} || {I, Part} <- PartialPathParts],
  partial_reverse_paths(Rest, [Paths | Acc]);
partial_reverse_paths([], Acc) ->
  lists:usort(lists:flatten(Acc)).

partial_forward_paths([PathParts | Rest], Acc) ->
  PartialPathParts = partial(lists:reverse(PathParts)),
  Paths = [{barrel_lib:join(Part, <<"/">>), I}  || {I, Part}  <- PartialPathParts],
  partial_forward_paths(Rest, [Paths | Acc]);
partial_forward_paths([], Acc) ->
  lists:usort(lists:flatten(Acc)).

%% TODO: this part can be optimized in rust or C if needed
flatten(Obj) ->
  Flat = maps:fold(
    fun
      (<<"_", _/binary >>, _, Acc) -> Acc;
      (Key, Val, Acc) when is_map(Val) ->
        json_obj(Val, [Key, <<"$">>], Acc);
      (Key, Val, Acc) when is_list(Val) ->
        json_array(Val,  [Key, <<"$">>], Acc);
      (Key, Val, Acc) ->
        [[Val, Key, <<"$">>] | Acc]
    end,
    [],
    Obj
  ),
  Flat.

json_obj(Obj, Root, Acc0) ->
  maps:fold(
    fun
      (Key, Val, Acc) when is_map(Val) ->
        json_obj(Val, [Key | Root], Acc);
      (Key, Val, Acc) when is_list(Val) ->
        json_array(Val, [Key | Root], Acc);
      (Key, Val, Acc) ->
        [[Val, Key | Root] | Acc]
    end,
    Acc0,
    Obj
  ).

json_array(Arr, Root, Acc) -> json_array_1(Arr, Root, 0, Acc).

json_array_1([Item | Rest], Root, Idx, Acc) when is_map(Item) ->
  Acc1 = json_obj(Item, [Idx | Root], Acc),
  json_array_1(Rest, Root, Idx + 1, Acc1);
json_array_1([Item | Rest], Root, Idx, Acc) when is_list(Item) ->
  Acc1 = json_array(Item, [Idx | Root], Acc),
  json_array_1(Rest, Root, Idx + 1, Acc1);
json_array_1([Item | Rest], Root, Idx, Acc) ->
  json_array_1(Rest, Root, Idx + 1, [[Item, Idx | Root] | Acc]);
json_array_1([], _Root, _Idx, Acc) ->
  Acc.

partial(L) -> partial(L, 0, []).

partial(L=[_|Rest], Idx, Acc) when length(L) > ?n ->
  partial(Rest, Idx + 1, [{Idx, lists:sublist(L, 1, ?n)} | Acc]);
partial(L, Idx, Acc) ->
  lists:reverse([{Idx, L} | Acc]).

changes_size() ->
  application:get_env(barrel, index_changes_size, ?DEFAULT_CHANGES_SIZE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

flatten_test() ->
  Doc =
    #{
      <<"a">> => 1,
      <<"b">> => <<"2">>,
      <<"c">> => #{
        <<"a">> => 1,
        <<"b">> => [<<"a">>, <<"b">>, <<"c">>],
        <<"c">> => #{ <<"a">> => 1}
      },
      <<"d">> => [<<"a">>, <<"b">>, <<"c">>],
      <<"e">> => [#{<<"a">> => 1}, #{ <<"b">> => 2}]
    },
  
  Expected = [
    [1, <<"a">>, <<"$">>],
    [<<"2">>, <<"b">>, <<"$">>],
    [1, <<"a">>, <<"c">>, <<"$">>],
    [<<"c">>, 2, <<"b">>, <<"c">>, <<"$">>],
    [<<"b">>, 1, <<"b">>, <<"c">>, <<"$">>],
    [<<"a">>, 0, <<"b">>, <<"c">>, <<"$">>],
    [1, <<"a">>, <<"c">>, <<"c">>, <<"$">>],
    [<<"c">>, 2, <<"d">>, <<"$">>],
    [<<"b">>, 1, <<"d">>, <<"$">>],
    [<<"a">>, 0, <<"d">>, <<"$">>],
    [1, <<"a">>, 0, <<"e">>, <<"$">>],
    [2, <<"b">>, 1, <<"e">>, <<"$">>]
  ],
  ?assertEqual(lists:sort(Expected), lists:sort(flatten(Doc))).

partial_test() ->
  L0 = [a, b, c],
  ?assertEqual([{0, [a, b, c]}], partial(L0)),
  L1 = [a, b, c, d, e, f],
  ?assertEqual([{0, [a, b, c]}, {1, [b, c, d]}, {2, [c, d, e]}, {3, [d, e, f]}], partial(L1)),
  L2 = [a, b, c, d],
  ?assertEqual([{1, [a, b, c]}, {2, [b, c, d]}], partial(L2)),
  L3 = [a, b, c, d, e],
  ?assertEqual([{1, [a, b, c]}, {2, [b, c, d]}, {3, [c, d, e]}], partial(L3)),
  L4 = [a, d, e, f],
  ?assertEqual([{1, [a, d, e]}, {2, [d, e, f]}], partial(L4)).

-endif.
