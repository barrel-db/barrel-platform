-module(barrel_indexer).
-author("Benoit Chesneau").

%% API
-export([
  start_link/2,
  init/3
]).

-export([
  system_continue/3,
  system_terminate/4,
  system_code_change/4
]).

-define(DEFAULT_CHANGES_SIZE, 10).

-define(n, 3). %% partial depth


start_link(DbId, Store) ->
  proc_lib:start_link(?MODULE, init, [self(), DbId, Store]).

init(Parent, DbId, Store) ->
  process_flag(trap_exit, true),
  ok = proc_lib:init_ack(Parent, {ok, self()}),
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
    is_integer(LastUpdatedSeq), LastUpdatedSeq > LastSeq ->
      Changes = fetch_changes(State, LastUpdatedSeq),
      process_changes(Changes, State);
    true ->
      Parent ! {wait_changes, self(), LastSeq},
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
      lager:info(
        "Barrel indexer ~p received unexpected message ~p~n",
        [DbId, Msg]
      )
  end.


system_continue(_, _, State) ->
  wait_changes_loop(State).

system_terminate(Reason, _, _, _State) ->
  exit(Reason).

system_code_change(Misc, _, _, _) ->
  {ok, Misc}.



process_changes(Changes, State0 = #{ store := Store, dbid := DbId}) ->
  State2 = lists:foldl(
    fun(Change = #{ seq := Seq }, State) ->
      {ToAdd, ToDel, DocId, FullPaths} = analyze(Change, State),
      
      ForwardOps = merge_forward_paths(ToAdd, ToDel, DocId, State),
      ReverseOps = merge_reverse_paths(ToAdd, ToDel, DocId, State),
      
      lager:info("~n====~n~nindex ~p~n~n", [ToAdd]),
      
      ok = barrel_store:update_index(
        Store, DbId, ForwardOps, ReverseOps, DocId, Seq, FullPaths
      ),
      lager:info("put was ok", []),
      timer:sleep(100),
      loop(State#{ update_seq := Seq })
    end,
    State0,
    Changes
  ),
  loop(State2).

fetch_changes(#{ store := Store, dbid := DbId, changes := OldChanges}, Since) ->
  Max = changes_size() - length(OldChanges),
  Fun = fun
          (_Seq, Change, {N, Acc}) ->
            N2 = N + 1,
            if
              N2 < Max -> {ok, {N2, [Change | Acc]}};
              true -> {stop, {N2, [Change | Acc]}}
            end
        end,
  {_, Changes} = barrel_store:changes_since(
    Store, DbId, Since, Fun, {Since, OldChanges}, [{include_doc, true}]
  ),
  Changes.

merge_forward_paths(ToAdd, ToDel, DocId, St) ->
  Ops0 = merge(
    forward_paths(ToAdd, []), DocId, add, index_get_forward_path, St, []
  ),
  
  merge(
    forward_paths(ToDel, []), DocId, del, index_get_forward_path, St, Ops0
  ).

merge_reverse_paths(ToAdd, ToDel, DocId, St) ->
  Ops0 = merge(
    reverse_paths(ToAdd, []), DocId, add, index_get_forward_path, St, []
  ),
  
  merge(
    reverse_paths(ToDel, []), DocId, del, index_get_forward_path, St, Ops0
  ).


merge([{Path, Sel} | Rest], DocId, Op, Fun, St = #{ store := Store, dbid := DbId}, Acc) ->
  case barrel_store:Fun(Store, DbId, Path) of
    {ok, Map} ->
      case maps:find(Sel, Map) of
        {ok, Entries} ->
          Entries2 = case Op of
                       add -> lists:usort([DocId | Entries]) ;
                       del -> Entries -- [DocId]
                     end,
          Sz = length(Entries2),
          Acc2 = if
                   Sz > 0 ->
                     Map2 = Map#{ Sel => Entries2 },
                     [{put, Path, Map2} | Acc];
                   true ->
                     Map2 = maps:remove(Sel, Map),
                     case maps:size(Map2) of
                       0 ->  [{delete, Path} | Acc];
                       _ ->  [{put, Path, Map2} | Acc]
                     end
                 end,
          merge(Rest, DocId, Op, Fun, St, Acc2);
        error ->
          Acc2 = case Op of
                   add ->
                     [{put, Path, #{ Sel => [DocId]}} | Acc];
                   del ->
                     Acc
                 end,
          merge(Rest, DocId, Op, Fun, St, Acc2)
      end;
    not_found ->
      Acc2 = case Op of
               add ->
                 [{put, Path, #{ Sel => [DocId]}} | Acc];
               del ->
                 Acc
             end,
      merge(Rest, DocId, Op, Fun, St, Acc2)
  end;
merge([], _DocId, _Op, _Fun, _St, Acc) ->
  Acc.

analyze(Change, #{ store := Store, dbid := DbId}) ->
  Doc = maps:get(doc, Change),
  Del = maps:get(deleted, Change, false),
  DocId = barrel_doc:id(Doc),
  OldPaths = case barrel_store:index_get_last_doc(Store, DbId, DocId) of
               {ok, OldPaths1} -> OldPaths1;
               not_found -> []
             end,
  
  case Del of
    true ->
      {[], OldPaths, DocId, []};
    false ->
      Paths = flatten(Doc),
      Removed = OldPaths -- Paths,
      Added = Paths -- OldPaths,
      {Added, Removed, DocId, Paths}
  end.

%% TODO: should we encode the pos ?
reverse_paths([{PathParts, Pos, Sel} | Rest], Acc) ->
  Path = << (barrel_lib:binary_join(PathParts, <<"/">>))/binary, Pos:8 >>,
  reverse_paths(Rest, [{Path, Sel} | Acc]);
reverse_paths([], Acc) ->
  lists:usort(Acc).

forward_paths([{PathParts, Pos, Sel} | Rest], Acc) ->
  Path = << (barrel_lib:binary_join(lists:reverse(PathParts), <<"/">>))/binary, Pos:8 >>,
  forward_paths(Rest, [{Path, Sel} | Acc]);
forward_paths([], Acc) ->
  lists:usort(Acc).

%% TODO: this part can be optimized in rust or C if needed
flatten(Obj) ->
  Flat = maps:fold(
    fun
      (<<"_", _/binary >>, _, Acc) -> Acc;
      (Key, Val, Acc) when is_map(Val) ->
        json_obj(Val, [Key, <<"$">>], 0, [], Acc);
      (Key, Val, Acc) when is_list(Val) ->
        json_array(Val, [Key, <<"$">>], 0, [], Acc);
      (Key, Val, Acc) ->
        [{[Val, Key, <<"$">>], 0, []} | Acc]
    end,
    [],
    Obj
  ),
  Flat.

json_obj(Obj, Root, Pos, Levels, Acc0) ->
  maps:fold(
    fun
      (Key, Val, Acc) when is_map(Val) ->
        case maybe_split([Key | Root]) of
          {true, NRoot, Path} ->
            json_obj(Val, NRoot, Pos + 1, Levels, [{Path, Pos, Levels} | Acc]);
          {false, NRoot, _} ->
            json_obj(Val, NRoot, Pos, Levels, Acc)
        end;
      (Key, Val, Acc) when is_list(Val) ->
        case maybe_split([Key | Root]) of
          {true, NRoot, Path} ->
            json_array(Val, NRoot, Pos + 1, Levels, [{Path, Pos, Levels} | Acc]);
          {false, NRoot, _} ->
            json_array(Val, NRoot, Pos, Levels, Acc)
        end;
      (Key, Val, Acc) ->
        [ {[Val, Key | lists:droplast(Root)], Pos + 1, Levels},
          {[Key | Root], Pos, Levels} | Acc
        ]
    end,
    Acc0,
    Obj
  ).

json_array(Arr, Root, Pos, Levels, Acc) -> json_array_1(Arr, Root, 0, Pos, Levels, Acc).

json_array_1([Item | Rest], Root, Idx, Pos, Levels, Acc) when is_map(Item) ->
  Acc1 = case maybe_split([Idx | Root]) of
           {true, NRoot, Path} ->
             json_obj(Item, NRoot, Pos + 1, [Idx | Levels], [{Path, Pos, Levels} | Acc]);
           {false, NRoot, _} ->
             json_obj(Item, NRoot, Pos, [Idx | Levels], Acc)
         end,
  json_array_1(Rest, Root, Idx + 1, Pos, Levels, Acc1);
json_array_1([Item | Rest], Root, Idx, Pos, Levels, Acc) when is_list(Item) ->
  Acc1 = case maybe_split([Idx | Root]) of
           {true, NRoot, Path} ->
             json_array(Item, NRoot, Pos + 1, [Idx | Levels], [{Path, Pos, Levels} | Acc]);
           {false, NRoot, _} ->
             json_array(Item, NRoot, Pos, [Idx | Levels], Acc)
         end,
  json_array_1(Rest, Root, Idx + 1, Pos, Levels, Acc1);
json_array_1([Item | Rest], Root, Idx, Pos, Levels, Acc) ->
  Acc1 = [
    {[Item, Idx | lists:droplast(Root)], Pos + 1, [Idx | Levels] },
    {[Idx | Root], Pos , Levels} | Acc
  ],
  json_array_1(Rest, Root, Idx + 1, Pos, Levels, Acc1);
json_array_1([], _Root, _Idx, _Pos, _Levels, Acc) ->
  Acc.

maybe_split(Parts) ->
  Len = length(Parts),
  if
    Len =:= 3 ->
      NParts = lists:droplast(Parts),
      {true, NParts, Parts};
    true ->
      {false, Parts, nil}
  end.

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
    {[1, <<"a">>, <<"$">>], 0, []},
    {[<<"2">>, <<"b">>, <<"$">>], 0, []},
    {[<<"a">>, <<"c">>, <<"$">>], 0, []},
    {[1, <<"a">>, <<"c">>], 1, []},
    {[<<"b">>, <<"c">>, <<"$">>], 0, []},
    {[0, <<"b">>, <<"c">>], 1, []},
    {[<<"a">>, 0, <<"b">>], 2, [0]},
    {[1, <<"b">>, <<"c">>], 1, []},
    {[<<"b">>, 1, <<"b">>], 2, [1]},
    {[2, <<"b">>, <<"c">>], 1, []},
    {[<<"c">>, 2, <<"b">>], 2, [2]},
    {[<<"c">>, <<"c">>, <<"$">>], 0, []},
    {[<<"a">>, <<"c">>, <<"c">>], 1, []},
    {[1, <<"a">>, <<"c">>], 2, []},
    {[2, <<"d">>, <<"$">>], 0, []},
    {[<<"c">>, 2, <<"d">>], 1, [2]},
    {[ 1, <<"d">>, <<"$">>], 0, []},
    {[<<"b">>, 1, <<"d">>], 1, [1]},
    {[0, <<"d">>, <<"$">>], 0, []},
    {[<<"a">>, 0, <<"d">>], 1, [0]},
    {[0, <<"e">>, <<"$">>], 0, []},
    {[<<"a">>, 0, <<"e">>], 1, [0]},
    {[1, <<"a">>, 0], 2, [0]},
    {[1, <<"e">>, <<"$">>], 0, []},
    {[<<"b">>, 1, <<"e">>], 1, [1]},
    {[2, <<"b">>, 1], 2, [1]}
    
  ],
  ?assertEqual(lists:sort(Expected), lists:sort(flatten(Doc))).

-endif.
