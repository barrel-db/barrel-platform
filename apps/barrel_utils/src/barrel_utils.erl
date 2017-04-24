-module(barrel_utils).

%% API exports
-export([pmap/2, pmap/3, pmap/4]).


%% @doc parallel map implementation
-spec pmap(F, List1) -> List2 when
  F :: fun(),
  List1 :: list(),
  List2 :: list().
pmap(Fun, List) -> pmap(Fun, List, length(List)).

%% @doc parallel map implementation with default timeout to 5000
-spec pmap(F, List1, Workers) -> List2 when
  F :: fun(),
  List1 :: list(),
  Workers :: non_neg_integer(), %% number of workers
  List2 :: list().
pmap(Fun, List, Workers) ->
  pmap(Fun, List, Workers, 5000).


-spec pmap(F, List1, Workers, Timeout) -> List2 when
  F :: fun(),
  List1 :: list(),
  Workers :: non_neg_integer(), %% number of workers
  Timeout :: non_neg_integer(), %% timeout
  List2 :: list().
pmap(Fun, List, NWorkers0, Timeout) ->
  NWorkers1 = erlang:min(length(List), NWorkers0),
  Parent = self(),
  Workers = [
    spawn_monitor(fun() -> pmap_worker(Parent, Fun) end)
    || _ <- lists:seq(1, NWorkers1)
  ],
  {Running, _} = lists:foldr(
    fun(E, {R, [{Pid, _}=W | Rest]}) ->
      Ref = erlang:make_ref(),
      Pid ! {Ref, E},
      {[Ref | R], Rest ++ [W]}
    end,
    {[], Workers},
    List
  ),
  Res = collect(Running, Timeout),
  [erlang:demonitor(MRef, [flush]) || {_Pid, MRef} <- Workers],
  Res.

collect([], _Timeout) -> [];
collect([Ref | Next], Timeout) ->
  receive
    {Ref, Res} ->
      [Res | collect(Next, Timeout)];
    {'DOWN', _MRef, process, _Pid, Reason} ->
      exit(Reason)
  after Timeout ->
    exit(pmap_timeout)
  end.

pmap_worker(Parent, Fun) ->
  receive
    {Ref, E} ->
      Parent ! {Ref, Fun(E)},
      pmap_worker(Parent, Fun)
  end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

pmap_test() ->
  L = [ 1, 2, 3, 4 ],
  Expected = [ 2, 4, 6, 8 ],
  Result = pmap(
    fun(E) -> E * 2 end,
    L,
    4
  ),
  ?assertEqual(Expected, Result).

-endif.