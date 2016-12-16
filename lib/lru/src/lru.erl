%%% -*- erlang -*-
%%%
%%% This file is part of erlang-lru released under the BSD license.
%%%
%%% Copyright (c) 2015 Benoît Chesneau <benoitc@e-engura.org>
%%%
-module('lru').
-behaviour(gen_server).

-export([start/1, start/2,
         start_link/1, start_link/2,
         stop/1,
         get/2, get/3,
         contains/2,
         keys/1,
         peek/2, peek/3,
         add/3,
         add_with/3,
         contains_or_add/3,
         contains_or_add_with/3,
         remove/2,
         remove_oldest/1,
         purge/1,
         size/1,
         count/1,
         info/1,
         set_size/2,
         resize/2,
         set_count/2,
         reset_count/2]).

%% API exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


-record(cache, {max_size = 0,
                max_objs = 0,
                size = 0,
                evict_list,
                items,
                evict_fun}).


-type lru_option() :: {evict_fun, fun()}
                      | {spawn_opt, list()}
                      | {max_size, non_neg_integer()}
                      | {max_objs, non_neg_integer()}.

-type name() :: {local, Name::atom()}
                | {global, GlobalName::term()}
                | {via, ViaName::term()}.


-type cache() :: cache() | name().

-type callback() :: fun()
                    | {fun(), list()}
                    | {module(), fun(), list()}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc creates an LRU of the given size
%% Options are:
%%  - `{evict_fun, Fun}' a function that will received the evicted key value
%%  "fun(Key, Value)".
%%  - `{spawn_opts, Opts}' the spawn options. see `erlang:spawn_opt/2' for
%%  more informations.
%%  - `{max_objs, Count}' the maximum number of items in the cache, default
%%  is 0 for an unlimited number
%%  - `{max_size, Size}`' the maxium size (in bytes) of items in the cache,
%%  default is 0 for an unlimited size
-spec start([lru_option()]) -> {ok, cache()} | {error, term()}.
start(Opts) ->
  SpawnOpts = proplists:get_value(spawn_opts, Opts, []),
  gen_server:start(?MODULE, [Opts], [{spawn_opts, SpawnOpts}]).

%% @doc creates an LRU of the given size with a registered name
-spec start(name(), [lru_option()]) -> {ok, cache()} | {error, term()}.
start(Name, Opts) ->
  SpawnOpts = proplists:get_value(spawn_opts, Opts, []),
  gen_server:start(Name, ?MODULE, [Opts], [{spawn_opts, SpawnOpts}]).

%% @doc creates an LRU of the given size as part of a supervision tree
-spec start_link([lru_option()]) -> {ok, cache()} | {error, term()}.
start_link(Opts) ->
  SpawnOpts = proplists:get_value(spawn_opts, Opts, []),
  gen_server:start_link(?MODULE, [Opts], [{spawn_opts, SpawnOpts}]).

%% @doc creates an LRU of the given size as part of a supervision tree with a
%% registered name.
-spec start_link(name(), [lru_option()]) -> {ok, cache()} | {error, term()}.
start_link(Name, Opts) ->
  SpawnOpts = proplists:get_value(spawn_opts, Opts, []),
  gen_server:start_link(Name, ?MODULE, [Opts],[{spawn_opts, SpawnOpts}]).

%% @doc stop the LRU cache
-spec stop(cache() | name()) -> ok.
stop(Cache) ->
  try
    gen_server:call(Cache, stop, infinity)
  catch
    exit:{noproc,_} -> ok;
    exit:noproc -> ok;
    %% Handle the case where the monitor triggers
    exit:{normal, _} -> ok
  end.

%% @doc adds a value to the cache.  Returns true if an eviction occured.
-spec add(cache(), term(), term() | fun()) -> true | false.
add(Cache, Key, Value) ->
  Reply = call(Cache, {add, Key, Value}),
  case Reply of
    {error, Error} -> erlang:error(Error);
    _ -> Reply
  end.

%% @doc like add but with a callback
-spec add_with(cache(), term(), callback()) -> true | false.
add_with(Cache, Key, Callback) ->
  Reply = call(Cache, {add_with, Key, Callback}),
  case Reply of
    {error, Error} -> erlang:error(Error);
    _ -> Reply
  end.

%% @doc lookup a key's value from the cache. Return undefined if it's not
%% found.
-spec get(cache(), term()) -> term() | undefined.
get(Cache, Key) ->
  lru:get(Cache, Key, undefined).


%% @doc lookup a key's value from the cache. Return the Default value if it's
%% not found.
-spec get(cache(), term(), term()) -> term().
get(Cache, Key, Default) ->
  call(Cache, {get, Key, Default}).

%% @doc Returns the key value (or undefined if not found) without updating the
%% "recently used"-ness of the key.
-spec peek(cache(), term()) -> term() | undefined.
peek(Cache, Key) ->
  peek(Cache, Key, undefined).

%% @doc Returns the key value (or undefined if not found) without updating the
%% "recently used"-ness of the key.
-spec peek(cache(), term(), term()) -> term() | undefined.
peek(Cache, Key, Default) ->
  call(Cache, {peek, Key, Default}).

%% @doc check if the key is in the cache
-spec contains(cache(), term() | fun()) -> true | false.
contains(Cache, Key) ->
  call(Cache, {contains, Key}).

%% @doc return all the keys from the cache
-spec keys(cache()) -> [term()].
keys(Cache) ->
  call(Cache, keys).

%% @doc  checks if a key is in the cache (without updating the recent-ness or
%% deleting it for being stale), if not, adds the value. Returns whether found and whether an eviction
%% occurred.
-spec contains_or_add(cache(), term(), term()) ->
  {Exists::boolean(), Evict::boolean()}.
contains_or_add(Cache, Key, Value) ->
  Reply = call(Cache, {contains_or_add, Key, Value}),
  case Reply of
    {error, Error} -> erlang:error(Error);
    _ -> Reply
  end.


%% @doc like contains_or_add but with a callback
-spec contains_or_add_with(cache(), term(), callback()) ->
  {Exists::boolean(), Evict::boolean()}.
contains_or_add_with(Cache, Key, Callback) ->
  Reply = call(Cache, {contains_or_add_with, Key, Callback}),
  case Reply of
    {error, Error} -> erlang:error(Error);
    _ -> Reply
  end.


%% @doc remove a key from the cache
-spec remove(cache(), Key::term()) -> ok.
remove(Cache, Key) ->
  call(Cache, {remove, Key}).

%% @doc remove the oldest item from the cache
-spec remove_oldest(cache()) -> ok.
remove_oldest(Cache) ->
  call(Cache, remove_oldest).

%% @doc get the size of items in the cache
-spec size(cache()) -> non_neg_integer().
size(Cache) ->
  call(Cache, size).

%% @doc get the number of items in the cache
-spec count(cache()) -> non_neg_integer().
count(Cache) ->
  call(Cache, count).

%% @doc purge all items from the cache.
-spec purge(cache()) -> ok.
purge(Cache) ->
  call(Cache, purge).

%% @doc get cache info
-spec info(cache()) -> list().
info(Cache) ->
  call(Cache, info).

%% @doc change the size of the cache
-spec set_size(cache(), non_neg_integer()) -> ok.
set_size(Cache, Size) ->
  gen_server:cast(Cache, {set_size, Size}).

%% @doc resize the cache
-spec resize(cache(), non_neg_integer()) -> ok.
resize(Cache, Size) ->
  gen_server:cast(Cache, {resize, Size}).

%% @doc set the number of items in the cache
-spec set_count(cache(), non_neg_integer()) -> ok.
set_count(Cache, Count) ->
  gen_server:cast(Cache, {set_count, Count}).

%% @doc reset the number of items in the cache
reset_count(Cache, Count) ->
  gen_server:cast(Cache, {reset_count, Count}).


%%====================================================================
%% Internal functions
%%====================================================================
%%

%% @private
init([Opts]) ->
  EvictFun = proplists:get_value(evict_fun, Opts),
  MaxSize = proplists:get_value(max_size, Opts, 0),
  MaxObjs = proplists:get_value(max_objs, Opts, 0),

  {ok, #cache{max_size = MaxSize,
              max_objs = MaxObjs,
              evict_list = [],
              items = #{},
              evict_fun = EvictFun}}.


%% @private
handle_call({add, Key, Value}, From, Cache) ->
  case value_1(Value) of
    {ok, Value2} ->
      do_add(Key, Value2, From, Cache);
    Error ->
      {reply, Error, Cache}
  end;

handle_call({add_with, Key, Callback}, From, Cache) ->
  case value(Callback) of
    {ok, Value2} ->
      do_add(Key, Value2, From, Cache);
    Error ->
      {reply, Error, Cache}
  end;

handle_call({get, Key, Default}, _From, Cache) ->
  case maps:find(Key, Cache#cache.items) of
    {ok, Value} ->
      Evicted2 =  move_front(Cache#cache.evict_list, Key),
      {reply, Value, Cache#cache{evict_list=Evicted2}};
    error ->
      {reply, Default, Cache}
  end;

handle_call({peek, Key, Default}, _From, Cache) ->
  {reply, maps:get(Key, Cache#cache.items, Default), Cache};

handle_call({contains, Key}, _From, Cache) ->
  {reply, maps:is_key(Key, Cache#cache.items), Cache};

handle_call({contains_or_add, Key, Value}, From, Cache) ->
  case value_1(Value) of
    {ok, Value2} ->
      do_contains_or_add(Key, Value2, From, Cache);
    Error ->
      {reply, Error, Cache}
  end;

handle_call({contains_or_add_with, Key, Callback}, From, Cache) ->
  case value(Callback) of
    {ok, Value2} ->
      do_contains_or_add(Key, Value2, From, Cache);
    Error ->
      {reply, Error, Cache}
  end;

handle_call(keys, _From, Cache) ->
  {reply, lists:reverse(Cache#cache.evict_list), Cache};

handle_call({remove, Key}, From, Cache) ->
  gen_server:reply(From, ok),
  {noreply, remove_element(Key, Cache)};

handle_call(remove_oldest, From, Cache) ->
  gen_server:reply(From, ok),
  {noreply, remove_oldest1(Cache)};

handle_call(count, _From, Cache) ->
  Sz = length(Cache#cache.evict_list),
  {reply, Sz, Cache};

handle_call(size, _From, Cache) ->
  Sz = erlang:external_size(Cache#cache.items),
  {reply, Sz, Cache};


handle_call(purge, _From, Cache) ->
  {reply, ok, Cache#cache{items=#{}, evict_list=[]}};

handle_call(info, _From, Cache) ->
  #cache{max_size=MaxSize,
         max_objs=MaxObjs,
         items=Items,
         evict_list=EvictList} = Cache,
  Info = [{max_size, MaxSize},
          {size, erlang:external_size(Items)},
          {max_objs, MaxObjs},
          {len, length(EvictList)}],
  {reply, Info, Cache};

handle_call(stop, _From, Cache) ->
  {stop, normal, ok, Cache}.

%% @private
handle_cast({reset_count, Count}, Cache) ->
  if
    Count < Cache#cache.max_objs ->
      Diff = Cache#cache.max_objs - Count,
      Cache2 = lists:foldl(fun(_, Cache1) ->
                               remove_oldest1(Cache1)
                           end, Cache, lists:seq(1, Diff)),
      {noreply, Cache2#cache{max_objs=Count}};
    true ->
      {noreply, Cache#cache{max_objs=Count}}
  end;

handle_cast({set_count, Count}, Cache) ->
  {noreply, Cache#cache{max_objs=Count}};

handle_cast({resize, Sz}, Cache) ->
  if
    Sz < Cache#cache.max_size ->
      Cache2 = remove_until_max(Cache#cache{max_size=Sz}),
      {noreply, Cache2};
    true ->
      {noreply, Cache#cache{max_size=Sz}}
  end;

handle_cast({set_size, Sz}, Cache) ->
  {noreply, Cache#cache{max_size=Sz}};

handle_cast(_Msg, Cache) ->
  {noreply, Cache}.

%% @private
handle_info(_Info, Cache) ->
  {noreply, Cache}.

%% @private
code_change(_OldVsn, Cache, _Extra) ->
  {ok, Cache}.

%% @private
terminate(_Reason, _Cache) ->
  ok.

do_add(Key, Value, From, Cache) ->
  #cache{items=Items,
         evict_list=Evicted} = Cache,

  case maps:find(Key, Items) of
    error ->
      %% add new item
      Items2 = maps:put(Key, Value, Items),
      Evicted2 = push_front(Evicted, Key),
      Cache1 = Cache#cache{items=Items2, evict_list=Evicted2},
      %% check if the size is not exceeded
      case has_exceeded(Cache1) of
        true ->
          gen_server:reply(From, true),
          {noreply, remove_oldest1(Cache1)};
        false ->
          {reply, false, Cache1}
      end;
    {ok, _OldValue} ->
      gen_server:reply(From, false),
      %% add new value
      Items2 = maps:put(Key, Value, Items),
      %% move old entry to front
      Evicted2 = move_front(Evicted, Key),
      {noreply, Cache#cache{items=Items2, evict_list=Evicted2}}
  end.

do_contains_or_add(Key, Value, From, Cache) ->
  case maps:is_key(Key, Cache#cache.items) of
    false ->
      #cache{items=Items,
             evict_list=Evicted} = Cache,
      Items2 = maps:put(Key, Value, Items),
      Evicted2 = push_front(Evicted, Key),
      Cache1 = Cache#cache{items=Items2, evict_list=Evicted2},
      case has_exceeded(Cache1) of
        true ->
          gen_server:reply(From, {false, true}),
          {noreply, remove_oldest1(Cache1)};
        false ->
          {reply, {false, false}, Cache1}
      end;
    true ->
      {reply, {true, false}, Cache}
  end.

value_1(Fun) when is_function(Fun)  ->
  case catch Fun() of
    {'EXIT', Error} -> {error, Error};
    {ok, Val} -> {ok, Val};
    Val -> {ok, Val}
  end;
value_1(Val) ->
  {ok, Val}.

value({M, F, A}) when is_function(F) ->
  case catch apply(M, F, A) of
    {'EXIT', Error} -> {error, Error};
    {ok, Val} -> {ok, Val};
    Val -> {ok, Val}
  end;
value({F, A}) when is_function(F) ->
  case catch apply(F, A) of
    {'EXIT', Error} -> {error, Error};
    {ok, Val} -> {ok, Val};
    Val -> {ok, Val}
  end;
value(F) when is_function(F) ->
  case catch F() of
    {'EXIT', Error} -> {error, Error};
    {ok, Val} -> {ok, Val};
    Val -> {ok, Val}
  end;
value(Val) ->
  {ok, Val}.

call(Cache, Msg) ->
  gen_server:call(Cache, Msg).

has_exceeded(Cache) ->
  #cache{max_size = MaxSize,
         max_objs = MaxObjs,
         items = Items,
         evict_list = Evicted} = Cache,

  Sz = erlang:external_size(Items),

  if
    Sz > MaxSize, MaxSize =/= 0 -> true;
    length(Evicted) > MaxObjs, MaxObjs =/= 0 -> true;
    true -> false
  end.

push_front(List, Key) ->
  [Key | List].


move_front(List, Key) ->
  [Key | lists:delete(Key, List)].

remove_until_max(Cache) ->
  Cache2 = remove_oldest1(Cache),
  Sz = erlang:external_size(Cache2#cache.items),
  if
    Sz =< Cache2#cache.max_size -> Cache2;
    true -> remove_until_max(Cache)
  end.

remove_oldest1(Cache) ->
  Last = lists:last(Cache#cache.evict_list),
  Cache1 = remove_item(Last, Cache),
  Cache1#cache{evict_list=lists:droplast(Cache#cache.evict_list)}.

remove_element(Key, Cache) ->
  Cache1 = remove_item(Key, Cache),
  Cache1#cache{evict_list=lists:delete(Key, Cache#cache.evict_list)}.

remove_item(Key, #cache{items=Items, evict_fun=EvictFun}=Cache) ->
  case EvictFun of
    undefined ->
      Cache#cache{items=maps:remove(Key, Items)};
    _ ->
      case maps:find(Key, Items) of
        {ok, Value} ->
          EvictFun(Key, Value);
        error ->
          ok
      end,
      Cache#cache{items=maps:remove(Key, Items)}
  end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

lru_test() ->

  _ = ets:new(lru_test, [named_table, ordered_set, public]),
  ets:insert_new(lru_test, {evict_count, 0}),
  EvictFun = fun(Key, Value) ->
                 if
                   Key /= Value ->
                     throw({not_equal, Key, Value});
                   true ->
                     ok
                 end,
                 _ = ets:update_counter(lru_test, evict_count, 1)
             end,
  {ok, Cache} = lru:start_link([{max_objs, 128}, {evict_fun, EvictFun}]),
  ?assert(is_pid(Cache)),
  [lru:add(Cache, I, I) || I <- lists:seq(1, 256)],
  ?assertEqual(lru:count(Cache), 128),
  ?assert(ets:lookup(lru_test, evict_count) =:= [{evict_count, 128}]),

  lists:foldl(fun(Key, I) ->
                  Value = lru:get(Cache, Key),
                  ?assert(Value =:= Key),
                  ?assert(Value =/= (I+ 128)),
                  I+1
              end, 0, lru:keys(Cache)),

  lists:foreach(fun(I) ->
                    ?assert(lru:get(Cache, I) =:= undefined)
                end, lists:seq(1, 128)),
  lists:foreach(fun(I) ->
                    ?assert(lru:get(Cache, I) =:= I)
                end, lists:seq(129, 256)),



  lists:foreach(fun(I) ->
                    lru:remove(Cache, I),
                    ?assert(lru:get(Cache, I) =:= undefined)
                end, lists:seq(129, 192)),


  ?assert(lru:get(Cache, 193) =:= 193),

  lists:foreach(fun(Key) ->
                    ?assert(lru:get(Cache, Key) > 192 )
                end, lru:keys(Cache)),

  lru:purge(Cache),
  ?assert(lru:count(Cache) =:= 0),

  lists:foreach(fun(I) ->
                    ?assert(lru:get(Cache, I) =:= undefined)
                end, lists:seq(1, 200)),

  ets:delete(lru_test),
    lru:stop(Cache),
    ok.

lru_add_test() ->
  _ = ets:new(lru_test, [named_table, ordered_set, public]),
  ets:insert_new(lru_test, [{evict_count, 0}]),

  EvictFun = fun(_Key, _Value) ->
                 _ = ets:update_counter(lru_test, evict_count, 1)
             end,

  {ok, Cache} = lru:start_link([{max_objs, 1}, {evict_fun, EvictFun}]),

  ?assert(lru:add(Cache, 1, 1) =:= false),
  ?assert(ets:lookup(lru_test, evict_count) =:= [{evict_count, 0}]),

  ?assert(lru:add(Cache, 2, 2) =:= true),
  ?assert(ets:lookup(lru_test, evict_count) =:= [{evict_count, 1}]),

  ets:delete(lru_test),
  lru:stop(Cache),
  ok.

lru_add_fun_test() ->
  {ok, Cache} = lru:start_link([{max_objs, 2}]),
  lru:add(Cache, 1, fun() -> 1 end),
  ?assert(lru:get(Cache, 1) =:= 1),
  lru:add(Cache, 2, fun() -> {ok, 2} end),
  lru:add(Cache, 2, fun() -> 2 end),
  lru:stop(Cache),
  ok.

lru_add_with_test() ->
  {ok, Cache} = lru:start_link([{max_objs, 2}]),
  lru:add_with(Cache, 1, fun() -> 1 end),
  ?assert(lru:get(Cache, 1) =:= 1),
  lru:add_with(Cache, 2, fun() -> {ok, 2} end),
  lru:add_with(Cache, 2, fun() -> 2 end),
  lru:stop(Cache),
  ok.

lru_contains_test() ->
  {ok, Cache} = lru:start_link([{max_objs, 2}]),
  lru:add(Cache, 1, 1),
  lru:add(Cache, 2, 2),
  ?assert(lru:contains(Cache, 1)),
  lru:add(Cache, 3, 3),
  ?assert(lru:contains(Cache, 1) =:= false),
  lru:stop(Cache),
  ok.

lru_contains_or_add_test() ->
  {ok, Cache} = lru:start_link([{max_objs, 2}]),
  lru:add(Cache, 1, 1),
  lru:add(Cache, 2, 2),
  ?assertEqual(lru:contains_or_add(Cache, 1, 1), {true, false}),
  lru:add(Cache, 3, 3),
  ?assertEqual(lru:contains_or_add(Cache, 1, 1), {false, true}),
  ?assert(lru:contains(Cache, 1)),
  lru:stop(Cache),
  ok.

lru_contains_or_add_fun_test() ->
  {ok, Cache} = lru:start_link([{max_objs, 2}]),
  lru:add(Cache, 1, 1),
  lru:add(Cache, 2, 2),
  ?assertEqual(lru:contains_or_add(Cache, 1, fun() -> 1 end), {true, false}),
  lru:add(Cache, 3, 3),
  ?assertEqual(lru:contains_or_add(Cache, 1, fun() -> 1 end), {false, true}),
  ?assert(lru:contains(Cache, 1)),
  lru:stop(Cache),
  ok.

lru_contains_or_add_with_test() ->
  {ok, Cache} = lru:start_link([{max_objs, 2}]),
  lru:add(Cache, 1, 1),
  lru:add(Cache, 2, 2),
  ?assertEqual(lru:contains_or_add_with(Cache, 1, fun() -> 1 end), {true, false}),
  lru:add(Cache, 3, 3),
  ?assertEqual(lru:contains_or_add_with(Cache, 1, fun() -> 1 end), {false, true}),
  ?assert(lru:contains(Cache, 1)),
  lru:stop(Cache),
  ok.

lru_peek_test() ->
  {ok, Cache} = lru:start_link([{max_objs, 2}]),
  lru:add(Cache, 1, 1),
  lru:add(Cache, 2, 2),
  ?assertEqual(lru:peek(Cache, 1), 1),
  lru:add(Cache, 3, 3),
  ?assert(lru:contains(Cache, 1) =:= false),
  lru:stop(Cache),
  ok.


lru_info_test() ->
  {ok, Cache} = lru:start_link([{max_objs, 2}]),
  lru:add(Cache, 1, 1),
  lru:add(Cache, 2, 2),
  ?assertEqual([{max_size, 0}, {size, 14}, {max_objs, 2}, {len, 2}], lru:info(Cache)),
  lru:stop(Cache),
  ok.

lru_set_count_test() ->
  {ok, Cache} = lru:start_link([{max_objs, 2}]),
  lru:add(Cache, 1, 1),
  lru:add(Cache, 2, 2),
  ?assertEqual([{max_size, 0}, {size, 14}, {max_objs, 2}, {len, 2}], lru:info(Cache)),
  lru:set_count(Cache, 3),
  ?assertEqual([{max_size, 0}, {size, 14}, {max_objs, 3}, {len, 2}], lru:info(Cache)),
  lru:add(Cache, 3, 3),
  ?assertEqual(lru:keys(Cache), [1, 2, 3]),
  lru:reset_count(Cache, 1),
  ?assertEqual(lru:keys(Cache), [3]),
  lru:stop(Cache),
  ok.

lru_size_test() ->
  {ok, Cache} = lru:start_link([{max_objs, 0}, {max_size, 14}]),
  lru:add(Cache, 1, 1),
  lru:add(Cache, 2, 2),
  ?assertEqual([1, 2], lru:keys(Cache)),
  lru:add(Cache, 3, 3),
  ?assertEqual([2, 3], lru:keys(Cache)),
  lru:add(Cache, 4, 4),
  ?assertEqual([3, 4], lru:keys(Cache)),
  lru:set_size(Cache, 18),
  lru:add(Cache, 5, 5),
  ?assertEqual([3, 4, 5], lru:keys(Cache)),
  lru:resize(Cache, 14),
  ?assertEqual([4, 5], lru:keys(Cache)),
  lru:stop(Cache),
  ok.


-endif.
