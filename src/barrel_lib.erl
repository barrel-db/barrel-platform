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

-module(barrel_lib).

-export([to_binary/1]).
-export([to_error/1]).
-export([to_atom/1]).
-export([join/2]).

-export([userctx/0, userctx/1, userctx_get/2,
         userctx_put/2, userctx_put/3, is_userctx/1]).
-export([adminctx/0]).
-export([load_config/2]).
-export([propmerge/3, propmerge1/2]).

-include_lib("syntax_tools/include/merl.hrl").

-type userctx() :: map().

-spec to_binary(binary()|list()|integer()|atom()) -> binary().
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, latin1);
to_binary(_) -> erlang:error(badarg).

to_error(V) ->
  try to_binary(V)
  catch
    _:_ -> list_to_binary(io_lib:format("~p", [V]))
  end.

to_atom(V) when is_atom(V) -> V;
to_atom(V) when is_list(V) -> list_to_atom(V);
to_atom(V) when is_binary(V) -> binary_to_atom(V, latin1).

join([], _Separator) ->
  <<>>;
join([S], _separator) ->
  S;
join(L, Separator) ->
  iolist_to_binary(join(lists:reverse(L), Separator, [])).

join([], _Separator, Acc) ->
  Acc;
join([S | Rest], Separator, []) ->
  join(Rest, Separator, [S]);
join([S | Rest], Separator, Acc) ->
  join(Rest, Separator, [S, Separator | Acc]).

-spec userctx() -> userctx().
userctx() -> #{}.

-spec userctx(list()) -> userctx().
userctx(L) -> maps:from_list(L).

-spec adminctx() -> userctx().
adminctx() -> userctx([{roles, [<<"_admin">>]}]).

-spec userctx_get(atom()|list(), userctx()) -> any().
userctx_get(L, C) when is_list(L) -> [g(P, C) || P <- L];
userctx_get(P, C) when is_atom(P) -> g(P, C).

g(name, #{ name := Ret}) -> Ret;
g(name, _C) -> null;
g(roles, #{roles := Ret}) -> Ret;
g(roles, _C) -> [];
g(handler, #{handler := Ret}) -> Ret;
g(handler, _C) -> undefined;
g(Else, C) -> maps:get(Else, C).

-spec userctx_put(any(), any(), userctx()) -> userctx().
userctx_put(K, V, Ctx) when is_map(Ctx) -> Ctx#{K => V};
userctx_put(_, _, _)  -> erlang:error(badarg).

-spec userctx_put(list(), userctx()) -> userctx().
userctx_put(L, C) when is_list(L) -> [userctx_put(K, V, C) || {K, V} <- L].

is_userctx(C) when is_map(C) -> true;
is_userctx(_) -> false.

%% @doc Utility that converts a given property list into a module that provides
%% constant time access to the various key/value pairs.
%%
%% Example:
%%
%%   load_config(store_config, [{backends, [{rocksdb_ram, barrel_rocksdb},
%%                                          {rocksdb_disk, barrel_rocksdb}]},
%%                              {data_dir, "/path/to_datadir"}]).
%%
%% creates the module store_config:
%%   store_config:backends(). => [{rocksdb_ram,barrel_rocksdb},{rocksdb_disk,barrel_rocksdb}]
%%   store_config:data_dir => "/path/to_datadir"
%%
-spec load_config(atom(), [{atom(), any()}]) -> ok.
load_config(Resource, Config) when is_atom(Resource), is_list(Config) ->
  Module = ?Q("-module(" ++ atom_to_list(Resource) ++ ")."),
  Functions = lists:foldl(fun({K, V}, Acc) ->
                              [make_function(K,
                                             V)
                               | Acc]
                          end,
                          [], Config),
  Exported = [?Q("-export([" ++ atom_to_list(K) ++ "/0]).") || {K, _V} <-
                                                               Config],
  Forms = lists:flatten([Module, Exported, Functions]),
  merl:compile_and_load(Forms, [verbose]),
  ok.

make_function(K, V) ->
    Cs = [?Q("() -> _@V@")],
      F = erl_syntax:function(merl:term(K), Cs),
        ?Q("'@_F'() -> [].").


%% @doc merge 2 proplists. All the Key - Value pairs from both proplists
%% are included in the new proplists. If a key occurs in both dictionaries
%% then Fun is called with the key and both values to return a new
%% value. This a wreapper around dict:merge
propmerge(F, L1, L2) ->
  dict:to_list(dict:merge(F, dict:from_list(L1), dict:from_list(L2))).

%% @doc Update a proplist with values of the second. In case the same
%% key is in 2 proplists, the value from the first are kept.
propmerge1(L1, L2) ->
  propmerge(fun(_, V1, _) -> V1 end, L1, L2).