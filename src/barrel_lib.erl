%% Copyright 2016, Benoit Chesneau
%% Copyright 2009-2014 The Apache Software Foundation
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

-export([val/1, val/2]).
-export([set/2]).
-export([unset/1]).


-export([to_binary/1]).
-export([to_error/1]).
-export([to_atom/1]).
-export([to_existing_atom/1]).
-export([to_list/1]).
-export([to_integer/1]).
-export([to_hex/1]).
-export([hex_to_binary/1]).

-export([join/2]).
-export([hexsig/1]).
-export([parse_term/1]).

-export([userctx/0, userctx/1, userctx_get/2,
         userctx_put/2, userctx_put/3, is_userctx/1]).
-export([adminctx/0]).
-export([json_user_ctx/1]).

-export([load_config/2]).
-export([propmerge/3, propmerge1/2]).

-export([priv_dir/0]).
-export([shutdown_sync/1]).
-export([get_nested_json_value/2]).
-export([validate_utf8/1]).
-export([should_flush/0, should_flush/1]).

-export([rfc1123_date/0, rfc1123_date/1]).
-export([url_encode/1]).
-export([dict_find/3]).
-export([rand32/0]).
-export([with_db/2]).
-export([url_strip_password/1]).

-export([encodeb64url/1, decodeb64url/1]).

-export([delete_file/1, delete_file/2]).


-include_lib("syntax_tools/include/merl.hrl").
-include_lib("couch_db.hrl").

-include("barrel.hrl").

-define(FLUSH_MAX_MEM, 10000000).

-type userctx() :: map().


val(Key) ->
  val(Key, undefined).

val(Key, Default) ->
  try ets:lookup_element(barrel_gvar, Key, 2)
  catch error:_ -> Default
  end.

set(Key, Val) ->
  ets:insert(barrel_gvar, {Key, Val}).

unset(Key) ->
  ets:delete(barrel_gvar, Key).


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

% works like list_to_existing_atom, except can be list or binary and it
% gives you the original value instead of an error if no existing atom.
to_existing_atom(V) when is_list(V) -> try list_to_existing_atom(V) catch _:_ -> V end;
to_existing_atom(V) when is_binary(V) -> try list_to_existing_atom(binary_to_list(V)) catch _:_ -> V end;
to_existing_atom(V) when is_atom(V) -> V.

to_list(V) when is_list(V) -> V;
to_list(V) when is_binary(V) -> V;
to_list(V) when is_atom(V) -> atom_to_list(V);
to_list(V) when is_integer(V) -> integer_to_list(V).

to_integer(V) when is_integer(V) -> V;
to_integer(V) when is_binary(V) -> binary_to_integer(V);
to_integer(V) when is_list(V) -> list_to_integer(V).

to_hex([]) -> [];
to_hex(Bin) when is_binary(Bin) ->
  << <<(to_digit(H)),(to_digit(L))>> || <<H:4,L:4>> <= Bin >>;
to_hex([H|T]) ->
  [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.

hex_to_binary(Bin) when is_binary(Bin) ->
  << <<(binary_to_integer( <<H, L>>, 16))>> || << H, L >> <= Bin >>.


parse_term(Bin) when is_binary(Bin) ->  parse_term(binary_to_list(Bin));
parse_term(List) -> {ok, Tokens, _} = erl_scan:string(List ++ "."), erl_parse:parse_term(Tokens).


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

hexsig(Sig) ->
  barrel_lib:to_hex(binary_to_list(Sig)).


dict_find(Key, Dict, DefaultValue) ->
  case dict:find(Key, Dict) of
    {ok, Value} ->
      Value;
    error ->
      DefaultValue
  end.

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
g(Else, C) -> maps:get(Else, C, undefined).

-spec userctx_put(any(), any(), userctx()) -> userctx().
userctx_put(K, V, Ctx) when is_map(Ctx) -> Ctx#{K => V};
userctx_put(_, _, _)  -> erlang:error(badarg).

-spec userctx_put(list(), userctx()) -> userctx().
userctx_put(L, C) when is_list(L) -> [userctx_put(K, V, C) || {K, V} <- L].

is_userctx(C) when is_map(C) -> true;
is_userctx(_) -> false.

json_user_ctx(#db{name=DbName, user_ctx=Ctx}) ->
  [Name, Roles] = barrel_lib:userctx_get([name, roles], Ctx),
  #{<<"db">> => DbName,
    <<"name">> => Name,
    <<"roles">> => Roles}.

url_strip_password(Url) ->
  re:replace(Url,
    "http(s)?://([^:]+):[^@]+@(.*)$",
    "http\\1://\\2:*****@\\3",
    [{return, list}]).

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

priv_dir() ->
  case code:priv_dir(barrel) of
    {error, _} ->
      %% try to get relative priv dir. useful for tests.
      EbinDir = filename:dirname(code:which(?MODULE)),
      AppPath = filename:dirname(EbinDir),
      filename:join(AppPath, "priv");
    Dir -> Dir
  end.

shutdown_sync(Pid) when not is_pid(Pid)->
  ok;
shutdown_sync(Pid) ->
  MRef = erlang:monitor(process, Pid),
  try
      catch unlink(Pid),
      catch exit(Pid, shutdown),
    receive
      {'DOWN', MRef, _, _, _} ->
        ok
    end
  after
    erlang:demonitor(MRef, [flush])
  end.

get_nested_json_value(#{}=Obj, [Key|Keys]) ->
  case maps:get(Key, Obj, nil) of
    nil -> throw({not_found, <<"missing json key: ", Key/binary>>});
    Value -> get_nested_json_value(Value, Keys)
  end;
get_nested_json_value(Value, []) ->
  Value;
get_nested_json_value(_NotJSONObj, _) ->
  throw({not_found, json_mismatch}).

validate_utf8(Data) when is_list(Data) ->
  validate_utf8(list_to_binary(Data));
validate_utf8(Bin) when is_binary(Bin) ->
  validate_utf8_fast(Bin, 0).

validate_utf8_fast(B, O) ->
  case B of
    <<_:O/binary>> ->
      true;
    <<_:O/binary, C1, _/binary>> when
      C1 < 128 ->
      validate_utf8_fast(B, 1 + O);
    <<_:O/binary, C1, C2, _/binary>> when
      C1 >= 194, C1 =< 223,
      C2 >= 128, C2 =< 191 ->
      validate_utf8_fast(B, 2 + O);
    <<_:O/binary, C1, C2, C3, _/binary>> when
      C1 >= 224, C1 =< 239,
      C2 >= 128, C2 =< 191,
      C3 >= 128, C3 =< 191 ->
      validate_utf8_fast(B, 3 + O);
    <<_:O/binary, C1, C2, C3, C4, _/binary>> when
      C1 >= 240, C1 =< 244,
      C2 >= 128, C2 =< 191,
      C3 >= 128, C3 =< 191,
      C4 >= 128, C4 =< 191 ->
      validate_utf8_fast(B, 4 + O);
    _ ->
      false
  end.


rfc1123_date() ->
  {{YYYY,MM,DD},{Hour,Min,Sec}} = calendar:universal_time(),
  DayNumber = calendar:day_of_the_week({YYYY,MM,DD}),
  lists:flatten(
    io_lib:format("~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
      [day(DayNumber),DD,month(MM),YYYY,Hour,Min,Sec])).

rfc1123_date(undefined) ->
  undefined;
rfc1123_date(UniversalTime) ->
  {{YYYY,MM,DD},{Hour,Min,Sec}} = UniversalTime,
  DayNumber = calendar:day_of_the_week({YYYY,MM,DD}),
  lists:flatten(
    io_lib:format("~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
      [day(DayNumber),DD,month(MM),YYYY,Hour,Min,Sec])).

%% day

day(1) -> "Mon";
day(2) -> "Tue";
day(3) -> "Wed";
day(4) -> "Thu";
day(5) -> "Fri";
day(6) -> "Sat";
day(7) -> "Sun".

%% month

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

url_encode(Bin) when is_binary(Bin) ->
  url_encode(binary_to_list(Bin));
url_encode([H|T]) ->
  if
    H >= $a, $z >= H ->
      [H|url_encode(T)];
    H >= $A, $Z >= H ->
      [H|url_encode(T)];
    H >= $0, $9 >= H ->
      [H|url_encode(T)];
    H == $_; H == $.; H == $-; H == $: ->
      [H|url_encode(T)];
    true ->
      case lists:flatten(io_lib:format("~.16.0B", [H])) of
        [X, Y] ->
          [$%, X, Y | url_encode(T)];
        [X] ->
          [$%, $0, X | url_encode(T)]
      end
  end;
url_encode([]) ->
  [].

% returns a random integer
rand32() -> crypto:rand_uniform(0, 16#100000000).

should_flush() -> should_flush(?FLUSH_MAX_MEM).
should_flush(MemThreshHold) ->
  {memory, ProcMem} = process_info(self(), memory),
  BinMem = lists:foldl(fun({_Id, Size, _NRefs}, Acc) -> Size+Acc end,
    0, element(2,process_info(self(), binary))),
  if ProcMem+BinMem > 2*MemThreshHold ->
    garbage_collect(),
    {memory, ProcMem2} = process_info(self(), memory),
    BinMem2 = lists:foldl(fun({_Id, Size, _NRefs}, Acc) -> Size+Acc end,
      0, element(2,process_info(self(), binary))),
    ProcMem2+BinMem2 > MemThreshHold;
    true -> false end.



with_db(Db, Fun) when is_record(Db, db) ->
  Fun(Db);
with_db(DbName, Fun) ->
  case couch_db:open_int(DbName, [{user_ctx, barrel_lib:adminctx()}]) of
    {ok, Db} ->
      try Fun(Db)
      after
          catch couch_db:close(Db)
      end;
    Else ->
      throw(Else)
  end.

-spec encodeb64url(binary() | iolist()) -> binary().
encodeb64url(Bin) when is_binary(Bin) ->
  << << (urlencode_digit(D)) >> || <<D>> <= base64:encode(Bin), D =/= $= >>;
encodeb64url(L) when is_list(L) -> encodeb64url(iolist_to_binary(L)).

-spec decodeb64url(binary() | iolist()) -> binary().
decodeb64url(Bin) when is_binary(Bin) ->
  Bin2 = case byte_size(Bin) rem 4 of
           % 1 -> << Bin/binary, "===" >>;
           2 -> << Bin/binary, "==" >>;
           3 -> << Bin/binary, "=" >>;
           _ -> Bin
         end,
  base64:decode(<< << (urldecode_digit(D)) >> || <<D>> <= Bin2 >>);
decodeb64url(L) when is_list(L) ->
  decodeb64url(iolist_to_binary(L)).

urlencode_digit($/) -> $_;
urlencode_digit($+) -> $-;
urlencode_digit(D)  -> D.

urldecode_digit($_) -> $/;
urldecode_digit($-) -> $+;
urldecode_digit(D)  -> D.


delete_file(Name) ->
  delete_file(Name, true).


delete_file(Name, Async) ->
  Dir = filename:dirname(Name),
  Base = filename:basename(Name),
  DelFile = filename:join([Dir, lists:flatten([".", Base, ".del",
    binary_to_list(barrel_uuids:random())])]),

  case file:rename(Name, DelFile) of
    ok ->
      case Async of
        true ->
          spawn(file, delete, [DelFile]),
          ok;
        false -> file:delete(DelFile)
      end;
    Error ->
      Error
  end.
