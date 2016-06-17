%% Copyright 2016 Benoit Chesneau
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

%%% -*- erlang -*-
%%%
%%% This file is part of barrel_config released under the Apache 2 license.
%%% See the NOTICE for more information.


%% @doc module wrapping econfig calls.
-module(barrel_config).

%% public API
-export([init/1]).
-export([all/0]).
-export([prefix/1]).
-export([get/1, get/2, get/3]).
-export([set/2, set/3, set/4]).
-export([del/2, del/3]).
-export([get_boolean/2, get_boolean/3]).
-export([get_integer/2, get_integer/3]).
-export([get_float/2, get_float/3]).
-export([get_list/2, get_list/3]).
-export([get_binary/2, get_binary/3]).
-export([subscribe/0, unsubscribe/0]).
-export([get_env/1, get_env/2]).

-export([pget_boolean/2, pget_boolean/3, pget_int/2, pget_int/3,
  pget_float/2, pget_float/3, pget_list/2, pget_list/3,
  pget_binary/2, pget_binary/3]).

-export([section_to_opts/1]).

%% internal usage
-export([handle_config_change/1]).

-define(CFGNAME, barrel).



%% @doc int with config files
init(IniFiles) ->
  ok = wait_for_econfig(),
  econfig:register_config(?CFGNAME, IniFiles, [autoreload,
    {change_fun, {barrel_config, handle_config_change}}]).

all() -> econfig:cfg2list(?CFGNAME).

prefix(Prefix) -> econfig:prefix(?CFGNAME, Prefix).

get(Section) -> econfig:get_value(?CFGNAME, Section).
get(Section, Key) -> econfig:get_value(?CFGNAME, Section, Key).
get(Section, Key, Default) -> econfig:get_value(?CFGNAME, Section, Key, Default).

set(Section, KVs) -> econfig:set_value(?CFGNAME, Section, KVs).
set(Section, Key, Value) -> econfig:set_value(?CFGNAME, Section, Key, Value).
set(Section, Key, Value, Persist) -> econfig:set_value(?CFGNAME, Section, Key, Value, Persist).

del(Section, Key) -> econfig:delete_value(?CFGNAME, Section, Key).
del(Section, Key, Persist) -> econfig:delete_value(?CFGNAME, Section, Key, Persist).

get_boolean(Section, Key) -> econfig:get_boolean(?CFGNAME, Section, Key).
get_boolean(Section, Key, Default) -> econfig:get_boolean(?CFGNAME, Section, Key, Default).

get_integer(Section, Key) -> econfig:get_integer(?CFGNAME, Section, Key).
get_integer(Section, Key, Default) -> econfig:get_integer(?CFGNAME, Section, Key, Default).

get_float(Section, Key) -> econfig:get_float(?CFGNAME, Section, Key).
get_float(Section, Key, Default) -> econfig:get_float(?CFGNAME, Section, Key, Default).

get_list(Section, Key) -> econfig:get_list(?CFGNAME, Section, Key).
get_list(Section, Key, Default) -> econfig:get_list(?CFGNAME, Section, Key, Default).

get_binary(Section, Key) -> econfig:get_binary(?CFGNAME, Section, Key).
get_binary(Section, Key, Default) -> econfig:get_binary(?CFGNAME, Section, Key, Default).


subscribe() -> econfig:subscribe(?CFGNAME).

unsubscribe() -> econfig:unsubscribe(?CFGNAME).


get_env(Key) -> get_env(Key, undefined).

get_env(Key, Default) ->
  case barrel_config:get("barrel", Key) of
    undefined ->
      application:get_env(barrel, Key, Default);
    Val ->
      Val
  end.

section_to_opts(Name) -> [{barrel_lib:to_atom(K), V} || {K, V} <- barrel_config:get(Name)].


pget_boolean(Key, Props) -> pget_boolean(Key, Props, undefined).
pget_boolean(Key, Props, Default) ->
  case proplists:get_value(Key, Props, Default) of
    undefined -> Default;
    Val when is_boolean(Val) -> Val;
    Val ->
      case string:to_lower(Val) of
        "true" -> true;
        "false" -> false;
        "1" -> true;
        "0" -> false;
        "on" -> true;
        "off" -> false;
        "yes" -> true;
        "no" -> false;
        true -> true;
        false -> false;
        _ ->
          error(badarg)
      end
  end.

pget_int(Key, Props) -> pget_int(Key, Props, undefined).
pget_int(Key, Props, Default) ->
  case proplists:get_value(Key, Props, Default) of
    undefined -> Default;
    Val when is_integer(Val) -> Val;
    Val ->
      case string:to_integer(Val) of
        {IVal, []} -> IVal;
        _ -> error(badarg)
      end
  end.

pget_float(Key, Props) -> pget_float(Key, Props, undefined).
pget_float(Key, Props, Default) ->
  case proplists:get_value(Key, Props, Default) of
    undefined -> Default;
    Val when is_float(Val) -> Val;
    Val ->
      case string:to_float(Val) of
        {FVal, []} -> FVal;
        _ -> error(badarg)
      end
  end.

pget_list(Key, Props) -> pget_list(Key, Props, undefined).
pget_list(Key, Props, Default) ->
  case proplists:get_value(Key, Props, Default) of
    undefined -> Default;
    Val when is_list(Val) -> Val;
    "" -> [];
    Val ->
      lists:filtermap(fun(V) ->
        case string:strip(V) of
          "" -> false;
          V2 -> {true, V2}
        end
                      end, string:tokens(Val, ","))
  end.

pget_binary(Key, Props) -> pget_binary(Key, Props, undefined).
pget_binary(Key, Props, Default) ->
  case proplists:get_value(Key, Props, Default) of
    undefined -> Default;
    Val when is_binary(Val) -> Val;
    Val ->
      try list_to_binary(Val) of
        Bin ->  Bin
      catch
        _ -> error(badarg)
      end
  end.

%% @private
handle_config_change({config_updated, ?CFGNAME, {Type, {Section, Key}}}) ->
  hooks:run(config_key_update, [Section, Key, Type]);
handle_config_change({config_updated, ?CFGNAME, Type}) ->
  hooks:run(config_update, Type);
handle_config_change(_) ->
  ok.

%% @private
wait_for_econfig() ->
  case whereis(econfig_sup) of
    undefined ->
      timer:sleep(10),
      wait_for_econfig();
    _Pid ->
      ok
  end.
