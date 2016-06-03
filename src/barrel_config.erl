%% Copyright 2016 Benoit Chesneau
%%
%% Licensed under the EUPL, Version 1.1 only (the "Licence");
%% You may not use this work except in compliance with the Licence.
%% You may obtain a copy of the Licence at:
%%
%% https://joinup.ec.europa.eu/software/page/eupl
%%
%% Unless required by applicable law or agreed to in  writing, software
%% distributed under the Licence is distributed on an "AS IS" basis, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the Licence for the specific language governing permissions and
%% limitations under the Licence.

%%% -*- erlang -*-
%%%
%%% This file is part of barrel_config released under the Apache 2 license.
%%% See the NOTICE for more information.


%% @doc module wrapping econfig calls.
-module(barrel_config).

%% public API
-export([init/1]).
-export([all/0]).
-export([get/1, get/2, get/3]).
-export([set/2, set/3, set/4]).
-export([del/2, del/3]).
-export([get_boolean/2, get_boolean/3]).
-export([get_integer/2, get_integer/3]).
-export([get_float/2, get_float/3]).
-export([get_list/2, get_list/3]).
-export([get_binary/2, get_binary/3]).
-export([subscribe/0, unsubscribe/0]).


%% internal usage
-export([handle_config_change/1]).

-define(CFGNAME, barrel).


%% @doc int with config files
init(IniFiles) ->
  ok = wait_for_econfig(),
  econfig:register_config(?CFGNAME, IniFiles, [autoreload,
    {change_fun, {barrel_config, handle_config_change}}]).

all() -> econfig:cfg2list(?CFGNAME).

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