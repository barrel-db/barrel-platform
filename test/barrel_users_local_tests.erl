
%% Copyright (c) 2016. Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% Created by benoitc on 21/06/16.

-module(barrel_users_local_tests).
-author("Benoit Chesneau").

-include_lib("eunit/include/eunit.hrl").

-define(tempdir,
  fun() ->
    Nums = tuple_to_list(erlang:timestamp()),
    Prefix = "eunit-test-dir",
    Suffix = lists:concat([integer_to_list(Num) || Num <- Nums]),
    list_to_binary(Prefix ++ "-" ++ Suffix)
  end).

setup() ->
  Dir = filename:absname(binary_to_list(?tempdir())),
  filelib:ensure_dir(filename:join(Dir,"dummy")),
  ets:new(barrel_gvar, [ordered_set, public, named_table]),
  ets:insert(barrel_gvar, {dir, Dir}),

  {ok, Pid} = barrel_users_local:start_link(),
  {Dir, Pid}.

teardown({Dir, Pid}) ->
  barrel_lib:shutdown_sync(Pid),
  ets:delete(barrel_gvar),
  Files = filelib:wildcard(filename:join(Dir, "PASSWD.*")),
  lists:foreach(fun(File) -> file:delete(File) end, Files),
  file:del_dir(Dir).


barrel_user_local_test_() ->
  {
    "Barrel user local test",
    {foreach,
      fun setup/0, fun teardown/1,
      [
        fun password_file_exists/1,
        fun null_user_created/1,
        fun set_user/1,
        fun delete_user/1,
        fun check_password/1,
        fun reload_password_file/1
      ]
    }
  }.


password_file_exists({Dir, _Pid}) ->
  ?_assert(filelib:is_regular(filename:join([Dir, ".barrel", "users.0"]))).


null_user_created({_Dir, _Pid}) ->
  ?_assertMatch({ok, _Doc}, barrel_users_local:get_user(<<"barrel">>)).

set_user({_Dir, _Pid}) ->
  ok = barrel_users_local:set_user(<<"test">>, <<"pass">>, [<<"_admin">>], #{}),
  ?_assertMatch({ok, _Doc}, barrel_users_local:get_user(<<"test">>)).

delete_user({_Dir, _Pid}) ->
  ok = barrel_users_local:set_user(<<"test">>, <<"pass">>, [<<"_admin">>], #{}),
  {ok, _Doc} = barrel_users_local:get_user(<<"test">>),
  ok = barrel_users_local:delete_user(<<"test">>),
  ?_assertEqual({error, not_found}, barrel_users_local:get_user(<<"test">>)).

check_password({_Dir, _Pid}) ->
  ok = barrel_users_local:set_user(<<"test">>, <<"pass">>, [<<"_admin">>], #{}),
  {ok, Doc} = barrel_users_local:get_user(<<"test">>),
  #{<<"salt">> := Salt,
    <<"iterations">> := Iterations,
    <<"derived_key">> := ExpectedHash} = Doc,
  Hash = barrel_passwords:pbkdf2(<<"pass">>, Salt, Iterations),
  ?_assertEqual(true, barrel_passwords:verify(ExpectedHash, Hash)).

reload_password_file({Dir, _Pid}) ->
  {ok, File} = file:open(filename:join([Dir, ".barrel", "users.0"]), [raw, write]),
  ok = file:write(File, ["barrel:test\n"]),
  ok = barrel_users_local:reload(),
  {ok, Doc} = barrel_users_local:get_user(<<"barrel">>),
  #{<<"salt">> := Salt,
    <<"iterations">> := Iterations,
    <<"derived_key">> := ExpectedHash} = Doc,
  Hash = barrel_passwords:pbkdf2(<<"test">>, Salt, Iterations),
  ?_assertEqual(true, barrel_passwords:verify(ExpectedHash, Hash)).

