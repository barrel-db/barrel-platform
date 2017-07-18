%% Copyright 2017, Benoit Chesneau
%% ex: ts=2 sw=2 et
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

%% Created by benoitc on 03/09/16.

-module(barrel_httpc_eqc_tests).
-author("Zach Kessin <zkessin@get-finch.com>").


-define(DB_URL,<<"http://localhost:7080/dbs/testdb">>).
-define(CONFIG, ?DB_URL).
%% API
%% -export([
%%   all/0,
%%   init_per_suite/1,
%%   end_per_suite/1,
%%   init_per_testcase/2,
%%   end_per_testcase/2
%% ]).
-export([

				 initial_state/0,
				 command/1,
				 precondition/2,
				 postcondition/3,
				 next_state/3]).

-include_lib("triq/include/triq.hrl").
%-include_lib("triq/include/triq_statem.hrl").

%% use eunit
-include_lib("eunit/include/eunit.hrl").

-behaviour('triq_statem').



%% init_per_suite(Config) ->
%%   {ok, _} = application:ensure_all_started(barrel_rest),
%%   Config.

%% init_per_testcase(_, Config) ->
%%   _ = barrel_store:create_db(<<"testdb">>, #{}),
%%   _ = barrel_store:create_db(<<"source">>, #{}),
%%   {ok, Conn} = barrel_httpc:connect(?DB_URL),
%%   [{db, Conn} | Config].

%% end_per_testcase(_, _Config) ->
%%   ok = barrel:delete_db(<<"testdb">>),
%%   ok = barrel:delete_db(<<"source">>),
%%   ok.

%% end_per_suite(Config) ->
%%   _ = application:stop(barrel_rest),
%%   Config.


%% db(Config) -> proplists:get_value(db, Config).

-record(state,
				{
					db_online = true,
					keys   = []
				 }).

%% run_test_() ->
%% 		{timeout,
%% 		 36000,
%% 		 ?_assert(check())
%% 		}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_map(KeyGen, ValueGen) ->
		?LET(List,
				 list({KeyGen, ValueGen}),
				 begin
						 maps:from_list(List)
				 end).

key() ->
		binary().

create_record() ->
		?LET({Id, Ver},
				 {key(), int(1,20)},

				 #{<<"id">> => Id, <<"v">> => Ver}).

prop_run_statem() ->
		{ok,_} = application:ensure_all_started(barrel_rest),
		?FORALL(Cmds,
						triq_statem:commands(?MODULE),
						begin
								?debugFmt("Cmds ~p", [Cmds]),
								_ = barrel_store:create_db(<<"testdb">>, #{}),
								triq_statem:run_commands(?MODULE, Cmds),
								ok = barrel:delete_db(<<"testdb">>),
								true
						end).


initial_state() ->
		#state{}.
command(_State = #state{db_online = true, keys= []}) ->
		?debugFmt("State ~p", [_State]),
		oneof([
					 {call, barrel_httpc, post,   [?CONFIG, create_record(), []]}
					 %% {call, barrel_httpc, get,    [?CONFIG, key(),           []]},
					 %% {call, barrel_httpc, delete, [?CONFIG, key(),           []]}
					]);
command(_State = #state{db_online = true, keys= Keys}) ->
		?debugFmt("State ~p", [_State]),
		oneof([
					 {call, barrel_httpc, post,   [?CONFIG, create_record(), []]},
					 {call, barrel_httpc, get,    [?CONFIG, oneof(Keys),     []]},
					 {call, barrel_httpc, delete, [?CONFIG, oneof(Keys),     []]},
					 {call, barrel_httpc, get,    [?CONFIG, key(),           []]},
					 {call, barrel_httpc, delete, [?CONFIG, key(),           []]}
					]);
command(#state{db_online = false}) ->
		oneof([


					]).


next_state(State = #state{keys= Keys},_Var, {call, barrel_httpc, post, [?CONFIG, Record,[]]}) ->
		K = maps:get(<<"id">>,Record),
		State#state{keys = [K| Keys]};
next_state(State = #state{keys = Keys},_Var, {call, barrel_httpc, delete, [?CONFIG, K,[]]}) ->
		State#state{keys = lists:delete(K, Keys)};
next_state(State,_Var, {call, barrel_httpc, get, [?CONFIG, _Record,[]]}) ->
		State;
next_state(State,_Var, _Cmd) ->
		State.


precondition(_,_) ->
		true.

postcondition(_,_,_) ->
		true.
