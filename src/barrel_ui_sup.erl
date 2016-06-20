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

%% Created by benoitc on 17/06/16.

-module(barrel_ui_sup).
-author("Benoit Chesneau").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init(any()) ->
  {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  SupFlags = {one_for_one, 10, 3600},

  Auth = {barrel_auth,
    {barrel_auth, start_link, []},
    permanent, brutal_kill, worker, [barrel_auth]},

  AuthCache =
    {barrel_aut_cache,
      {barrel_auth_cache, start_link, []},
      permanent, brutal_kill, worker, [barrel_aut_cache]},

  Api = {barrel_api_sup,
    {barrel_api_sup, start_link, []},
    permanent, infinity, supervisor, [barrel_api_sup]},


  {ok, {SupFlags, [Auth, AuthCache, Api]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

