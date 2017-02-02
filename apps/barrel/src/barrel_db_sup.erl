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

-module(barrel_db_sup).
-author("Benoit Chesneau").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([open_db/2]).

-include("barrel.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init(any()) ->
  {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  {ok, {{one_for_one, 10, 60}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private

-spec open_db(binary(), map()) -> supervisor:startchild_ret().
open_db(DbId, Config) ->
  supervisor:start_child(?MODULE, db_spec(DbId, Config)).


db_spec(Id, Config) ->
  #{
    id => Id,
    start => {barrel_db, start_link, [Id, Config]},
    restart => temporary,
    shutdown => 2000,
    type => worker,
    modules => [barrel_db]
  }.