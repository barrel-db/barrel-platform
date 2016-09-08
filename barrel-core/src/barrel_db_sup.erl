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
-author("benoitc").

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(term(), term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, Store) ->
  Key = {n, l, {db_sup, Name}},
  supervisor:start_link({via, gproc, Key}, ?MODULE, [Name, Store]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
                     MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([Name, Store]) ->
  Children = [db_spec(Name, Store),
              event_mgr(Name)],
  
  {ok, {{rest_for_one, 1, 5}, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

event_mgr(Name) ->
  Key = {n, l, {event, Name}},
  #{id    => {ev, Name},
    start => {gen_event, start_link, [{via, gproc, Key}]}}.


db_spec(Name, Store) ->
  #{
    id => {db, Name},
    start => {barrel_db, start_link, [Name, Store]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [barrel_db]
  }.
  
