%% Copyright 2016, Bernard Notarianni
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

%%%-------------------------------------------------------------------
%% @doc barrel_http top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(barrel_http_sup).
-author("Bernard Notarianni").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-define(DEFAULT_NB_ACCEPTORS, 100).
-define(DEFAULT_PORT, 7080).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(_Args) ->
  ListenPort = application:get_env(barrel_http, listen_port, ?DEFAULT_PORT),
  NbAcceptors = application:get_env(barrel_http, nb_acceptors, ?DEFAULT_NB_ACCEPTORS),
  
  Trails =
  trails:trails([ cowboy_swagger_handler
                  , barrel_http_rest_system
                  , barrel_http_rest_replicate
                  , barrel_http_rest_revsdiff
                  , barrel_http_rest_changes
                  , barrel_http_rest_all_docs
                  , barrel_http_rest_walk
                  , barrel_http_rest_dbs
                  , barrel_http_rest_db
    
                  , barrel_http_rest_doc
                  , barrel_http_rest_root
                ]),
  trails:store(Trails),
  Dispatch = trails:single_host_compile(Trails),
  
  
  Http = ranch:child_spec(
    barrel_http, NbAcceptors, ranch_tcp, [{port, ListenPort}], cowboy_protocol,
    [{env, [{dispatch, Dispatch}]}]
  ),

  Specs = [Http],
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  {ok, {SupFlags, Specs}}.
