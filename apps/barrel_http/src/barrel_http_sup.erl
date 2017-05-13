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
-define(DEFAULT_ACCESS_LOG, false).
-define(DEFAULT_METRICS, undefined).
-define(DEFAULT_TIMEOUT, 60000).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(_Args) ->
  ListenPort = application:get_env(barrel_http, listen_port, ?DEFAULT_PORT),
  NbAcceptors = application:get_env(barrel_http, nb_acceptors, ?DEFAULT_NB_ACCEPTORS),
  RequestTimeout = application:get_env(barrel_http, request_timeout, ?DEFAULT_TIMEOUT),

  _ = prometheus_http_impl:setup(),

  Routes = [ {"/api-doc", barrel_http_redirect,
              [{location, <<"/api-doc/index.html">>}]}
           , {"/api-doc/[...]", cowboy_static, {priv_dir, barrel_http, "swagger",
                                                 [{mimetypes, cow_mimetypes, all}]}}

           , {"/dbs/:database/system/:docid", barrel_http_rest_system, []}
           , {"/replicate",                   barrel_http_rest_replicate, []}
           , {"/replicate/:repid",            barrel_http_rest_replicate, []}
           , {"/dbs/:database/revsdiff",      barrel_http_rest_revsdiff, []}
           , {"/dbs/:database/walk/[...]",    barrel_http_rest_walk, []}
           , {"/dbs",                         barrel_http_rest_dbs, []}
           , {"/dbs/:database",               barrel_http_rest_db, []}
           , {"/dbs/:database/docs",          barrel_http_rest_docs, []}
           , {"/dbs/:database/docs/:docid",   barrel_http_rest_docs, []}
           , {"/",                            barrel_http_rest_root, []}
           , {"/metrics",                     barrel_monitor_exporter_handler, {default}}
           ],
  Dispatch = cowboy_router:compile([{'_', Routes}]),

  Options0 = #{env => #{dispatch => Dispatch}, request_timeout => RequestTimeout},

  Streams =  [barrel_http_count, cowboy_stream_h],

  Options1 = Options0#{stream_handlers => Streams},

  _  = lager:info("starting HTTP server on port ~p", [ListenPort]),
  Http = ranch:child_spec(
           barrel_http, NbAcceptors,
           ranch_tcp, [{port, ListenPort}],
           cowboy_clear, Options1),

  Specs = [Http],
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  {ok, {SupFlags, Specs}}.
