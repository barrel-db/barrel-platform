% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.
%
-module(couch_httpd_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([upgrade/0]).
-export([reload_listener/1,
         reload_listeners/0]).


%% internal API
-export([init/1]).
-export([config_change/2]).


-spec start_link() -> ignore | {error, term()} | {ok, pid()}.
start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),

    %% register to config events
    ok  = couch_config:register(fun ?MODULE:config_change/2, Pid),

    %% display uris
    couch_httpd_util:display_uris(),

    %% write_uris
    couch_httpd_util:write_uri_file(),

    {ok, Pid}.


%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @doc upgrade  a listener
-spec reload_listener(atom()) -> {ok, pid()} | {error, term()}.
reload_listener(Id) ->
    %% stop the listener and remove it from the supervision temporarely
    supervisor:terminate_child(?MODULE, Id),
    supervisor:delete_child(?MODULE, Id),

    %% restart the listener
    supervisor:start_child(?MODULE, listener_spec(Id)),
    couch_httpd_util:display_uris([Id]),
    ok.

%% upgrade all listeners
-spec reload_listeners() -> ok.
reload_listeners() ->
    [reload_listener(Id) || Id <- couch_httpd_util:get_listeners()],
    ok.


-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
    Listeners = [listener_spec(Id) || Id <- couch_httpd_util:get_listeners()],
    Vhost = {couch_httpd_vhost,
             {couch_httpd_vhost, start_link, []},
             permanent, brutal_kill, worker, [couch_httpd_vhost]},
    {ok, {{one_for_one, 9, 10}, Listeners ++ [Vhost]}}.


listener_spec(Id) ->
    {Id,
     {couch_httpd, start_link, [Id]},
     permanent, brutal_kill, worker, [couch_httpd]}.

config_change("httpd", "bind_address") ->
    ?MODULE:reload_listeners();
config_change("httpd", "port") ->
    ?MODULE:reload_listener(couch_http);
config_change("httpd", "default_handler") ->
    ?MODULE:reload_listeners();
config_change("httpd", "server_options") ->
    ?MODULE:reload_listeners();
config_change("httpd", "socket_options") ->
    ?MODULE:reload_listeners();
config_change("httpd", "authentication_handlers") ->
    couch_httpd:set_auth_handlers();
config_change("httpd_global_handlers", _) ->
    ?MODULE:reload_listeners();
config_change("httpd_db_handlers", _) ->
    ?MODULE:reload_listeners();
config_change("ssl", _) ->
    ?MODULE:reload_listener(couch_https).
