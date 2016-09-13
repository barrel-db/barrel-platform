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

-module(barrel_http).
-behaviour(gen_server).

%% specific API
-export([start_link/0]).
-export([stop/0]).

%% gen_server API
-export([init/1, handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([handle_cast/2]).



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

init(_) ->
    Routes = [{"/[:dbid]", rest_db_handler, []},
              {"/[:dbid]/_revs_diff", rest_revsdiff_handler, []},
              {"/[:dbid]/_changes", rest_changes_handler, []},
              {"/[:dbid]/[:docid]", rest_doc_handler, []}
             ],

    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, [{port, Port}]} = application:get_env(barrel, http_server),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(shutdown, State) ->
    {stop, normal, State}.

handle_info(_Info, State) -> {noreply, State}.

%% default gen_server callbacks
terminate(_Reason, _State) ->  ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
