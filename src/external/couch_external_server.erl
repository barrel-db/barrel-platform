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

-module(couch_external_server).
-behaviour(gen_server).

-export([start_link/2, stop/1, execute/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-include_lib("couch_db.hrl").


% External API

start_link(Name, Command) ->
    gen_server:start_link(couch_external_server, [Name, Command], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

execute(Pid, JsonReq) ->
    {json, Json} = gen_server:call(Pid, {execute, JsonReq}, infinity),
    ?JSON_DECODE(Json).

% Gen Server Handlers

init([Name, Command]) ->
    lager:info("EXTERNAL: Starting process for: ~s", [Name]),
    lager:info("COMMAND: ~s", [Command]),
    process_flag(trap_exit, true),
    Timeout = barrel_config:get_integer("couchdb", "os_process_timeout", 5000),
    barrel_config:subscribe(),
    {ok, Pid} = couch_os_process:start_link(Command, [{timeout, Timeout}]),
    {ok, {Name, Command, Pid}}.

terminate(_Reason, {_Name, _Command, Pid}) ->
    couch_os_process:stop(Pid),
    ok.

handle_call({execute, JsonReq}, _From, {Name, Command, Pid}) ->
    {reply, couch_os_process:prompt(Pid, JsonReq), {Name, Command, Pid}}.


handle_info({config_updated, barrel, {_, {"couchdb", "os_process_timeout"}}}, {_, _, Pid}=State) ->
    Timeout = barrel_config:get_integer("couchdb", "os_process_timeout", 5000),
    couch_os_process:set_timeout(Pid, Timeout),
    {noreply, State};
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, {Name, Command, Pid}) ->
    lager:info("EXTERNAL: Process for ~s exiting. (reason: ~w)", [Name, Reason]),
    {stop, Reason, {Name, Command, Pid}}.

handle_cast(stop, {Name, Command, Pid}) ->
    lager:info("EXTERNAL: Shutting down ~s", [Name]),
    exit(Pid, normal),
    {stop, normal, {Name, Command, Pid}};
handle_cast(_Whatever, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
