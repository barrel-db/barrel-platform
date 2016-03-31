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

-module(couch_couchjs).

-export([start_link/1]).
-export([set_timeout/2, prompt/2]).
-export([send/2, writeline/2, readline/1, writejson/2, readjson/1]).


start_link(JsFile) ->
    Command = filename:join([couch_util:priv_dir(), "barreljs"]),
    couch_os_process:start_link(Command ++ " " ++ JsFile).

set_timeout(Pid, Timeout) ->
    couch_os_process:set_timeout(Pid, Timeout).
    
send(Pid, Data) ->
    couch_os_process:send(Pid, Data).

prompt(Pid, Data) ->
    couch_os_process:prompt(Pid, Data).

writeline(OsProc, Data) ->
    couch_os_process:writeline(OsProc, Data).

readline(OsProc) ->
    couch_os_process:readline(OsProc).

writejson(OsProc, Data) ->
    couch_os_process:write_json(OsProc, Data).

readjson(OsProc) ->
    couch_os_process:readjson(OsProc).
