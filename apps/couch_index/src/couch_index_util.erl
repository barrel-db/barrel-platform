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

-module(couch_index_util).

-export([root_dir/0]).
-export([index_dir/2, index_file/3]).
-export([sort_lib/1]).

-include_lib("couch/include/couch_db.hrl").

root_dir() ->
  barrel_config:get("couchdb", "view_index_dir").


index_dir(Module, DbName) when is_binary(DbName) ->
    DbDir = "." ++ binary_to_list(DbName) ++ "_design",
    filename:join([root_dir(), DbDir, Module]);
index_dir(Module, #db{}=Db) ->
    index_dir(Module, couch_db:name(Db)).


index_file(Module, DbName, FileName) ->
    filename:join(index_dir(Module, DbName), FileName).


sort_lib({Lib}) ->
    sort_lib(Lib, []).
sort_lib([], LAcc) ->
    lists:keysort(1, LAcc);
sort_lib([{LName, {LObj}}|Rest], LAcc) ->
    LSorted = sort_lib(LObj, []), % descend into nested object
    sort_lib(Rest, [{LName, LSorted}|LAcc]);
sort_lib([{LName, LCode}|Rest], LAcc) ->
    sort_lib(Rest, [{LName, LCode}|LAcc]).
