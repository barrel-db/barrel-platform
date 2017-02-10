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

-module(barrel_httpc_attachments_SUITE).
-author("Benoit Chesneau").

-define(DB_URL,<<"http://localhost:7080/dbs/testdb">>).


%% API
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2
]).

-export([
  attachment_doc/1,
  binary_attachment/1
]).

all() ->
  [
    attachment_doc,
    binary_attachment
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_http),
  {ok, _} = application:ensure_all_started(barrel_store),
  {ok, _} = application:ensure_all_started(barrel_httpc),
  Config.

init_per_testcase(_, Config) ->
  _ = barrel_httpc:create_database(?DB_URL),
  {ok, Conn} = barrel_httpc:connect(?DB_URL),
  [{db, Conn} | Config].

end_per_testcase(_, _Config) ->
  _ = barrel_httpc:delete_database(?DB_URL),
  ok.

end_per_suite(Config) ->
  Config.


db(Config) -> proplists:get_value(db, Config).


attachment_doc(Config) ->
  DocId = <<"a">>,
  Doc = #{ <<"id">> => DocId, <<"v">> => 1},
  {ok, <<"a">>, R1} = barrel_httpc:put(db(Config) , Doc, []),
  AttId = <<"myattachement">>,
  AttDescription = #{
    <<"id">> => AttId,
    <<"content_type">> => <<"image/png">>,
    <<"link">> => <<"http://somehost.com/cat.png">>
  },
  
  {ok, DocId, R2} = barrel_httpc_attachments:attach(db(Config) , DocId, AttDescription, [{rev, R1}]),
  {ok, AttDescription} = barrel_httpc_attachments:get_attachment(db(Config) , DocId, <<"myattachement">>, []),
  [AttDescription] = barrel_httpc_attachments:attachments(db(Config) , DocId, []),
  
  AttDescription2 = AttDescription#{<<"link">> => <<"http://anotherhost.com/panther.png">>},
  {error, attachment_conflict} = barrel_httpc_attachments:attach(db(Config) , DocId, AttDescription2, [{rev, R2}]),
  {ok, DocId, R3} = barrel_httpc_attachments:replace_attachment(db(Config) , DocId, AttId, AttDescription2, [{rev, R2}]),
  [AttDescription2] = barrel_httpc_attachments:attachments(db(Config) , DocId, []),
  {ok, DocId, _} = barrel_httpc_attachments:delete_attachment(db(Config) , DocId, AttId, [{rev, R3}]),
  [] = barrel_httpc_attachments:attachments(db(Config) , DocId, []),
  ok.

binary_attachment(Config) ->
  DocId = <<"a">>,
  Doc = #{ <<"id">> => DocId, <<"v">> => 1},
  {ok, <<"a">>, R1} = barrel_httpc:put(db(Config) , Doc, []),
  AttId = <<"myattachement">>,
  AttDescription = #{
    <<"id">> => AttId,
    <<"content_type">> => <<"image/png">>
  },
  Blob = <<"blobdata">>,
  
  {ok, DocId, R2} = barrel_httpc_attachments:attach(db(Config) , DocId, AttDescription, Blob, [{rev, R1}]),
  {ok, Blob} = barrel_httpc_attachments:get_attachment_binary(db(Config) , DocId, AttId, [{rev, R2}]),
  
  Blob2 = <<"anotherblobdata">>,
  {ok, DocId, R3} = barrel_httpc_attachments:replace_attachment_binary(db(Config) , DocId, AttId, Blob2, [{rev, R2}]),
  {ok, Blob2} = barrel_httpc_attachments:get_attachment_binary(db(Config) , DocId, AttId, [{rev, R3}]),
  ok.
