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

-module(barrel_attachments_SUITE).
-author("Benoit Chesneau").

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
  {ok, _} = application:ensure_all_started(barrel),
  Config.


init_per_testcase(_, Config) ->
  ok = barrel_db:start(<<"testdb">>, barrel_test_rocksdb),
  [{db, <<"testdb">>}|Config].

end_per_testcase(_, _Config) ->
  barrel_db:clean(<<"testdb">>),
  ok.

end_per_suite(Config) ->
  erocksdb:destroy("testdb", []),
  Config.

attachment_doc(Config) ->
  Db = proplists:get_value(db, Config),
  DocId = <<"a">>,
  Doc = #{ <<"_id">> => DocId, <<"v">> => 1},
  {ok, <<"a">>, R1} = barrel_db:put(Db, DocId, Doc, []),
  AttId = <<"myattachement">>,
  AttDescription = #{
    <<"id">> => AttId,
    <<"content_type">> => <<"image/png">>,
    <<"link">> => <<"http://somehost.com/cat.png">>
   },

  {ok, DocId, R2} = barrel_attachments:attach(Db, DocId, AttDescription, [{db_version, R1}]),
  {ok, AttDescription} = barrel_attachments:get_attachment(Db, DocId, <<"myattachement">>, []),
  [AttDescription] = barrel_attachments:attachments(Db, DocId, []),

  AttDescription2 = AttDescription#{link => <<"http://anotherhost.com/panther.png">>},
  {error, attachment_conflict} = barrel_attachments:attach(Db, DocId, AttDescription2, [{db_version, R2}]),
  {ok, DocId, R3} = barrel_attachments:replace_attachment(Db, DocId, AttId, AttDescription2, [{db_version, R2}]),
  [AttDescription2] = barrel_attachments:attachments(Db, DocId, []),
  {ok, DocId, _} = barrel_attachments:delete_attachment(Db, DocId, AttId, [{db_version, R3}]),
  [] = barrel_attachments:attachments(Db, DocId, []),
  ok.

binary_attachment(Config) ->
  Db = proplists:get_value(db, Config),
  DocId = <<"a">>,
  Doc = #{ <<"_id">> => DocId, <<"v">> => 1},
  {ok, <<"a">>, R1} = barrel_db:put(Db, DocId, Doc, []),
  AttId = <<"myattachement">>,
  AttDescription = #{
    <<"id">> => AttId,
    <<"content_type">> => <<"image/png">>
   },
  Blob = <<"blobdata">>,

  {ok, DocId, R2} = barrel_attachments:attach(Db, DocId, AttDescription, Blob, [{db_version, R1}]),
  {ok, Blob} = barrel_attachments:get_attachment_binary(Db, DocId, AttId, [{db_version, R2}]),

  Blob2 = <<"anotherblobdata">>,
  {ok, DocId, R3} = barrel_attachments:replace_attachment_binary(Db, DocId, AttId, Blob2, [{db_version, R2}]),
  {ok, Blob2} = barrel_attachments:get_attachment_binary(Db, DocId, AttId, [{db_version, R3}]),
  ok.
