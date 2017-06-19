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
-define(DB,<<"testdb">>).


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
  binary_attachment/1,
  atomic_attachment/1,
  atomic_erlang_term_attachment/1,
  attachment_parsing/1
]).

all() ->
  [
   attachment_doc,
   binary_attachment,
   atomic_attachment,
   atomic_erlang_term_attachment,
   attachment_parsing
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_rest),
  Config.

init_per_testcase(_, Config) ->
  _ = barrel_httpc:create_database(?DB_URL),
  {ok, Conn} = barrel_httpc:connect(?DB_URL),
  [{db_url, Conn}, {db, ?DB} | Config].

end_per_testcase(_, _Config) ->
  _ = barrel_httpc:delete_database(?DB_URL),
  ok.

end_per_suite(Config) ->
  _ = application:stop(barrel_rest),
  Config.


db(Config) -> proplists:get_value(db_url, Config).
localdb(Config) -> proplists:get_value(db, Config).


attachment_doc(Config) ->
  DocId = <<"a">>,
  Doc = #{ <<"id">> => DocId, <<"v">> => 1},
  {ok, <<"a">>, R1} = barrel_httpc:post(db(Config) , Doc, []),
  AttId = <<"myattachement">>,
  AttDescription = #{
    <<"id">> => AttId,
    <<"content-type">> => <<"image/png">>,
    <<"link">> => <<"http://somehost.com/cat.png">>
  },

  {ok, DocId, R2} = barrel_httpc_attachments:attach(db(Config) , DocId, AttDescription, [{rev, R1}]),
  {ok, Doc, _} = barrel_httpc:get(db(Config), DocId, []),
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
  {ok, <<"a">>, R1} = barrel_httpc:post(db(Config) , Doc, []),
  AttId = <<"myattachement">>,
  AttDescription = #{
    <<"id">> => AttId,
    <<"content-type">> => <<"image/png">>
  },
  Blob = <<"blobdata">>,

  {ok, DocId, R2} = barrel_httpc_attachments:attach(db(Config) , DocId, AttDescription, Blob, [{rev, R1}]),
  {ok, Blob} = barrel_httpc_attachments:get_attachment_binary(db(Config) , DocId, AttId, [{rev, R2}]),

  Blob2 = <<"anotherblobdata">>,
  {ok, DocId, R3} = barrel_httpc_attachments:replace_attachment_binary(db(Config) , DocId, AttId, Blob2, [{rev, R2}]),
  {ok, Blob2} = barrel_httpc_attachments:get_attachment_binary(db(Config) , DocId, AttId, [{rev, R3}]),
  ok.

atomic_attachment(Config) ->
  DocId = <<"a">>,
  Doc = #{ <<"id">> => DocId, <<"v">> => 1},
  AttId = <<"myattachement">>,
  Blob = <<"blobdata">>,
  Attachments = [#{<<"id">> => AttId,
                   <<"blob">> => Blob},
                  #{<<"id">> => <<"2">>, <<"blob">> => <<"2">>}],

  %% store a document with attachments
  {ok, <<"a">>, R1} = barrel_httpc:post(db(Config), Doc, Attachments, []),

  %% the document is stored with attachments in prop _attachments
  {ok, StoredDoc, _} = barrel:get(localdb(Config), DocId, []),
  B64 = base64:encode(Blob),
  #{<<"_attachments">> :=
      [#{<<"id">> := AttId,
         <<"content-type">> := <<"application/octet-stream">>,
         <<"content-length">> := 8,
         <<"blob">> := B64}|_]} = StoredDoc,

  %% httpc:get does not return attachments by defautl
  {ok, Doc, #{<<"rev">> := R1}} = barrel_httpc:get(db(Config), DocId, []),

  %% ask httpc:get to retrive the attachments with options {attachments, all}
  {ok, Doc, [A1,A2], _} = barrel_httpc:get(db(Config), DocId,
                                        [{attachments, all}]),
  #{<<"id">> := AttId,
    <<"content-type">> := <<"application/octet-stream">>,
    <<"content-length">> := 8,
    <<"blob">> := Blob} = A1,
  #{<<"id">> := <<"2">>,
    <<"content-type">> := <<"application/octet-stream">>,
    <<"content-length">> := 1,
    <<"blob">> := <<"2">>} = A2,

  %% update the attachments
  Attachments2 = [#{<<"id">> => AttId,
                    <<"blob">> => Blob},
                  #{<<"id">> => <<"2">>, <<"blob">> => <<"3">>}],
  {ok, <<"a">>, _R2} = barrel_httpc:put(db(Config), Doc, Attachments2, []),
  {ok, Doc, [A1,A3], _} = barrel_httpc:get(db(Config), DocId,
                                           [{attachments, all}]),
  #{<<"id">> := <<"2">>,
    <<"content-type">> := <<"application/octet-stream">>,
    <<"content-length">> := 1,
    <<"blob">> := <<"3">>} = A3,

  %% delete all attachments
  {ok, <<"a">>, _} = barrel_httpc:put(db(Config), Doc, [], []),
  {ok, Doc, [], _} = barrel_httpc:get(db(Config), DocId, [{attachments, all}]),
  ok.

atomic_erlang_term_attachment(Config) ->
  DocId = <<"test_document">>,
  Doc = #{<<"id">> => DocId, <<"v">> => 1},
  AttId = <<"myattachement">>,
  Term = {atuple, [a, list], #{a => "map", <<"with">> => <<"binary">>}},

  %% bad content type
  BadContentType = [#{<<"id">> => AttId,
                      <<"content-type">> => <<"application/erlang">>,
                      <<"blob">> => <<"somebinary">>}],
  {error, {erlang_term_expected, AttId}} = barrel_httpc:post(db(Config), Doc, BadContentType, []),

  %% content-type not given for erlang term
  WithoutContentType = [#{<<"id">> => AttId,
                          <<"blob">> => Term}],
  {ok, DocId, _} = barrel_httpc:post(db(Config), Doc, WithoutContentType, []),

  %% retrieve decoded attachment
  {ok, Doc, [A], _} = barrel_httpc:get(db(Config), DocId,
                                       [{attachments, all}]),
		lager:warning("Got Document ~p", [A]),
  #{<<"id">> := AttId,
    <<"content-type">> := <<"application/erlang">>,
    <<"content-length">> := Len,
    <<"blob">> := Term} = A,
	true = lists:member(Len, [60,64]),
  ok.

attachment_parsing(Config) ->
  DocId = <<"a">>,
  Doc = #{ <<"id">> => DocId, <<"v">> => 1},
  AttId = <<"myattachement">>,
  Blob = <<"blobdata">>,
  Attachments = [#{<<"id">> => AttId,
                   <<"blob">> => Blob}],

  {ok, <<"a">>, R1} = barrel_httpc:post(db(Config), Doc, Attachments, []),

  {ok, Doc, #{<<"rev">> := R1}} = barrel_httpc:get(db(Config), DocId, []),
  {ok, NoParsing, #{<<"rev">> := R1}} =
    barrel_httpc:get(db(Config), DocId, [{attachments_parsing, false}]),
  #{<<"_attachments">> := EncodedAttachments,
    <<"id">> := DocId,
    <<"v">> := 1} = NoParsing,
  [#{<<"id">> := AttId}] = EncodedAttachments,
  ok.
