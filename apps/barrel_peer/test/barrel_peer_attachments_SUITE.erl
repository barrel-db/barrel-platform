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

-module(barrel_peer_attachments_SUITE).
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
  binary_attachment/1,
  atomic_attachment/1,
  atomic_erlang_term_attachment/1
]).

all() ->
  [
    attachment_doc,
    binary_attachment,
    atomic_attachment,
    atomic_erlang_term_attachment
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(barrel_http),
  {ok, _} = application:ensure_all_started(barrel),
  {ok, _} = application:ensure_all_started(barrel_httpc),
  {ok, _} = application:ensure_all_started(barrel_peer),
  Config.

init_per_testcase(_, Config) ->
  _ = barrel_peer:create_database(?DB_URL),
  {ok, Conn} = barrel_peer:connect(?DB_URL),
  [{db, Conn} | Config].

end_per_testcase(_, _Config) ->
  _ = barrel_peer:delete_database(?DB_URL),
  ok.

end_per_suite(Config) ->
  Config.


db(Config) -> proplists:get_value(db, Config).


attachment_doc(Config) ->
  DocId = <<"a">>,
  Doc = #{ <<"id">> => DocId, <<"v">> => 1},
  {ok, <<"a">>, R1} = barrel_peer:post(db(Config) , Doc, []),
  AttId = <<"myattachement">>,
  AttDescription = #{
    <<"id">> => AttId,
    <<"content_type">> => <<"image/png">>,
    <<"link">> => <<"http://somehost.com/cat.png">>
  },

  {ok, DocId, R2} = barrel_peer:attach(db(Config) , DocId, AttDescription, [{rev, R1}]),
  {ok, AttDescription} = barrel_peer:get_attachment(db(Config) , DocId, <<"myattachement">>, []),
  [AttDescription] = barrel_peer:attachments(db(Config) , DocId, []),

  AttDescription2 = AttDescription#{<<"link">> => <<"http://anotherhost.com/panther.png">>},
  {error, attachment_conflict} = barrel_peer:attach(db(Config) , DocId, AttDescription2, [{rev, R2}]),
  {ok, DocId, R3} = barrel_peer:replace_attachment(db(Config) , DocId, AttId, AttDescription2, [{rev, R2}]),
  [AttDescription2] = barrel_peer:attachments(db(Config) , DocId, []),
  {ok, DocId, _} = barrel_peer:delete_attachment(db(Config) , DocId, AttId, [{rev, R3}]),
  [] = barrel_peer:attachments(db(Config) , DocId, []),
  ok.

binary_attachment(Config) ->
  DocId = <<"a">>,
  Doc = #{ <<"id">> => DocId, <<"v">> => 1},
  {ok, <<"a">>, R1} = barrel_peer:post(db(Config) , Doc, []),
  AttId = <<"myattachement">>,
  AttDescription = #{
    <<"id">> => AttId,
    <<"content_type">> => <<"image/png">>
  },
  Blob = <<"blobdata">>,

  {ok, DocId, R2} = barrel_peer:attach(db(Config) , DocId, AttDescription, Blob, [{rev, R1}]),
  {ok, Blob} = barrel_peer:get_attachment_binary(db(Config) , DocId, AttId, [{rev, R2}]),

  Blob2 = <<"anotherblobdata">>,
  {ok, DocId, R3} = barrel_peer:replace_attachment_binary(db(Config) , DocId, AttId, Blob2, [{rev, R2}]),
  {ok, Blob2} = barrel_peer:get_attachment_binary(db(Config) , DocId, AttId, [{rev, R3}]),
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
  {ok, <<"a">>, R1} = barrel_peer:post(db(Config), Doc, Attachments, []),

  %% get does not return attachments by defautl
  {ok, Doc, #{<<"rev">> := R1}} = barrel_peer:get(db(Config), DocId, []),

  %% ask get to retrive the attachments with options {attachments, all}
  {ok, Doc, [A1,A2], _} = barrel_peer:get(db(Config), DocId,
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
  {ok, <<"a">>, _R2} = barrel_peer:put(db(Config), Doc, Attachments2, []),
  {ok, Doc, [A1,A3], _} = barrel_peer:get(db(Config), DocId,
                                           [{attachments, all}]),
  #{<<"id">> := <<"2">>,
    <<"content-type">> := <<"application/octet-stream">>,
    <<"content-length">> := 1,
    <<"blob">> := <<"3">>} = A3,

  %% delete all attachments
  {ok, <<"a">>, _} = barrel_peer:put(db(Config), Doc, [], []),
  {ok, Doc, [], _} = barrel_peer:get(db(Config), DocId, [{attachments, all}]),
  ok.

atomic_erlang_term_attachment(Config) ->
  DocId = <<"a">>,
  Doc = #{<<"id">> => DocId, <<"v">> => 1},
  AttId = <<"myattachement">>,
  Term = {atuple, [a, list], #{a => "map", <<"with">> => <<"binary">>}},

  %% bad content type
  BadContentType = [#{<<"id">> => AttId,
                      <<"content-type">> => <<"application/erlang">>,
                      <<"blob">> => <<"somebinary">>}],
  {error, {erlang_term_expected, AttId}} = barrel_peer:post(db(Config), Doc, BadContentType, []),

  %% content-type not given for erlang term
  WithoutContentType = [#{<<"id">> => AttId,
                          <<"blob">> => Term}],
  {ok, <<"a">>, _} = barrel_peer:post(db(Config), Doc, WithoutContentType, []),

  %% retrieve decoded attachment
  {ok, Doc, [A], _} = barrel_peer:get(db(Config), DocId,
                                       [{attachments, all}]),
  #{<<"id">> := AttId,
    <<"content-type">> := <<"application/erlang">>,
    <<"content-length">> := 64,
    <<"blob">> := Term} = A,
  ok.
