%% Copyright (c) 2017. Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(barrel_keys).
-author("benoitc").

%% API
-export([
  db_id/1,
  prefix/1,
  prefix/2,
  db_prefix/1,
  db_key/1,
  db_meta_key/2,
  doc_key/2,
  seq_key/2,
  sys_key/2,
  rev_key/3,
  idx_forward_path_key/2,
  idx_last_doc_key/2,
  idx_reverse_path_key/2
]).


db_id(DbName) -> << (barrel_lib:uniqid())/binary, "-", DbName/binary >>.

prefix(db) -> << 0, 0, 0, 100 >>;
prefix(_) -> erlang:error(badarg).

prefix(DbId, db_meta) when is_binary(DbId) ->  <<  DbId/binary, 0, 0 >>;
prefix(DbId, doc) when is_binary(DbId) ->  <<  DbId/binary, 0, 0, 50 >>;
prefix(DbId, seq) when is_binary(DbId) ->  <<  DbId/binary, 0, 0, 100>>;
prefix(DbId, sys_doc) when is_binary(DbId) ->  <<  DbId/binary, 0, 0, 200 >>;
prefix(DbId, idx_last_doc) when is_binary(DbId) ->  <<  DbId/binary, 0, 0, 400 >>;
prefix(DbId, idx_forward_path) when is_binary(DbId)->  <<  DbId/binary, 0, 0, 410, 0 >>;
prefix(DbId, idx_reverse_path) when is_binary(DbId)->  <<  DbId/binary, 0, 0, 420, 0 >>;
prefix(_, _) ->  erlang:error(badarg).

db_prefix(DbId) -> << DbId/binary, 0 >>.

%% metadata keys

db_key(DbName) when is_binary(DbName) -> << (prefix(db))/binary, DbName/binary >>.

%% db keys

db_meta_key(DbId, Meta) -> << (prefix(DbId, db_meta))/binary, (barrel_lib:to_binary(Meta))/binary >>.

doc_key(DbId, DocId) ->  << (prefix(DbId, doc))/binary,  DocId/binary >>.

seq_key(DbId, Seq) -> << (prefix(DbId, seq))/binary, Seq:32>>.

sys_key(DbId, DocId) -> << (prefix(DbId, sys_doc))/binary,  DocId/binary>>.

rev_key(DbId, DocId, Rev) -> << DbId/binary, DocId/binary, 1, Rev/binary >>.

%% index keys

idx_last_doc_key(DbId, DocId) -> << (prefix(DbId, idx_last_doc))/binary, DocId/binary >>.

idx_forward_path_key(DbId, Path) -> << (prefix(DbId, idx_forward_path))/binary, (encode_path(Path))/binary >>.

idx_reverse_path_key(DbId, Path) -> << (prefix(DbId, idx_reverse_path))/binary, (encode_path(Path))/binary >>.

encode_path(Path) -> encode_path(Path, <<>>).

encode_path([P | R], Acc) ->
  encode_path(R, << Acc/binary, (sext:encode(P))/binary, "/" >>);
encode_path([], Acc) ->
  Acc.