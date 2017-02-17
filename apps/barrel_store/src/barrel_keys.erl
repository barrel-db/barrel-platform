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
  prefix/1,
  db_meta_key/1,
  doc_key/1,
  seq_key/1,
  sys_key/1,
  rev_key/2,
  res_key/1,
  idx_forward_path_key/1,
  idx_last_doc_key/1,
  idx_reverse_path_key/1
]).

prefix(db_meta) ->  << 0, 0, 0 >>;
prefix(doc) ->  <<  0, 50, 0 >>;
prefix(seq) ->  <<  0, 100, 0>>;
prefix(res) ->  <<  0, 200, 0>>;
prefix(sys_doc) ->  << 0, 300, 0 >>;
prefix(idx_last_doc) ->  <<  0, 400, 0 >>;
prefix(idx_forward_path) ->  <<  0, 410, 0 >>;
prefix(idx_reverse_path) ->  <<  0, 420, 0 >>;
prefix(_) ->  erlang:error(badarg).

%% metadata keys

%% db keys

db_meta_key(Meta) -> << (prefix(db_meta))/binary, (barrel_lib:to_binary(Meta))/binary >>.

doc_key(DocId) ->  << (prefix(doc))/binary,  DocId/binary >>.

seq_key(Seq) -> << (prefix(seq))/binary, Seq:32>>.

sys_key(DocId) -> << (prefix(sys_doc))/binary,  DocId/binary>>.

rev_key(DocId, Rev) -> << DocId/binary, 1, Rev/binary >>.

res_key(RId) -> << (prefix(res))/binary, RId:32>>.

%% index keys

idx_last_doc_key(RID) -> << (prefix(idx_last_doc))/binary, (barrel_lib:to_binary(RID))/binary  >>.

idx_forward_path_key(Path) -> << (prefix(idx_forward_path))/binary, (encode_path(Path))/binary >>.

idx_reverse_path_key(Path) -> << (prefix(idx_reverse_path))/binary, (encode_path(Path))/binary >>.


encode_path(Parts) ->
  barrel_lib:binary_join(
    [enc(P) || P <- Parts],
    <<"/">>
  ).

enc(Part) when is_binary(Part) ->
  Len = byte_size(Part),
  if
    Len > 100 ->
      << P1:100/binary, _/binary>> = Part,
      sext:encode(P1);
    true ->
      sext:encode(Part)
  end;
enc(Part) ->
  sext:encode(Part).