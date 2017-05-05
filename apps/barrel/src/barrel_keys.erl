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
  res_key/1
]).

-export([
  forward_path_key/2,
  reverse_path_key/2,
  encode_path_forward/2,
  encode_path_reverse/2
]).

prefix(db_meta) ->  << 0, 0, 0 >>;
prefix(doc) ->  <<  0, 50, 0 >>;
prefix(seq) ->  <<  0, 100, 0>>;
prefix(res) ->  <<  0, 200, 0>>;
prefix(sys_doc) ->  << 0, 300, 0 >>;
prefix(idx_forward_path) ->  <<  0, 410, 0 >>;
prefix(idx_reverse_path) ->  <<  0, 420, 0 >>;
prefix(idx_snapshot) -> << 0, 400, 0 >>;
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

forward_path_key(Path, Seq) ->
  barrel_encoding:encode_varint_ascending(
    encode_path_forward(prefix(idx_forward_path), Path),
    Seq
   ).

reverse_path_key(Path, Seq) ->
  barrel_encoding:encode_varint_ascending(
    encode_path_reverse(prefix(idx_reverse_path), Path),
    Seq
   ).

encode_path_forward(Prefix, [P0]) ->
  enc_1(Prefix, P0);
encode_path_forward(Prefix, [P0, P1]) ->
  enc_1(enc_1(Prefix, P0), P1);
encode_path_forward(Prefix, [P0, P1, P2]) ->
  enc_2(enc_1(enc_1(Prefix, P0), P1), P2).

encode_path_reverse(Prefix, [P0]) ->
  enc_1(Prefix, P0);
encode_path_reverse(Prefix, [P0, P1]) ->
  enc_1(enc_1(Prefix, P1), P0);
encode_path_reverse(Prefix, [P0, P1, P2]) ->
  enc_1(enc_1(enc_2(Prefix, P2), P1), P0).

enc_1(B, P) when is_integer(P) ->
   barrel_encoding:encode_varint_ascending(B, P);
enc_1(B, P) when is_float(P) ->
   barrel_encoding:encode_float_ascending(B, P);
enc_1(B, P)  ->
  barrel_encoding:encode_varint_ascending(B, erlang:phash2(P)).

enc_2(B, P) when is_binary(P) ->
  barrel_encoding:encode_binary_ascending(B, P);
enc_2(B, P) when is_integer(P) ->
  barrel_encoding:encode_varint_ascending(B, P);
enc_2(B, P) when is_float(P) ->
  barrel_encoding:encode_float_ascending(B, P);
enc_2(B, false) ->
  barrel_encoding:encode_literal_ascending(B, false);
enc_2(B, null) ->
  barrel_encoding:encode_literal_ascending(B, null);
enc_2(B, true) ->
  barrel_encoding:encode_literal_ascending(B, true).
