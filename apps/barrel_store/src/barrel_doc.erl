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

-module(barrel_doc).

-export([
  revid/3
  , parse_revision/1
  , encode_revisions/1
  , parse_revisions/1
  , trim_history/3
  , compare/2
  , make_doc/3]).



-export([id/1, rev/1, id_rev/1]).
-export([deleted/1]).

-type doc() :: map().
-type docid() :: binary().
-type revid() :: binary().

-export_types([docid/0, revid/0]).

-include("barrel_store.hrl").

%% @doc generate a unique revision id for a document.
%%
%% the algorithm to generate a revision ID is the following:
%%
%% 1) We first create a canonical form of the JSON body following the draft here:
%%    https://tools.ietf.org/html/draft-staykov-hu-json-canonical-form-00
%%
%%    Note: every keys at first level of the document object prefixed with "_" are removed
%%
%% 2) we create a SHA 256 hash by appending the following value in order:
%%      - Gen: the new generation of the client (integer encoded to json)
%%      - Parent Id: string
%%      - Deleted: true or false (boolean encoded as json)
%%      - Canonical Json
-spec revid(Pos, Parent, Doc) -> RevId when
  Pos :: non_neg_integer(),
  Parent :: binary(),
  Doc :: #doc{},
  RevId :: binary().
revid(Pos, Parent, Doc) ->
  Ctx0 = crypto:hash_init(sha256),
  CanonicalJson = jsone:encode(Doc#doc.body, [canonical_form]),
  BinPos = jsone:encode(Pos),
  BinDel = jsone:encode(Doc#doc.deleted),
  Ctx2 = lists:foldl(fun(V, C) ->
                         crypto:hash_update(C, V)
                     end, Ctx0, [BinPos, Parent, BinDel, CanonicalJson]),
  Digest = crypto:hash_final(Ctx2),
  << BinPos/binary, "-", (barrel_lib:to_hex(Digest))/binary >>.

parse_revision(<<"">>) -> {0, <<"">>};
parse_revision(Rev) when is_binary(Rev) ->
  case binary:split(Rev, <<"-">>) of
    [BinPos, Hash] -> {binary_to_integer(BinPos), Hash};
    _ -> error(bad_rev)
  end;
parse_revision(Rev) when is_list(Rev) -> parse_revision(list_to_binary(Rev));
parse_revision(Rev) -> error({bad_rev, Rev}).

parse_revisions(#{ <<"revisions">> := Revisions}) ->
  case Revisions of
    #{ <<"start">> := Start, <<"ids">> := Ids} ->
      {Revs, _} = lists:foldl(
        fun(Id, {Acc, I}) ->
          Acc2 = [<< (integer_to_binary(I))/binary,"-", Id/binary >> | Acc],
          {Acc2, I - 1}
        end, {[], Start}, Ids),
      lists:reverse(Revs);
    _ -> []

  end;
parse_revisions(#{<<"_rev">> := Rev}) -> [Rev];
parse_revisions(_) -> [].

encode_revisions(Revs) ->
  [Oldest | _] = Revs,
  {Start, _} = barrel_doc:parse_revision(Oldest),
  Digests = lists:foldl(fun(Rev, Acc) ->
      {_, Digest}=parse_revision(Rev),
      [Digest | Acc]
    end, [], Revs),
  #{<<"start">> => Start, <<"ids">> => lists:reverse(Digests)}.

trim_history(EncodedRevs, Ancestors, Limit) ->
  #{ <<"start">> := Start, <<"ids">> := Digests} = EncodedRevs,
  ADigests = array:from_list(Digests),
  {_, Limit2} = lists:foldl(
    fun(Ancestor, {Matched, Unmatched}) ->
      {Gen, Digest} = barrel_doc:parse_revision(Ancestor),
      Idx = Start - Gen,
      IsDigest = array:get(Idx, ADigests) =:= Digest,
      if
        Idx >= 0, Idx < Matched, IsDigest =:= true-> {Idx, Idx+1};
        true -> {Matched, Unmatched}
      end
    end, {length(Digests), Limit}, Ancestors),
  EncodedRevs#{ <<"ids">> => (lists:sublist(Digests, Limit2)) }.

compare(RevA, RevB) ->
  RevTupleA = parse_revision(RevA),
  RevTupleB = parse_revision(RevB),
  compare1(RevTupleA, RevTupleB).

compare1(RevA, RevB) when RevA > RevB -> 1;
compare1(RevA, RevB) when RevA < RevB -> -1;
compare1(_, _) -> 0.

-spec id(doc()) -> docid() | undefined.
id(#{<<"id">> := Id}) -> Id;
id(#{}) -> undefined;
id(_) -> erlang:error(bad_doc).

rev(#{<<"_rev">> := Rev}) -> Rev;
rev(#{}) -> <<>>;
rev(_) -> error(bad_doc).

-spec id_rev(doc()) -> {docid(), revid()}.
id_rev(#{<<"id">> := Id, <<"_rev">> := Rev}) -> {Id, Rev};
id_rev(#{<<"id">> := Id}) -> {Id, <<>>};
id_rev(#{<<"_rev">> := _Rev}) -> erlang:error(bad_doc);
id_rev(#{}) -> {undefined, <<>>};
id_rev(_) -> erlang:error(bad_doc).

deleted(#{ <<"_deleted">> := Del}) when is_boolean(Del) -> Del;
deleted(_) -> false.


%% @doc return a doc record from a objec
%% this is used internally to handle docs
-spec make_doc(Obj, Revs, Deleted) -> Doc when
    Obj :: map(),
    Revs :: [revid()],
    Deleted :: boolean(),
    Doc :: #doc{}.
make_doc(Obj, Revs, Deleted) when is_map(Obj) ->
  Id = case maps:find(<<"id">>, Obj) of
         {ok, Id0} -> Id0;
         error -> barrel_lib:uniqid()
       end,

  #doc{ id = Id,
        revs = Revs,
        body = Obj,
        deleted = Deleted }.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

revid_test() ->
  Doc = make_doc(#{<<"id">> => <<"test">>,
                   <<"hello">> => <<"yo">>}, <<>>, false),
	Rev = revid(10, <<"9-test">>, Doc),
	?assertEqual(<<"10-7a5c147096d3fdbd537847bfb8708646caedb1bab8deb07c7865157874ca91bf">>, Rev),
  Doc1 = make_doc(#{<<"hello">> => <<"yo">>,
                    <<"id">> => <<"test">>}, <<>>, false),
  ?assertEqual(Rev, revid(10, <<"9-test">>, Doc1)),
  Doc2 = make_doc(#{<<"id">> => <<"test">>,
                   <<"hello">> => <<"yo">>}, <<>>, true),
  Rev2 = revid(10, <<"9-test">>, Doc2),
  ?assertEqual(<<"10-5500c3bc2a38219e6b417aecb8061e39159ba7a462dbd1ad89e534f22a904d57">>, Rev2).

parse_test() ->
	Parsed = parse_revision(<<"10-2f25ea96da3fed514795b0ced028d58a">>),
  ?assertEqual({10, <<"2f25ea96da3fed514795b0ced028d58a">>}, Parsed).

compare_test() ->
  ?assertEqual(0, compare(<<"1-a">>, <<"1-a">>)),
  ?assertEqual(1, compare(<<"1-b">>, <<"1-a">>)),
  ?assertEqual(-1, compare(<<"1-a">>, <<"1-b">>)),
  ?assertEqual(1, compare(<<"2-a">>, <<"1-a">>)),
  ?assertEqual(1, compare(<<"2-b">>, <<"1-a">>)).

deleted_test() ->
  ?assertEqual(true, deleted(#{ <<"_deleted">> => true})),
  ?assertEqual(false, deleted(#{ <<"_deleted">> => false})).

encode_revisions_test() ->
  Revs = [<<"2-b19b17d048f082aa4a62c8da1262a33a">>,<<"1-5c1f0a9d721f0731a46645d18b763047">>],
  ?assertEqual(#{
    <<"start">> => 2,
    <<"ids">> => [<<"b19b17d048f082aa4a62c8da1262a33a">>, <<"5c1f0a9d721f0731a46645d18b763047">>]
  }, encode_revisions(Revs)).

parse_revisions_test() ->
  Revs = [<<"2-b19b17d048f082aa4a62c8da1262a33a">>,<<"1-5c1f0a9d721f0731a46645d18b763047">>],
  Body = #{
    <<"revisions">> => #{
      <<"start">> => 2,
      <<"ids">> => [<<"b19b17d048f082aa4a62c8da1262a33a">>, <<"5c1f0a9d721f0731a46645d18b763047">>]
    }
  },
  ?assertEqual(Revs, parse_revisions(Body)).


make_doc_test() ->
  Obj = #{ <<"id">> => <<"a">>,
           <<"field">> => <<"f">>},
  Rev = <<"rev">>,
  Deleted = true,
  Doc =
    #doc{id = <<"a">>,
         revs = [<<"rev">>],
         body = #{ <<"id">> => <<"a">>, <<"field">> => <<"f">> },
         deleted = true},

  ?assertEqual(Doc, make_doc(Obj, [Rev], Deleted)).
-endif.
