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
  , doc_from_obj/1]).

-include("barrel_common.hrl").

-export([id/1, rev/1, id_rev/1]).
-export([deleted/1]).

-type doc() :: map().
-type docid() :: binary().
-type revid() :: binary().

-export_types([docid/0, revid/0]).

%% TODO: normalize the body. Handle deleted apart ?
revid(Pos, Parent, Body0) ->
  Ctx0 = crypto:hash_init(md5),
  Body = maps:filter(fun
                       (<<"">>, _) -> false;
                       (<<"_deleted">> , _) -> true;
                       (<<"_attachments">>, _) -> true;
                       (<<"_", _/binary>>, _) -> false;
                       (_, _) -> true
                     end, Body0),
  BinPos = integer_to_binary(Pos),
  Ctx2 = lists:foldl(fun(V, C) ->
                         crypto:hash_update(C, V)
                     end, Ctx0, [BinPos, Parent, term_to_binary(Body)]),
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

parse_revisions(#{ <<"_revisions">> := Revisions}) ->
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

-spec id(doc()) -> docid().
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



%% @doc return a doc record from a objecy
%% this is used internally to handle docs
-spec doc_from_obj(Obj) -> Doc when
  Obj :: map(),
  Doc :: #doc{}.
doc_from_obj(Obj) when is_map(Obj) ->
  %% normalize the body
  Body = maps:filter(fun
                       (<<"">>, _) -> false;
                       (<<"_deleted">>, _) -> true;
                       (<<"_attachments">>, _) -> true;
                       (<<"_", _/binary>>, _) -> false;
                       (_, _) -> true
                     end, Obj),
  Id = case maps:find(<<"id">>, Obj) of
            {ok, Id0} -> Id0;
            error -> barrel_lib:uniqid()
          end,

  % TODO: maybe directly handle json object coming
  % from HTTP from revs? (to parse history)
  Revs = case maps:find(<<"_rev">>, Obj) of
           {ok, Rev} -> [Rev];
           error -> [<<>>]
         end,
  
  Deleted = deleted(Obj),
  #doc{ id = Id,
        revs = Revs,
        body = Body,
        deleted = Deleted }.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

revid_test() ->
	Rev = revid(10, <<"9-test">>, #{<<"id">> => <<"test">>,
													<<"hello">> => <<"yo">>}),
	?assertEqual(<<"10-88197ae5119917ebb5bda615a4cab3e8">>, Rev),
  Rev = revid(10, <<"9-test">>, #{<<"id">> => <<"test">>,
    <<"hello">> => <<"yo">>, <<"_hello">> => ok}),
  ?assertEqual(<<"10-88197ae5119917ebb5bda615a4cab3e8">>, Rev),
  Rev2 = revid(10, <<"9-test">>, #{<<"id">> => <<"test">>,
    <<"hello">> => <<"yo">>, <<"_deleted">> => true }),
  ?assertEqual(<<"10-0a824dd48f4e552ba168a8dd6a2cc98c">>, Rev2).

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
    <<"_revisions">> => #{
      <<"start">> => 2,
      <<"ids">> => [<<"b19b17d048f082aa4a62c8da1262a33a">>, <<"5c1f0a9d721f0731a46645d18b763047">>]
    }
  },
  ?assertEqual(Revs, parse_revisions(Body)).


doc_from_obj_test() ->
  Obj = #{ <<"id">> => <<"a">>,
           <<"_rev">> => <<"rev">>,
           <<"_deleted">> => true,
           <<"_rid">> => <<"rid">>,
           <<"field">> => <<"f">>},
  Doc =
    #doc{id = <<"a">>,
         revs = [<<"rev">>],
         body = #{ <<"id">> => <<"a">>, <<"_deleted">> => true, <<"field">> => <<"f">> },
         deleted = true},
  
  ?assertEqual(Doc, doc_from_obj(Obj)).
-endif.
