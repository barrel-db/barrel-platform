%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2016 16:07
%%%-------------------------------------------------------------------
-module(barrel_trie).
-author("benoitc").

-export([new/0, new/1, insert/2, match/2, delete/1, delete/2]).
-export([lookup/2]).
-export([validate/1, is_match/2,  is_wildcard/1]).

-include("barrel_trie.hrl").

-record(trie_tree, {ntab = notable :: ets:tab(),
  ttab = notable :: ets:tab()}).


-type topic() :: binary().
-type word()   :: '' | '+' | '#' | binary().

-type words()  :: list(word()).

-opaque trie() :: #trie_tree{}.
-export_type([trie/0]).

-define(MAX_TOPIC_LEN, 4096).

%% @doc create a new TRIE relative to the current process. The trie can be
%% shared between reader processes.
-spec new() -> trie().
new() -> new(protected).

-spec new(protected | private) -> trie().
new(Access) ->
  case lists:member(Access, [protected, private, public]) of
    true ->
      T = ets:new(trie,  [ordered_set, Access,
        {keypos, #trie.edge}]),
      N = ets:new(trie_node, [ordered_set, Access,
        {keypos, #trie_node.node_id}]),
      #trie_tree{ntab=N, ttab=T};
    false ->
      erlang:error(badarg)
  end.

%% @doc insert a new topic to the tie
-spec insert(trie(), binary()) -> ok.
insert(T, Topic) when is_binary(Topic) ->
  NT = T#trie_tree.ntab,
  case ets:lookup(NT, Topic) of
    [#trie_node{topic=Topic}] ->
      ok;
    [TrieNode=#trie_node{topic=undefined}] ->
      true = ets:insert(NT, TrieNode#trie_node{topic=Topic}),
      ok;
    [] ->
      Fun = fun(Triple) ->  add_path(Triple, T) end,
      lists:foreach(Fun, triples(Topic)),
      true = ets:insert(NT, #trie_node{node_id=Topic, topic=Topic}),
      ok
  end;
insert(_, _) ->
  erlang:error(badarg).

%% @doc %% @doc Find trie nodes that match topic
-spec match(trie(), binary()) -> [binary()].
match(T, Topic) when is_binary(Topic) ->
  TrieNodes = match_node(root, words(Topic), T),
  [Name || #trie_node{topic=Name} <- TrieNodes, Name =/= undefined].


%% @doc delete the entire trie.
-spec delete(trie()) -> ok.
delete(#trie_tree{ntab=NT, ttab=TT}) ->
  true = ets:delete(NT),
  true = ets:delete(TT),
  ok.

%% @doc delete a topic from the trie
-spec delete(trie(), binary()) -> ok.
delete(T, Topic) when is_binary(Topic) ->
  NT = T#trie_tree.ntab,
  case ets:lookup(NT, Topic) of
    [#trie_node{edge_count=0}] ->
      ets:delete(NT, Topic),
      delete_path(lists:reverse(triples(Topic)), T),
      ok;
    [TrieNode] ->
      ets:insert(NT, TrieNode#trie_node{topic=undefined}),
      ok;
    [] ->
      ok
  end.

%% @doc lookup a trie node.
-spec lookup(trie(), binary()) -> [#trie_node{}].
lookup(T, NodeId) ->
  ets:lookup(T#trie_tree.ntab, NodeId).

%% @doc Validate Topic
-spec(validate({name | filter, topic()}) -> boolean()).
validate({_, <<>>}) ->
  false;
validate({_, Topic}) when is_binary(Topic) and (size(Topic) > ?MAX_TOPIC_LEN) ->
  false;
validate({filter, Topic}) when is_binary(Topic) ->
  validate2(words(Topic));
validate({name, Topic}) when is_binary(Topic) ->
  Words = words(Topic),
  validate2(Words) and (not is_wildcard(Words)).

validate2([]) ->
  true;
validate2(['#']) -> % end with '#'
  true;
validate2(['#'|Words]) when length(Words) > 0 ->
  false;
validate2([''|Words]) ->
  validate2(Words);
validate2(['+'|Words]) ->
  validate2(Words);
validate2([W|Words]) ->
  case validate3(W) of
    true -> validate2(Words);
    false -> false
  end.

validate3(<<>>) ->
  true;
validate3(<<C/utf8, _Rest/binary>>) when C == $#; C == $+; C == 0 ->
  false;
validate3(<<_/utf8, Rest/binary>>) ->
  validate3(Rest).


%% @doc Is wildcard topic?
-spec(is_wildcard(topic() | words()) -> true | false).
is_wildcard(Topic) when is_binary(Topic) ->
  is_wildcard(words(Topic));
is_wildcard([]) ->
  false;
is_wildcard(['#'|_]) ->
  true;
is_wildcard(['+'|_]) ->
  true;
is_wildcard([_H|T]) ->
  is_wildcard(T).

%% @doc Match Topic name with filter
-spec(is_match(Name, Filter) -> boolean() when
  Name    :: topic() | words(),
  Filter  :: topic() | words()).
is_match(Name, Filter) when is_binary(Name) and is_binary(Filter) ->
  is_match(words(Name), words(Filter));
is_match([], []) ->
  true;
is_match([H|T1], [H|T2]) ->
  match(T1, T2);
is_match([<<$$, _/binary>>|_], ['+'|_]) ->
  false;
is_match([_H|T1], ['+'|T2]) ->
  is_match(T1, T2);
is_match([<<$$, _/binary>>|_], ['#']) ->
  false;
is_match(_, ['#']) ->
  true;
is_match([_H1|_], [_H2|_]) ->
  false;
is_match([_H1|_], []) ->
  false;
is_match([], [_H|_T2]) ->
  false.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

%% @private
%% @doc Add path to trie tree.
add_path({Node, Word, Child}, T) ->
  NT = T#trie_tree.ntab,
  TT = T#trie_tree.ttab,

  Edge = #trie_edge{node_id=Node, word=Word},
  case ets:lookup(NT, Node) of
    [TrieNode = #trie_node{edge_count=Count}] ->
      case ets:lookup(TT, Edge) of
        [] ->
          ets:insert(NT, TrieNode#trie_node{edge_count=Count+1}),
          ets:insert(TT, #trie{edge=Edge, node_id=Child});
        [_] ->
          ok
      end;
    [] ->
      ets:insert(NT, #trie_node{node_id=Node, edge_count=1}),
      ets:insert(TT, #trie{edge=Edge, node_id=Child})
  end.

%% @private
%% @doc Match node with word or '+'.
match_node(root, [<<"$SYS">>|Words], T) ->
  match_node(<<"$SYS">>, Words, T, []);

match_node(NodeId, Words, T) ->
  match_node(NodeId, Words, T, []).


match_node(NodeId, [], T, ResAcc) ->
  ets:lookup(T#trie_tree.ntab, NodeId) ++ 'match_#'(NodeId, ResAcc, T);

match_node(NodeId, [W|Words], T, ResAcc) ->
  TT = T#trie_tree.ttab,
  lists:foldl(fun(WArg, Acc) ->
    case ets:lookup(TT, #trie_edge{node_id=NodeId, word=WArg}) of
      [#trie{node_id=ChildId}] -> match_node(ChildId, Words, T, Acc);
      [] -> Acc
    end
              end, 'match_#'(NodeId, ResAcc, T), [W, '+']).

%% @private
%% @doc Match node with '#'.
'match_#'(NodeId, ResAcc, T) ->
  NT = T#trie_tree.ntab,
  TT = T#trie_tree.ttab,
  case ets:lookup(TT, #trie_edge{node_id=NodeId, word = '#'}) of
    [#trie{node_id=ChildId}] ->
      ets:lookup(NT, ChildId) ++ ResAcc;
    [] ->
      ResAcc
  end.

%% @private
%% @doc Delete paths from trie tree.
delete_path([], _T) ->
  ok;
delete_path([{NodeId, Word, _} | RestPath], T) ->
  NT = T#trie_tree.ntab,
  TT = T#trie_tree.ttab,

  ets:delete(TT, #trie_edge{node_id=NodeId, word=Word}),
  case ets:lookup(NT, NodeId) of
    [#trie_node{edge_count=1, topic=undefined}] ->
      ets:delete(NT, NodeId),
      delete_path(RestPath, T);
    [TrieNode=#trie_node{edge_count=1, topic=_}] ->
      ets:insert(NT, TrieNode#trie_node{edge_count=0});
    [TrieNode=#trie_node{edge_count=C}] ->
      ets:insert(NT, TrieNode#trie_node{edge_count=C-1});
    [] ->
      throw({notfound, NodeId})
  end.



join(root, W) ->
  bin(W);
join(Parent, W) ->
  <<(bin(Parent))/binary, $/, (bin(W))/binary>>.

bin('')  -> <<>>;
bin('+') -> <<"+">>;
bin('#') -> <<"#">>;
bin(B) when is_binary(B) -> B.

triples(Topic) when is_binary(Topic) ->
  triples(words(Topic), root, []).

triples([], _Parent, Acc) ->
  lists:reverse(Acc);

triples([W|Words], Parent, Acc) ->
  Node = join(Parent, W),
  triples(Words, Node, [{Parent, W, Node}|Acc]).

words(Topic) when is_binary(Topic) ->
  [word(W) || W <- binary:split(Topic, <<"/">>, [global])].

word(<<>>)    -> '';
word(<<"+">>) -> '+';
word(<<"#">>) -> '#';
word(Bin)     -> Bin.
