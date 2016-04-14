-type trie_node_id() :: binary() | atom().

-record(trie_node, {
  node_id         :: trie_node_id(),
  edge_count = 0  :: non_neg_integer(),
  topic           :: binary() | undefined,
  flags           :: [retained | static]
}).

-record(trie_edge, {
  node_id        :: trie_node_id(),
  word           :: binary() | atom()
}).

-record(trie, {
  edge          :: #trie_edge{},
  node_id       :: trie_node_id()
}).

