
%% Created by benoitc on 13/09/16.

-module(barrel_store).
-author("Benoit Chesneau").

%% API
-export([]).


%%%=============================================================================
%%% Callbacks
%%%=============================================================================

-callback init(atom(), term()) -> {ok, any()}.

-callback open_db(term(), term()) -> term().

-callback clean_db(term(), term(), term()) -> ok | {error, term()}.

-callback all_dbs(term()) -> list().

-callback get_doc_info(term(), binary(), binary()) -> {ok, map()} | {error, term()}.

-callback write_doc(term(), binary(), binary(), integer(), map(), map())
    -> {ok, integer()} | {error, term()}.

-callback get_doc(DbId :: binary(), DocId :: binary(), Rev :: barrel_db:rev(),
  History :: boolean(), MaxHistory::integer(), HistoryForm::list(), State :: any())
    -> {ok, Body :: map() } | {error, term()}.


-callback fold_by_id(DbId :: binary(), Fun :: fun(),
  AccIn::any(), FoldOpts :: list(), State::any()) -> AccOut :: any().

-callback changes_since(DbId :: binary(), Since :: integer(), Fun :: fun(),
  AccIn::any(), State::any()) -> AccOut :: any().

-callback last_update_seq(DbId :: binary(), State :: any()) ->
  Seq :: integer | {error, term()}.
