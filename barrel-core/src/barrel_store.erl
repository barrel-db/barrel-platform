
%% Created by benoitc on 13/09/16.

-module(barrel_store).
-author("Benoit Chesneau").
-behaviour(supervisor).

%% API
-export([
  open_db/3,
  clean_db/3,
  all_dbs/1,
  get_doc_info/3,
  write_doc/6,
  get_doc/7,
  fold_by_id/5,
  changes_since/6,
  last_update_seq/2,
  write_system_doc/4,
  read_system_doc/3,
  delete_system_doc/3,
  start_link/2
]).

-export([
  index_get_forward_path/3,
  index_get_reverse_path/3,
  index_get_last_doc/3,
  index_seq/2,
  update_index/7,
  find_by_key/6
]).

%% supervisor callback
-export([init/1]).


-define(DEFAULT_WORKERS, 100).


%%%=============================================================================
%%% Callbacks
%%%=============================================================================

-callback init(atom(), term()) -> {ok, any()}.

-callback open_db(term(), term(), list()) -> term().

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
  AccIn::any(), Opts::list(), State::any()) -> AccOut :: any().

-callback last_update_seq(DbId :: binary(), State :: any()) ->
  Seq :: integer | {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================

open_db(Store, Name, Options) ->
  call(Store, {open_db, Name, Options}).

clean_db(Store, Name, DbId) ->
  call(Store, {clean_db, Name, DbId}).

all_dbs(Store) ->
  call(Store, all_dbs).

get_doc_info(Store, DbId, DocId) ->
  call(Store, {get_doc_info, DbId, DocId}).

write_doc(Store, DbId, DocId, LastSeq, DocInfo, Body) ->
  call(Store, {write_doc, DbId, DocId, LastSeq, DocInfo, Body}).

get_doc(Store, DbId, DocId, Rev, WithHistory, MaxHistory, HistoryFrom) ->
  call(Store, {get_doc, DbId, DocId, Rev, WithHistory, MaxHistory, HistoryFrom}).


fold_by_id(Store, DbId, Fun, AccIn, Opts) ->
  call(Store, {fold_by_id, DbId, Fun, AccIn, Opts}).

changes_since(Store, DbId, Since, Fun, AccIn, Opts) ->
  call(Store, {changes_since, DbId, Since, Fun, AccIn, Opts}).

last_update_seq(Store, DbId) ->
  call(Store, {last_update_seq, DbId}).

write_system_doc(Store, DbId, DocId, Doc) ->
  call(Store, {write_system_doc, DbId, DocId, Doc}).

read_system_doc(Store, DbId, DocId) ->
  call(Store, {read_system_doc, DbId, DocId}).

delete_system_doc(Store, DbId, DocId) ->
  call(Store, {delete_system_doc, DbId, DocId}).

index_get_last_doc(Store, DbId, DocId) ->
  call(Store, {index_get_last_doc, DbId, DocId}).

index_seq(Store, DbId) ->
  call(Store, {index_seq, DbId}).

update_index(Store, DbId, ForwardOps, ReverseOps, DocId, Seq, FullPaths) ->
  call(Store, {update_index, DbId, ForwardOps, ReverseOps, DocId, Seq, FullPaths}).


index_get_reverse_path(Store, DbId, Path) ->
  call(Store, {index_get_reverse_path, DbId, Path}).

index_get_forward_path(Store, DbId, Path) ->
  call(Store, {index_get_forward_path, DbId, Path}).


find_by_key(Store, DbId, Path, Fun, AccIn, Opts) ->
  call(Store, {find_by_key, DbId, Path, Fun, AccIn, Opts}).

call(Store, Msg) ->
  try wpool:call(Store, Msg)
  catch
    _:Error -> {error, {store_error, {Store, Error}}}
  end.


start_link(Name, Config) ->
  supervisor:start_link(?MODULE, [Name, Config]).


init([Name, Config0]) ->
  {Specs0, Config1} = case maps:find(backend, Config0) of
                        {ok, BackendMod} ->
                          Backend = backend_name(Name),
                          BackendSpec =
                            #{id => Backend,
                              start => {BackendMod, start_link, [Backend, Name, Config0]},
                              restart => permanent,
                              shutdown => 5000,
                              type => worker,
                              modules => [BackendMod]
                            },
                          {[BackendSpec], Config0#{store_backend => Backend, store_name => Name}};
                        error ->
                          {[], Config0#{store_name => Name}}
                      end,
  StoreMod = maps:get(store, Config1),
  StoreSpec =
    #{id => Name,
      start => {barrel_store_pool, start_link, [Name, StoreMod, Config1]},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [StoreMod]
    },
  Specs = Specs0 ++ [StoreSpec],
  {ok, {{one_for_all, 5, 10}, Specs}}.

%%%=============================================================================
%%% Internal API
%%%=============================================================================
backend_name(Name) ->
  barrel_lib:to_atom(atom_to_list(Name) ++ "-backend").
