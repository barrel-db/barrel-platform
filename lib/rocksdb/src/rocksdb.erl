%%% -*- erlang -*-
%%
%% Copyright (c) 2012-2015 Rakuten, Inc.
%% Copyright (c) 2016 Benoit Chesneau
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


%% @doc Erlang Wrapper for RocksDB
-module(rocksdb).

-export([open/2, open_with_cf/3, close/1]).
-export([snapshot/1, release_snapshot/1]).
-export([list_column_families/2, create_column_family/3, drop_column_family/1]).
-export([put/4, put/5, delete/3, delete/4, write/3, get/3, get/4]).
-export([iterator/2, iterator/3, iterators/3, iterators/4, iterator_move/2, iterator_close/1]).
-export([fold/4, fold/5, fold_keys/4, fold_keys/5]).
-export([destroy/2, repair/2, is_empty/1]).
-export([checkpoint/2]).
-export([count/1, count/2, stats/1, stats/2, get_property/2, get_property/3]).

-export_type([db_handle/0,
        cf_handle/0,
        itr_handle/0,
        snapshot_handle/0,
        compression_type/0,
        compaction_style/0,
        access_hint/0,
        wal_recovery_mode/0]).

-on_load(init/0).

%% This cannot be a separate function. Code must be inline to trigger
%% Erlang compiler's use of optimized selective receive.
-define(WAIT_FOR_REPLY(Ref),
    receive {Ref, Reply} ->
        Reply
    end).

-spec init() -> ok | {error, any()}.
init() ->
  SoName = case code:priv_dir(?MODULE) of
         {error, bad_name} ->
           case code:which(?MODULE) of
             Filename when is_list(Filename) ->
               filename:join([filename:dirname(Filename),"../priv", "rocksdb"]);
             _ ->
               filename:join("../priv", "rocksdb")
           end;
         Dir ->
           filename:join(Dir, "rocksdb")
       end,
  erlang:load_nif(SoName, application:get_all_env(rocksdb)).

-record(db_path, {path        :: file:filename_all(),
          target_size :: non_neg_integer()}).

-record(cf_descriptor, {name    :: string(),
            options :: cf_options()}).

-type compression_type() :: snappy | zlib | bzip2 | lz4 | lz4h | none.
-type compaction_style() :: level | universal | fifo | none.
-type access_hint() :: normal | sequential | willneed | none.
-type wal_recovery_mode() :: tolerate_corrupted_tail_records |
               absolute_consistency |
               point_in_time_recovery |
               skip_any_corrupted_records.

-opaque db_handle() :: binary().
-opaque cf_handle() :: binary().
-opaque itr_handle() :: binary().
-opaque snapshot_handle() :: binary().

-type block_based_table_options() :: [{no_block_cache, boolean()} |
                    {block_size, pos_integer()} |
                    {block_cache_size, pos_integer()} |
                    {bloom_filter_policy, BitsPerKey :: pos_integer()} |
                    {format_version, 0 | 1 | 2} |
                    {skip_table_builder_flush, boolean()} |
                    {cache_index_and_filter_blocks, boolean()}].

-type cf_options() :: [{block_cache_size_mb_for_point_lookup, non_neg_integer()} |
             {memtable_memory_budget, pos_integer()} |
             {write_buffer_size,  pos_integer()} |
             {max_write_buffer_number,  pos_integer()} |
             {min_write_buffer_number_to_merge,  pos_integer()} |
             {compression,  compression_type()} |
             {num_levels,  pos_integer()} |
             {level0_file_num_compaction_trigger,  integer()} |
             {level0_slowdown_writes_trigger,  integer()} |
             {level0_stop_writes_trigger,  integer()} |
             {max_mem_compaction_level,  pos_integer()} |
             {target_file_size_base,  pos_integer()} |
             {target_file_size_multiplier,  pos_integer()} |
             {max_bytes_for_level_base,  pos_integer()} |
             {max_bytes_for_level_multiplier,  pos_integer()} |
             {max_compaction_bytes,  pos_integer()} |
             {soft_rate_limit,  float()} |
             {hard_rate_limit,  float()} |
             {arena_block_size,  integer()} |
             {disable_auto_compactions,  boolean()} |
             {purge_redundant_kvs_while_flush,  boolean()} |
             {compaction_style,  compaction_style()} |
             {verify_checksums_in_compaction,  boolean()} |
             {filter_deletes,  boolean()} |
             {max_sequential_skip_in_iterations,  pos_integer()} |
             {inplace_update_support,  boolean()} |
             {inplace_update_num_locks,  pos_integer()} |
             {table_factory_block_cache_size, pos_integer()} |
             {in_memory_mode, boolean()} |
             {block_based_table_options, block_based_table_options()}].

-type db_options() :: [{total_threads, pos_integer()} |
             {create_if_missing, boolean()} |
             {create_missing_column_families, boolean()} |
             {error_if_exists, boolean()} |
             {paranoid_checks, boolean()} |
             {max_open_files, integer()} |
             {max_total_wal_size, non_neg_integer()} |
             {disable_data_sync, boolean()} |
             {use_fsync, boolean()} |
             {db_paths, list(#db_path{})} |
             {db_log_dir, file:filename_all()} |
             {wal_dir, file:filename_all()} |
             {delete_obsolete_files_period_micros, pos_integer()} |
             {max_background_compactions, pos_integer()} |
             {max_background_flushes, pos_integer()} |
             {max_log_file_size, non_neg_integer()} |
             {log_file_time_to_roll, non_neg_integer()} |
             {keep_log_file_num, pos_integer()} |
             {max_manifest_file_size, pos_integer()} |
             {table_cache_numshardbits, pos_integer()} |
             {wal_ttl_seconds, non_neg_integer()} |
             {wal_size_limit_mb, non_neg_integer()} |
             {manifest_preallocation_size, pos_integer()} |
             {allow_os_buffer, boolean()} |
             {allow_mmap_reads, boolean()} |
             {allow_mmap_writes, boolean()} |
             {is_fd_close_on_exec, boolean()} |
             {skip_log_error_on_recovery, boolean()} |
             {stats_dump_period_sec, non_neg_integer()} |
             {advise_random_on_open, boolean()} |
             {access_hint, access_hint()} |
             {compaction_readahead_size, non_neg_integer()} |
             {use_adaptive_mutex, boolean()} |
             {bytes_per_sync, non_neg_integer()} |
             {skip_stats_update_on_db_open, boolean()} |
             {wal_recovery_mode, wal_recovery_mode()} |
             {allow_concurrent_memtable_write, boolean()} |
             {enable_write_thread_adaptive_yield, boolean()} |
             {in_memory, boolean()}].

-type read_options() :: [{verify_checksums, boolean()} |
             {fill_cache, boolean()} |
             {iterate_upper_bound, binary()} |
             {tailing, boolean()} |
             {total_order_seek, boolean()} |
             {snapshot, snapshot_handle()}].

-type write_options() :: [{sync, boolean()} |
              {disable_wal, boolean()} |
              {timeout_hint_us, non_neg_integer()} |
              {ignore_missing_column_families, boolean()}].

-type write_actions() :: [{put, Key::binary(), Value::binary()} |
              {put, ColumnFamilyHandle::cf_handle(), Key::binary(), Value::binary()} |
              {delete, Key::binary()} |
              {delete, ColumnFamilyHandle::cf_handle(), Key::binary()} |
              clear].

-type iterator_action() :: first | last | next | prev | binary().



%% @doc Open RocksDB with the defalut column family
-spec open(Name, DBOpts) -> Result when
  Name :: file:filename_all(),
  DBOpts :: db_options(),
  Result :: {ok, db_handle()} | {error, any()}.
open(Name, DBOpts) ->
  CallerRef = make_ref(),
  async_open(CallerRef, Name, DBOpts),
  ?WAIT_FOR_REPLY(CallerRef).

async_open(_CallerRef, _Name, _DBOpts) ->
  erlang:nif_error({error, not_loaded}).

%% @doc Open RocksDB with the specified column families
-spec(open_with_cf(Name, DBOpts, CFDescriptors) ->
       {ok, db_handle(), list(cf_handle())} | {error, any()}
         when Name::file:filename_all(),
          DBOpts :: db_options(),
          CFDescriptors :: list(#cf_descriptor{})).
open_with_cf(Name, DBOpts, CFDescriptors) ->
  CallerRef = make_ref(),
  async_open_with_cf(CallerRef, Name, DBOpts, CFDescriptors),
  ?WAIT_FOR_REPLY(CallerRef).

async_open_with_cf(_Callerfef, _Name, _DBOpts, _CFDescriptors) ->
  erlang:nif_error({error, not_loaded}).

%% @doc Close RocksDB
-spec close(DBHandle) -> Res when
  DBHandle :: db_handle(),
  Res :: ok | {error, any()}.
close(DBHandle) ->
  CallerRef = make_ref(),
  async_close(CallerRef, DBHandle),
  ?WAIT_FOR_REPLY(CallerRef).

async_close(_Callerfef, _DBHandle) ->
  erlang:nif_error({error, not_loaded}).

%% ===============================================
%% Column Families API
%% ===============================================

%% @doc List column families
-spec list_column_families(Name, DBOpts) -> Res when
  Name::file:filename_all(),
  DBOpts::db_options(),
  Res :: {ok, list(string())} | {error, any()}.
list_column_families(Name, DbOpts) ->
  CallerRef = make_ref(),
  async_list_column_families(CallerRef, Name, DbOpts),
  ?WAIT_FOR_REPLY(CallerRef).

async_list_column_families(_Callerfef, _Name, _DbOpts) ->
  erlang:nif_error({error, not_loaded}).

%% @doc Create a new column family
-spec create_column_family(DBHandle, Name, CFOpts) -> Res when
  DBHandle :: db_handle(),
  Name ::string(),
  CFOpts :: cf_options(),
  Res :: {ok, cf_handle()} | {error, any()}.
create_column_family(DBHandle, Name, CFOpts) ->
  CallerRef = make_ref(),
  async_create_column_family(CallerRef, DBHandle, Name, CFOpts),
  ?WAIT_FOR_REPLY(CallerRef).

async_create_column_family(_Callerfef, _DBHandle, _Name, _CFOpts) ->
  erlang:nif_error({error, not_loaded}).

%% @doc Drop a column family
-spec drop_column_family(CFHandle) -> Res when
  CFHandle::cf_handle(),
  Res :: ok | {error, any()}.
drop_column_family(CFHandle) ->
  CallerRef = make_ref(),
  async_drop_column_family(CallerRef, CFHandle),
  ?WAIT_FOR_REPLY(CallerRef).

async_drop_column_family(_Callerfef, _CFHandle) ->
  erlang:nif_error({error, not_loaded}).

%% @doc return a database snapshot
%% Snapshots provide consistent read-only views over the entire state of the key-value store
-spec snapshot(DbHandle::db_handle()) -> {ok, snapshot_handle()} | {error, any()}.
snapshot(DbHandle) ->
  CallerRef = make_ref(),
  async_snapshot(CallerRef, DbHandle),
  ?WAIT_FOR_REPLY(CallerRef).

async_snapshot(_CallerRef, _DbHandle) ->
  erlang:nif_error({error, not_loaded}).

%% @doc release a snapshot
-spec release_snapshot(SnapshotHandle::snapshot_handle()) -> ok | {error, any()}.
release_snapshot(SnapshotHandle) ->
  CallerRef = make_ref(),
  async_release_snapshot(CallerRef, SnapshotHandle),
  ?WAIT_FOR_REPLY(CallerRef).

async_release_snapshot(_CallerRef, _SnapshotHandle) ->
  erlang:nif_error({error, not_loaded}).

%% @doc Put a key/value pair into the default column family
-spec put(DBHandle, Key, Value, WriteOpts) -> Res when
  DBHandle::db_handle(),
  Key::binary(),
  Value::binary(),
  WriteOpts::write_options(),
  Res :: ok | {error, any()}.
put(DBHandle, Key, Value, WriteOpts) ->
  write(DBHandle, [{put, Key, Value}], WriteOpts).

%% @doc Put a key/value pair into the specified column family
-spec put(DBHandle, CFHandle, Key, Value, WriteOpts) -> Res when
  DBHandle::db_handle(),
  CFHandle::cf_handle(),
  Key::binary(),
  Value::binary(),
  WriteOpts::write_options(),
  Res :: ok | {error, any()}.
put(DBHandle, CFHandle, Key, Value, WriteOpts) ->
  write(DBHandle, [{put, CFHandle, Key, Value}], WriteOpts).

%% @doc Delete a key/value pair in the default column family
-spec(delete(DBHandle, Key, WriteOpts) ->
       ok | {error, any()} when DBHandle::db_handle(),
                    Key::binary(),
                    WriteOpts::write_options()).
delete(DBHandle, Key, WriteOpts) ->
  write(DBHandle, [{delete, Key}], WriteOpts).

%% @doc Delete a key/value pair in the specified column family
-spec delete(DBHandle, CFHandle, Key, WriteOpts) -> Res when
  DBHandle::db_handle(),
  CFHandle::cf_handle(),
  Key::binary(),
  WriteOpts::write_options(),
  Res ::  ok | {error, any()}.
delete(DBHandle, CFHandle, Key, WriteOpts) ->
  write(DBHandle, [{delete, CFHandle, Key}], WriteOpts).

async_write(_CallerRef, _DBHandle, _WriteActions, _WriteOpts) ->
  erlang:nif_error({error, not_loaded}).

%% @doc Apply the specified updates to the database.
-spec write(DBHandle, WriteActions, WriteOpts) -> Res when
  DBHandle::db_handle(),
   WriteActions::write_actions(),
   WriteOpts::write_options(),
   Res :: ok | {error, any()}.
write(DBHandle, WriteActions, WriteOpts) ->
  CallerRef = make_ref(),
  async_write(CallerRef, DBHandle, WriteActions, WriteOpts),
  ?WAIT_FOR_REPLY(CallerRef).

%% @doc Retrieve a key/value pair in the default column family
-spec get(DBHandle, Key, ReadOpts) ->  Res when
  DBHandle::db_handle(),
  Key::binary(),
  ReadOpts::read_options(),
   Res :: {ok, binary()} | not_found | {error, any()}.
get(DBHandle, Key, ReadOpts) ->
  CallerRef = make_ref(),
  async_get(CallerRef, DBHandle, Key, ReadOpts),
  ?WAIT_FOR_REPLY(CallerRef).

async_get(_CallerRef, _DBHandle, _Key, _ReadOpts) ->
  erlang:nif_error({error, not_loaded}).

%% @doc Retrieve a key/value pair in the specified column family
-spec get(DBHandle, CFHandle, Key, ReadOpts) -> Res when
  DBHandle::db_handle(),
  CFHandle::cf_handle(),
  Key::binary(),
  ReadOpts::read_options(),
  Res :: {ok, binary()} | not_found | {error, any()}.
get(DBHandle, CFHandle, Key, ReadOpts) ->
  CallerRef = make_ref(),
  async_get(CallerRef, DBHandle, CFHandle, Key, ReadOpts),
  ?WAIT_FOR_REPLY(CallerRef).

async_get(_CallerRef, _DBHandle, _CfHandle, _Key, _ReadOpts) ->
  erlang:nif_error({error, not_loaded}).

%% @doc Return a iterator over the contents of the database.
%% The result of iterator() is initially invalid (caller must
%% call iterator_move function on the iterator before using it).
-spec iterator(DBHandle, ReadOpts) -> Res when
  DBHandle::db_handle(),
  ReadOpts::read_options(),
  Res :: {ok, itr_handle()} | {error, any()}.
iterator(DBHandle, ReadOpts) ->
  CallerRef = make_ref(),
  async_iterator(CallerRef, DBHandle, ReadOpts),
  ?WAIT_FOR_REPLY(CallerRef).

async_iterator(_CallerRef, _DBHandle, _ReadOpts) ->
  erlang:nif_error({error, not_loaded}).

-spec iterator(DBHandle, ReadOpts, keys_only) -> Res when
  DBHandle::db_handle(),
  ReadOpts::read_options(),
  Res :: {ok, itr_handle()} | {error, any()}.

iterator(DBHandle, ReadOpts, keys_only) ->
  CallerRef = make_ref(),
  async_iterator(CallerRef, DBHandle, ReadOpts, keys_only),
  ?WAIT_FOR_REPLY(CallerRef).


async_iterator(_CallerRef, _DBHandle, _ReadOpts, keys_only) ->
  erlang:nif_error({error, not_loaded}).

%% @doc Return a iterator over the contents of the specified column families.
-spec iterators(DBHandle, CFHandles, ReadOpts) -> Res when
  DBHandle ::db_handle(),
  CFHandles :: [cf_handle()],
  ReadOpts :: read_options(),
  Res :: {ok, itr_handle()} | {error, any()}.
iterators(DBHandle, CFHandles, ReadOpts) ->
  CallerRef = make_ref(),
  async_iterators(CallerRef, DBHandle, CFHandles, ReadOpts),
  ?WAIT_FOR_REPLY(CallerRef).

async_iterators(_CallerRef, _DBHandle, _CFHandles, _ReadOpts) ->
  erlang:nif_error({error, not_loaded}).


%% @doc Return keys iterator over the contents of the specified column families.
iterators(DBHandle, CFHandles, ReadOpts, keys_only) ->
  CallerRef = make_ref(),
  async_iterators(CallerRef, DBHandle, CFHandles, ReadOpts, keys_only),
  ?WAIT_FOR_REPLY(CallerRef);

iterators(_, _, _, _) ->
  erlang:error(badarg).

async_iterators(_CallerRef, _DBHandle, _CFHandles, _ReadOpts, keys_only) ->
  erlang:nif_error({error, not_loaded}).

%% @doc  Move to the specified place
-spec iterator_move(ITRHandle, ITRAction) -> Res when
  ITRHandle::itr_handle(),
  ITRAction::iterator_action(),
  Res ::  {ok, Key::binary(), Value::binary()} |
      {ok, Key::binary()} |
      {error, invalid_iterator} |
      {error, iterator_closed}.
iterator_move(ITRHandle, ITRAction) ->
  case async_iterator_move(undefined, ITRHandle, ITRAction) of
    Ref when is_reference(Ref) ->
      receive
        {Ref, X} -> X
      end;
    {ok, _} = Key -> Key;
    {ok, _, _} = KeyVal -> KeyVal;
    ER -> ER
  end.

async_iterator_move(_CallerRef, _ITRHandle, _ITRAction) ->
  erlang:nif_error({error, not_loaded}).


%% @doc Close a iterator
-spec iterator_close(ITRHandle::itr_handle()) -> ok.
iterator_close(ITRHandle) ->
  CallerRef = make_ref(),
  async_iterator_close(CallerRef, ITRHandle),
  ?WAIT_FOR_REPLY(CallerRef).

async_iterator_close(_CallerRef, _ITRHandle) ->
  erlang:nif_error({error, not_loaded}).


-type fold_fun() :: fun(({Key::binary(), Value::binary()}, any()) -> any()).

%% @doc Calls Fun(Elem, AccIn) on successive elements in the default column family
%% starting with AccIn == Acc0.
%% Fun/2 must return a new accumulator which is passed to the next call.
%% The function returns the final value of the accumulator.
%% Acc0 is returned if the default column family is empty.
-spec fold(DBHandle, Fun, AccIn, ReadOpts) -> AccOut when
  DBHandle::db_handle(),
  Fun::fold_fun(),
  AccIn::any(),
  ReadOpts::read_options(),
  AccOut :: any().
fold(DBHandle, Fun, Acc0, ReadOpts) ->
  {ok, Itr} = iterator(DBHandle, ReadOpts),
  do_fold(Itr, Fun, Acc0).

%% @doc Calls Fun(Elem, AccIn) on successive elements in the specified column family
%% Other specs are same with fold/4
-spec fold(DBHandle, CFHandle, Fun, AccIn, ReadOpts) -> AccOut when
  DBHandle::db_handle(),
  CFHandle::cf_handle(),
  Fun::fold_fun(),
  AccIn::any(),
  ReadOpts::read_options(),
  AccOut :: any().
fold(_DBHandle, _CFHandle, _Fun, _Acc0, _ReadOpts) ->
  _Acc0.

-type fold_keys_fun() :: fun((Key::binary(), any()) -> any()).

%% @doc Calls Fun(Elem, AccIn) on successive elements in the default column family
%% starting with AccIn == Acc0.
%% Fun/2 must return a new accumulator which is passed to the next call.
%% The function returns the final value of the accumulator.
%% Acc0 is returned if the default column family is empty.
-spec fold_keys(DBHandle, Fun, AccIn, ReadOpts) -> AccOut when
  DBHandle::db_handle(),
  Fun::fold_keys_fun(),
  AccIn::any(),
  ReadOpts::read_options(),
  AccOut :: any().
fold_keys(DBHandle, Fun, Acc0, ReadOpts) ->
  {ok, Itr} = iterator(DBHandle, ReadOpts, keys_only),
  do_fold(Itr, Fun, Acc0).

%% @doc Calls Fun(Elem, AccIn) on successive elements in the specified column family
%% Other specs are same with fold_keys/4
-spec fold_keys(DBHandle, CFHandle, Fun, AccIn, ReadOpts) -> AccOut when
  DBHandle::db_handle(),
  CFHandle::cf_handle(),
  Fun::fold_keys_fun(),
  AccIn::any(),
  ReadOpts::read_options(),
  AccOut :: any().
fold_keys(_DBHandle, _CFHandle, _Fun, _Acc0, _ReadOpts) ->
  _Acc0.

%% @doc is the database empty
-spec  is_empty(DBHandle::db_handle()) -> true | false.
is_empty(DbHandle) ->
  CallerRef = make_ref(),
  async_is_empty(CallerRef, DbHandle),
  ?WAIT_FOR_REPLY(CallerRef).

async_is_empty(_CallerRef, _DBHandle) ->
  erlang:nif_error({error, not_loaded}).

%% @doc Destroy the contents of the specified database.
%% Be very careful using this method.
-spec destroy(Name::file:filename_all(), DBOpts::db_options()) -> ok | {error, any()}.
destroy(Name, DBOpts) ->
  CallerRef = make_ref(),
  async_destroy(CallerRef, Name, DBOpts),
  ?WAIT_FOR_REPLY(CallerRef).

async_destroy(_CallerRef, _Name, _DBOpts) ->
  erlang:nif_error({error, not_loaded}).

%% @doc Try to repair as much of the contents of the database as possible.
%% Some data may be lost, so be careful when calling this function
-spec repair(Name::file:filename_all(), DBOpts::db_options()) -> ok | {error, any()}.
repair(Name, DBOpts) ->
  CallerRef = make_ref(),
  async_repair(CallerRef, Name, DBOpts),
  ?WAIT_FOR_REPLY(CallerRef).

async_repair(_CallerRef, _Name, _DbOpts) ->
   erlang:nif_error({error, not_loaded}).

%% @doc take a snapshot of a running RocksDB database in a separate directory
%% http://rocksdb.org/blog/2609/use-checkpoints-for-efficient-snapshots/
-spec checkpoint(
  DbHandle::db_handle(), Path::file:filename_all()
) -> ok | {error, any()}.
checkpoint(DbHandle, Path) ->
  CallerRef = make_ref(),
  async_checkpoint(CallerRef, DbHandle, Path),
  ?WAIT_FOR_REPLY(CallerRef).

async_checkpoint(_Callerfef, _DbHandle, _Path) ->
  erlang:nif_error({error, not_loaded}).

%% @doc Return the approximate number of keys in the default column family.
%% Implemented by calling GetIntProperty with "rocksdb.estimate-num-keys"
-spec count(DBHandle::db_handle()) ->  non_neg_integer() | {error, any()}.
count(DBHandle) ->
  count_1(get_property(DBHandle, <<"rocksdb.estimate-num-keys">>)).

%% @doc
%% Return the approximate number of keys in the specified column family.
%%
-spec count(DBHandle::db_handle(), CFHandle::cf_handle()) -> non_neg_integer() | {error, any()}.
count(DBHandle, CFHandle) ->
  count_1(get_property(DBHandle, CFHandle, <<"rocksdb.estimate-num-keys">>)).

count_1({ok, BinCount}) -> erlang:binary_to_integer(BinCount);
count_1(Error) -> Error.

%% @doc Return the current stats of the default column family
%% Implemented by calling GetProperty with "rocksdb.stats"
-spec stats(DBHandle::db_handle()) -> {ok, any()} | {error, any()}.
stats(DBHandle) ->
  get_property(DBHandle, <<"rocksdb.stats">>).

%% @doc Return the current stats of the specified column family
%% Implemented by calling GetProperty with "rocksdb.stats"
-spec stats(
  DBHandle::db_handle(), CFHandle::cf_handle()
) -> {ok, any()} | {error, any()}.
stats(DBHandle, CfHandle) ->
  get_property(DBHandle, CfHandle, <<"rocksdb.stats">>).

%% @doc Return the RocksDB internal status of the default column family specified at Property
-spec get_property(
  DBHandle::db_handle(), Property::binary()
) -> {ok, any()} | {error, any()}.
get_property(_DBHandle, _Property) ->
  erlang:nif_error({error, not_loaded}).

%% @doc Return the RocksDB internal status of the specified column family specified at Property
-spec get_property(
  DBHandle::db_handle(), CFHandle::cf_handle(), Property::binary()
) -> string() | {error, any()}.
get_property(_DBHandle, _CFHandle, _Property) ->
  erlang:nif_error({error, not_loaded}).

%% ===================================================================
%% Internal functions
%% ===================================================================
do_fold(Itr, Fun, Acc0) ->
  try
    fold_loop(iterator_move(Itr, first), Itr, Fun, Acc0)
  after
    iterator_close(Itr)
  end.

fold_loop({error, iterator_closed}, _Itr, _Fun, Acc0) ->
  throw({iterator_closed, Acc0});
fold_loop({error, invalid_iterator}, _Itr, _Fun, Acc0) ->
  Acc0;
fold_loop({ok, K}, Itr, Fun, Acc0) ->
  Acc = Fun(K, Acc0),
  fold_loop(iterator_move(Itr, next), Itr, Fun, Acc);
fold_loop({ok, K, V}, Itr, Fun, Acc0) ->
  Acc = Fun({K, V}, Acc0),
  fold_loop(iterator_move(Itr, next), Itr, Fun, Acc).
