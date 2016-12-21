// -------------------------------------------------------------------
//
// eleveldb: Erlang Wrapper for LevelDB (http://code.google.com/p/leveldb/)
//
// Copyright (c) 2011-2013 Basho Technologies, Inc. All Rights Reserved.
//
// This file is provided to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file
// except in compliance with the License.  You may obtain
// a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//
// -------------------------------------------------------------------

#include <syslog.h>

#include <new>
#include <set>
#include <stack>
#include <deque>
#include <sstream>
#include <utility>
#include <stdexcept>
#include <algorithm>
#include <vector>

#include "erocksdb.h"

#ifndef INCL_THREADING_H
    #include "threading.h"
#endif

#ifndef ATOMS_H
    #include "atoms.h"
#endif


#include "detail.hpp"

#ifndef INCL_UTIL_H
    #include "util.h"
#endif

#ifndef INCL_ENV_H
  #include "env.h"
#endif

static ErlNifFunc nif_funcs[] =
{


  {"async_open", 3, erocksdb::AsyncOpen},
  {"async_open_with_cf", 4, erocksdb::AsyncOpenWithCf},
  {"async_close", 2, erocksdb::AsyncClose},
  {"get_property", 2, erocksdb::GetProperty},
  {"get_property", 3, erocksdb::GetProperty},
  {"async_destroy", 3, erocksdb::AsyncDestroy},

  // column families
  {"async_list_column_families", 3, erocksdb::AsyncListColumnFamilies},
  {"async_create_column_family", 4, erocksdb::AsyncCreateColumnFamily},
  {"async_drop_column_family", 2, erocksdb::AsyncDropColumnFamily},

  // kv operations
  {"async_write", 4, erocksdb::AsyncWrite},
  {"async_get", 4, erocksdb::AsyncGet},
  {"async_get", 5, erocksdb::AsyncGet},

  {"async_snapshot", 2, erocksdb::AsyncSnapshot},
  {"async_release_snapshot", 2, erocksdb::AsyncReleaseSnapshot},

  // iterator operations
  {"async_iterator", 3, erocksdb::AsyncIterator},
  {"async_iterator", 4, erocksdb::AsyncIterator},
  {"async_iterator_move", 3, erocksdb::AsyncIteratorMove},
  {"async_iterator_close", 2, erocksdb::AsyncIteratorClose},
  {"async_iterators", 4, erocksdb::AsyncIterators},
  {"async_iterators", 5, erocksdb::AsyncIterators},

  // db management
  {"async_checkpoint", 3, erocksdb::AsyncCheckpoint},
  {"async_repair", 3, erocksdb::AsyncRepair},
  {"async_is_empty", 2, erocksdb::AsyncIsEmpty},

};

namespace erocksdb {

// Atoms (initialized in on_load)
// Related to Erlang
ERL_NIF_TERM ATOM_TRUE;
ERL_NIF_TERM ATOM_FALSE;
ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ERROR;
ERL_NIF_TERM ATOM_EINVAL;
ERL_NIF_TERM ATOM_BADARG;
ERL_NIF_TERM ATOM_NOT_FOUND;

// Related to CFOptions
ERL_NIF_TERM ATOM_BLOCK_CACHE_SIZE_MB_FOR_POINT_LOOKUP;
ERL_NIF_TERM ATOM_MEMTABLE_MEMORY_BUDGET;
ERL_NIF_TERM ATOM_WRITE_BUFFER_SIZE;
ERL_NIF_TERM ATOM_MAX_WRITE_BUFFER_NUMBER;
ERL_NIF_TERM ATOM_MIN_WRITE_BUFFER_NUMBER_TO_MERGE;
ERL_NIF_TERM ATOM_COMPRESSION;
ERL_NIF_TERM ATOM_NUM_LEVELS;
ERL_NIF_TERM ATOM_LEVEL0_FILE_NUM_COMPACTION_TRIGGER;
ERL_NIF_TERM ATOM_LEVEL0_SLOWDOWN_WRITES_TRIGGER;
ERL_NIF_TERM ATOM_LEVEL0_STOP_WRITES_TRIGGER;
ERL_NIF_TERM ATOM_MAX_MEM_COMPACTION_LEVEL;
ERL_NIF_TERM ATOM_TARGET_FILE_SIZE_BASE;
ERL_NIF_TERM ATOM_TARGET_FILE_SIZE_MULTIPLIER;
ERL_NIF_TERM ATOM_MAX_BYTES_FOR_LEVEL_BASE;
ERL_NIF_TERM ATOM_MAX_BYTES_FOR_LEVEL_MULTIPLIER;
ERL_NIF_TERM ATOM_MAX_COMPACTION_BYTES;
ERL_NIF_TERM ATOM_SOFT_RATE_LIMIT;
ERL_NIF_TERM ATOM_HARD_RATE_LIMIT;
ERL_NIF_TERM ATOM_ARENA_BLOCK_SIZE;
ERL_NIF_TERM ATOM_DISABLE_AUTO_COMPACTIONS;
ERL_NIF_TERM ATOM_PURGE_REDUNDANT_KVS_WHILE_FLUSH;
ERL_NIF_TERM ATOM_COMPACTION_STYLE;
ERL_NIF_TERM ATOM_VERIFY_CHECKSUMS_IN_COMPACTION;
ERL_NIF_TERM ATOM_FILTER_DELETES;
ERL_NIF_TERM ATOM_MAX_SEQUENTIAL_SKIP_IN_ITERATIONS;
ERL_NIF_TERM ATOM_INPLACE_UPDATE_SUPPORT;
ERL_NIF_TERM ATOM_INPLACE_UPDATE_NUM_LOCKS;
ERL_NIF_TERM ATOM_TABLE_FACTORY_BLOCK_CACHE_SIZE;
ERL_NIF_TERM ATOM_IN_MEMORY_MODE;
ERL_NIF_TERM ATOM_IN_MEMORY;
ERL_NIF_TERM ATOM_BLOCK_BASED_TABLE_OPTIONS;

// Related to DBOptions
ERL_NIF_TERM ATOM_TOTAL_THREADS;
ERL_NIF_TERM ATOM_CREATE_IF_MISSING;
ERL_NIF_TERM ATOM_CREATE_MISSING_COLUMN_FAMILIES;
ERL_NIF_TERM ATOM_ERROR_IF_EXISTS;
ERL_NIF_TERM ATOM_PARANOID_CHECKS;
ERL_NIF_TERM ATOM_MAX_OPEN_FILES;
ERL_NIF_TERM ATOM_MAX_TOTAL_WAL_SIZE;
ERL_NIF_TERM ATOM_DISABLE_DATA_SYNC;
ERL_NIF_TERM ATOM_USE_FSYNC;
ERL_NIF_TERM ATOM_DB_PATHS;
ERL_NIF_TERM ATOM_DB_LOG_DIR;
ERL_NIF_TERM ATOM_WAL_DIR;
ERL_NIF_TERM ATOM_DELETE_OBSOLETE_FILES_PERIOD_MICROS;
ERL_NIF_TERM ATOM_MAX_BACKGROUND_COMPACTIONS;
ERL_NIF_TERM ATOM_MAX_BACKGROUND_FLUSHES;
ERL_NIF_TERM ATOM_MAX_LOG_FILE_SIZE;
ERL_NIF_TERM ATOM_LOG_FILE_TIME_TO_ROLL;
ERL_NIF_TERM ATOM_KEEP_LOG_FILE_NUM;
ERL_NIF_TERM ATOM_MAX_MANIFEST_FILE_SIZE;
ERL_NIF_TERM ATOM_TABLE_CACHE_NUMSHARDBITS;
ERL_NIF_TERM ATOM_WAL_TTL_SECONDS;
ERL_NIF_TERM ATOM_WAL_SIZE_LIMIT_MB;
ERL_NIF_TERM ATOM_MANIFEST_PREALLOCATION_SIZE;
ERL_NIF_TERM ATOM_ALLOW_OS_BUFFER;
ERL_NIF_TERM ATOM_ALLOW_MMAP_READS;
ERL_NIF_TERM ATOM_ALLOW_MMAP_WRITES;
ERL_NIF_TERM ATOM_IS_FD_CLOSE_ON_EXEC;
ERL_NIF_TERM ATOM_SKIP_LOG_ERROR_ON_RECOVERY;
ERL_NIF_TERM ATOM_STATS_DUMP_PERIOD_SEC;
ERL_NIF_TERM ATOM_ADVISE_RANDOM_ON_OPEN;
ERL_NIF_TERM ATOM_ACCESS_HINT;
ERL_NIF_TERM ATOM_COMPACTION_READAHEAD_SIZE;
ERL_NIF_TERM ATOM_USE_ADAPTIVE_MUTEX;
ERL_NIF_TERM ATOM_BYTES_PER_SYNC;
ERL_NIF_TERM ATOM_SKIP_STATS_UPDATE_ON_DB_OPEN;
ERL_NIF_TERM ATOM_WAL_RECOVERY_MODE;
ERL_NIF_TERM ATOM_ALLOW_CONCURRENT_MEMTABLE_WRITE;
ERL_NIF_TERM ATOM_ENABLE_WRITE_THREAD_ADAPTATIVE_YIELD;


// Related to BlockBasedTable Options
ERL_NIF_TERM ATOM_NO_BLOCK_CACHE;
ERL_NIF_TERM ATOM_BLOCK_SIZE;
ERL_NIF_TERM ATOM_BLOCK_CACHE_SIZE;
ERL_NIF_TERM ATOM_BLOOM_FILTER_POLICY;
ERL_NIF_TERM ATOM_FORMAT_VERSION;
ERL_NIF_TERM ATOM_SKIP_TABLE_BUILDER_FLUSH;
ERL_NIF_TERM ATOM_CACHE_INDEX_AND_FILTER_BLOCKS;

// Related to Read Options
ERL_NIF_TERM ATOM_VERIFY_CHECKSUMS;
ERL_NIF_TERM ATOM_FILL_CACHE;
ERL_NIF_TERM ATOM_ITERATE_UPPER_BOUND;
ERL_NIF_TERM ATOM_TAILING;
ERL_NIF_TERM ATOM_TOTAL_ORDER_SEEK;
ERL_NIF_TERM ATOM_SNAPSHOT;
ERL_NIF_TERM ATOM_BAD_SNAPSHOT;

// Related to Write Options
ERL_NIF_TERM ATOM_SYNC;
ERL_NIF_TERM ATOM_DISABLE_WAL;
ERL_NIF_TERM ATOM_TIMEOUT_HINT_US;
ERL_NIF_TERM ATOM_IGNORE_MISSING_COLUMN_FAMILIES;

// Related to Write Actions
ERL_NIF_TERM ATOM_CLEAR;
ERL_NIF_TERM ATOM_PUT;
ERL_NIF_TERM ATOM_DELETE;

// Related to Iterator Actions
ERL_NIF_TERM ATOM_FIRST;
ERL_NIF_TERM ATOM_LAST;
ERL_NIF_TERM ATOM_NEXT;
ERL_NIF_TERM ATOM_PREV;

// Related to Iterator Value to be retrieved
ERL_NIF_TERM ATOM_KEYS_ONLY;

// Related to Access Hint
ERL_NIF_TERM ATOM_ACCESS_HINT_NORMAL;
ERL_NIF_TERM ATOM_ACCESS_HINT_SEQUENTIAL;
ERL_NIF_TERM ATOM_ACCESS_HINT_WILLNEED;
ERL_NIF_TERM ATOM_ACCESS_HINT_NONE;

// Related to Compression Type
ERL_NIF_TERM ATOM_COMPRESSION_TYPE_SNAPPY;
ERL_NIF_TERM ATOM_COMPRESSION_TYPE_ZLIB;
ERL_NIF_TERM ATOM_COMPRESSION_TYPE_BZIP2;
ERL_NIF_TERM ATOM_COMPRESSION_TYPE_LZ4;
ERL_NIF_TERM ATOM_COMPRESSION_TYPE_LZ4H;
ERL_NIF_TERM ATOM_COMPRESSION_TYPE_NONE;

// Related to Compaction Style
ERL_NIF_TERM ATOM_COMPACTION_STYLE_LEVEL;
ERL_NIF_TERM ATOM_COMPACTION_STYLE_UNIVERSAL;
ERL_NIF_TERM ATOM_COMPACTION_STYLE_FIFO;
ERL_NIF_TERM ATOM_COMPACTION_STYLE_NONE;

// Related to WAL Recovery Mode
ERL_NIF_TERM ATOM_WAL_TOLERATE_CORRUPTED_TAIL_RECORDS;
ERL_NIF_TERM ATOM_WAL_ABSOLUTE_CONSISTENCY;
ERL_NIF_TERM ATOM_WAL_POINT_IN_TIME_RECOVERY;
ERL_NIF_TERM ATOM_WAL_SKIP_ANY_CORRUPTED_RECORDS;

// Related to Error Codes
ERL_NIF_TERM ATOM_ERROR_DB_OPEN;
ERL_NIF_TERM ATOM_ERROR_DB_PUT;
ERL_NIF_TERM ATOM_ERROR_DB_DELETE;
ERL_NIF_TERM ATOM_ERROR_DB_WRITE;
ERL_NIF_TERM ATOM_ERROR_DB_DESTROY;
ERL_NIF_TERM ATOM_ERROR_DB_REPAIR;
ERL_NIF_TERM ATOM_BAD_WRITE_ACTION;
ERL_NIF_TERM ATOM_KEEP_RESOURCE_FAILED;
ERL_NIF_TERM ATOM_ITERATOR_CLOSED;
ERL_NIF_TERM ATOM_INVALID_ITERATOR;

// Related to NIF initialize parameters
ERL_NIF_TERM ATOM_WRITE_THREADS;

}   // namespace erocksdb


using std::nothrow;

ERL_NIF_TERM parse_init_option(ErlNifEnv* env, ERL_NIF_TERM item, erocksdb::DbOptions& opts)
{
    int arity;
    const ERL_NIF_TERM* option;
    if (enif_get_tuple(env, item, &arity, &option) && 2==arity)
    {
        if (option[0] == erocksdb::ATOM_WRITE_THREADS)
        {
            unsigned long temp;
            if (enif_get_ulong(env, option[1], &temp))
            {
                if (temp != 0)
                {
                    opts.m_DbThreads = temp;
                }   // if
            }   // if
        }   // if
    }

    return erocksdb::ATOM_OK;
}


static void on_unload(ErlNifEnv *env, void *priv_data)
{
    erocksdb::PrivData *p = static_cast<erocksdb::PrivData *>(priv_data);
    delete p;
}

static int on_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    /* Convert the private data to the new version. */
    *priv_data = *old_priv_data;
    return 0;
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
try
{
  *priv_data = NULL;

  rocksdb::Env::Default();

  // inform erlang of our two resource types
  erocksdb::DbObject::CreateDbObjectType(env);
  erocksdb::ColumnFamilyObject::CreateColumnFamilyObjectType(env);
  erocksdb::ItrObject::CreateItrObjectType(env);
  erocksdb::SnapshotObject::CreateSnapshotObjectType(env);

  // must initialize atoms before processing options
#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }
  // Related to Erlang
  ATOM(erocksdb::ATOM_TRUE, "true");
  ATOM(erocksdb::ATOM_FALSE, "false");
  ATOM(erocksdb::ATOM_OK, "ok");
  ATOM(erocksdb::ATOM_ERROR, "error");
  ATOM(erocksdb::ATOM_EINVAL, "einval");
  ATOM(erocksdb::ATOM_BADARG, "badarg");
  ATOM(erocksdb::ATOM_NOT_FOUND, "not_found");

  // Related to CFOptions
  ATOM(erocksdb::ATOM_BLOCK_CACHE_SIZE_MB_FOR_POINT_LOOKUP, "block_cache_size_mb_for_point_lookup");
  ATOM(erocksdb::ATOM_MEMTABLE_MEMORY_BUDGET, "memtable_memory_budget");
  ATOM(erocksdb::ATOM_WRITE_BUFFER_SIZE, "write_buffer_size");
  ATOM(erocksdb::ATOM_MAX_WRITE_BUFFER_NUMBER, "max_write_buffer_number");
  ATOM(erocksdb::ATOM_MIN_WRITE_BUFFER_NUMBER_TO_MERGE, "min_write_buffer_number_to_merge");
  ATOM(erocksdb::ATOM_COMPRESSION, "compression");
  ATOM(erocksdb::ATOM_NUM_LEVELS, "num_levels");
  ATOM(erocksdb::ATOM_LEVEL0_FILE_NUM_COMPACTION_TRIGGER, "level0_file_num_compaction_trigger");
  ATOM(erocksdb::ATOM_LEVEL0_SLOWDOWN_WRITES_TRIGGER, "level0_slowdown_writes_trigger");
  ATOM(erocksdb::ATOM_LEVEL0_STOP_WRITES_TRIGGER, "level0_stop_writes_trigger");
  ATOM(erocksdb::ATOM_MAX_MEM_COMPACTION_LEVEL, "max_mem_compaction_level");
  ATOM(erocksdb::ATOM_TARGET_FILE_SIZE_BASE, "target_file_size_base");
  ATOM(erocksdb::ATOM_TARGET_FILE_SIZE_MULTIPLIER, "target_file_size_multiplier");
  ATOM(erocksdb::ATOM_MAX_BYTES_FOR_LEVEL_BASE, "max_bytes_for_level_base");
  ATOM(erocksdb::ATOM_MAX_BYTES_FOR_LEVEL_MULTIPLIER, "max_bytes_for_level_multiplier");
  ATOM(erocksdb::ATOM_MAX_COMPACTION_BYTES, "max_compaction_bytes");
  ATOM(erocksdb::ATOM_SOFT_RATE_LIMIT, "soft_rate_limit");
  ATOM(erocksdb::ATOM_HARD_RATE_LIMIT, "hard_rate_limit");
  ATOM(erocksdb::ATOM_ARENA_BLOCK_SIZE, "arena_block_size");
  ATOM(erocksdb::ATOM_DISABLE_AUTO_COMPACTIONS, "disable_auto_compactions");
  ATOM(erocksdb::ATOM_PURGE_REDUNDANT_KVS_WHILE_FLUSH, "purge_redundant_kvs_while_flush");
  ATOM(erocksdb::ATOM_COMPACTION_STYLE, "compaction_style");
  ATOM(erocksdb::ATOM_VERIFY_CHECKSUMS_IN_COMPACTION, "verify_checksums_in_compaction");
  ATOM(erocksdb::ATOM_FILTER_DELETES, "filter_deletes");
  ATOM(erocksdb::ATOM_MAX_SEQUENTIAL_SKIP_IN_ITERATIONS, "max_sequential_skip_in_iterations");
  ATOM(erocksdb::ATOM_INPLACE_UPDATE_SUPPORT, "inplace_update_support");
  ATOM(erocksdb::ATOM_INPLACE_UPDATE_NUM_LOCKS, "inplace_update_num_locks");
  ATOM(erocksdb::ATOM_TABLE_FACTORY_BLOCK_CACHE_SIZE, "table_factory_block_cache_size");
  ATOM(erocksdb::ATOM_IN_MEMORY_MODE, "in_memory_mode");
  ATOM(erocksdb::ATOM_IN_MEMORY, "in_memory");
  ATOM(erocksdb::ATOM_BLOCK_BASED_TABLE_OPTIONS, "block_based_table_options");

  // Related to DBOptions
  ATOM(erocksdb::ATOM_TOTAL_THREADS, "total_threads");
  ATOM(erocksdb::ATOM_CREATE_IF_MISSING, "create_if_missing");
  ATOM(erocksdb::ATOM_CREATE_MISSING_COLUMN_FAMILIES, "create_missing_column_families");
  ATOM(erocksdb::ATOM_ERROR_IF_EXISTS, "error_if_exists");
  ATOM(erocksdb::ATOM_PARANOID_CHECKS, "paranoid_checks");
  ATOM(erocksdb::ATOM_MAX_OPEN_FILES, "max_open_files");
  ATOM(erocksdb::ATOM_MAX_TOTAL_WAL_SIZE, "max_total_wal_size");
  ATOM(erocksdb::ATOM_DISABLE_DATA_SYNC, "disable_data_sync");
  ATOM(erocksdb::ATOM_USE_FSYNC, "use_fsync");
  ATOM(erocksdb::ATOM_DB_PATHS, "db_paths");
  ATOM(erocksdb::ATOM_DB_LOG_DIR, "db_log_dir");
  ATOM(erocksdb::ATOM_WAL_DIR, "wal_dir");
  ATOM(erocksdb::ATOM_DELETE_OBSOLETE_FILES_PERIOD_MICROS, "delete_obsolete_files_period_micros");
  ATOM(erocksdb::ATOM_MAX_BACKGROUND_COMPACTIONS, "max_background_compactions");
  ATOM(erocksdb::ATOM_MAX_BACKGROUND_FLUSHES, "max_background_flushes");
  ATOM(erocksdb::ATOM_MAX_LOG_FILE_SIZE, "max_log_file_size");
  ATOM(erocksdb::ATOM_LOG_FILE_TIME_TO_ROLL, "log_file_time_to_roll");
  ATOM(erocksdb::ATOM_KEEP_LOG_FILE_NUM, "keep_log_file_num");
  ATOM(erocksdb::ATOM_MAX_MANIFEST_FILE_SIZE, "max_manifest_file_size");
  ATOM(erocksdb::ATOM_TABLE_CACHE_NUMSHARDBITS, "table_cache_numshardbits");
  ATOM(erocksdb::ATOM_WAL_TTL_SECONDS, "wal_ttl_seconds");
  ATOM(erocksdb::ATOM_WAL_SIZE_LIMIT_MB, "wal_size_limit_mb");
  ATOM(erocksdb::ATOM_MANIFEST_PREALLOCATION_SIZE, "manifest_preallocation_size");
  ATOM(erocksdb::ATOM_ALLOW_OS_BUFFER, "allow_os_buffer");
  ATOM(erocksdb::ATOM_ALLOW_MMAP_READS, "allow_mmap_reads");
  ATOM(erocksdb::ATOM_ALLOW_MMAP_WRITES, "allow_mmap_writes");
  ATOM(erocksdb::ATOM_IS_FD_CLOSE_ON_EXEC, "is_fd_close_on_exec");
  ATOM(erocksdb::ATOM_SKIP_LOG_ERROR_ON_RECOVERY, "skip_log_error_on_recovery");
  ATOM(erocksdb::ATOM_STATS_DUMP_PERIOD_SEC, "stats_dump_period_sec");
  ATOM(erocksdb::ATOM_ADVISE_RANDOM_ON_OPEN, "advise_random_on_open");
  ATOM(erocksdb::ATOM_ACCESS_HINT, "access_hint");
  ATOM(erocksdb::ATOM_COMPACTION_READAHEAD_SIZE, "compaction_readahead_size");
  ATOM(erocksdb::ATOM_USE_ADAPTIVE_MUTEX, "use_adaptive_mutex");
  ATOM(erocksdb::ATOM_BYTES_PER_SYNC, "bytes_per_sync");
  ATOM(erocksdb::ATOM_SKIP_STATS_UPDATE_ON_DB_OPEN, "skip_stats_update_on_db_open");
  ATOM(erocksdb::ATOM_WAL_RECOVERY_MODE, "wal_recovery_mode");
  ATOM(erocksdb::ATOM_ALLOW_CONCURRENT_MEMTABLE_WRITE, "allow_concurrent_memtable_write");
  ATOM(erocksdb::ATOM_ENABLE_WRITE_THREAD_ADAPTATIVE_YIELD, "enable_write_thread_adaptive_yield");


  // Related to BlockBasedTable Options
  ATOM(erocksdb::ATOM_NO_BLOCK_CACHE, "no_block_cache");
  ATOM(erocksdb::ATOM_BLOCK_SIZE, "block_size");
  ATOM(erocksdb::ATOM_BLOCK_CACHE_SIZE, "block_cache_size");
  ATOM(erocksdb::ATOM_BLOOM_FILTER_POLICY, "bloom_filter_policy");
  ATOM(erocksdb::ATOM_FORMAT_VERSION, "format_version");
  ATOM(erocksdb::ATOM_SKIP_TABLE_BUILDER_FLUSH, "skip_table_builder_flush");
  ATOM(erocksdb::ATOM_CACHE_INDEX_AND_FILTER_BLOCKS, "cache_index_and_filter_blocks");

  // Related to Read Options
  ATOM(erocksdb::ATOM_VERIFY_CHECKSUMS, "verify_checksums");
  ATOM(erocksdb::ATOM_FILL_CACHE,"fill_cache");
  ATOM(erocksdb::ATOM_ITERATE_UPPER_BOUND,"iterate_upper_bound");
  ATOM(erocksdb::ATOM_TAILING,"tailing");
  ATOM(erocksdb::ATOM_TOTAL_ORDER_SEEK,"total_order_seek");
  ATOM(erocksdb::ATOM_SNAPSHOT, "snapshot");
  ATOM(erocksdb::ATOM_BAD_SNAPSHOT, "bad_snapshot");

  // Related to Write Options
  ATOM(erocksdb::ATOM_SYNC, "sync");
  ATOM(erocksdb::ATOM_DISABLE_WAL, "disable_wal");
  ATOM(erocksdb::ATOM_TIMEOUT_HINT_US, "timeout_hint_us");
  ATOM(erocksdb::ATOM_IGNORE_MISSING_COLUMN_FAMILIES, "ignore_missing_column_families");

  // Related to Write Options
  ATOM(erocksdb::ATOM_CLEAR, "clear");
  ATOM(erocksdb::ATOM_PUT, "put");
  ATOM(erocksdb::ATOM_DELETE, "delete");

  // Related to Iterator Options
  ATOM(erocksdb::ATOM_FIRST, "first");
  ATOM(erocksdb::ATOM_LAST, "last");
  ATOM(erocksdb::ATOM_NEXT, "next");
  ATOM(erocksdb::ATOM_PREV, "prev");

  // Related to Iterator Value to be retrieved
  ATOM(erocksdb::ATOM_KEYS_ONLY, "keys_only");

  // Related to Access Hint
  ATOM(erocksdb::ATOM_ACCESS_HINT_NORMAL,"normal");
  ATOM(erocksdb::ATOM_ACCESS_HINT_SEQUENTIAL,"sequential");
  ATOM(erocksdb::ATOM_ACCESS_HINT_WILLNEED,"willneed");
  ATOM(erocksdb::ATOM_ACCESS_HINT_NONE,"none");

  // Related to Compression Type
  ATOM(erocksdb::ATOM_COMPRESSION_TYPE_SNAPPY, "snappy");
  ATOM(erocksdb::ATOM_COMPRESSION_TYPE_ZLIB, "zlib");
  ATOM(erocksdb::ATOM_COMPRESSION_TYPE_BZIP2, "bzip2");
  ATOM(erocksdb::ATOM_COMPRESSION_TYPE_LZ4, "lz4");
  ATOM(erocksdb::ATOM_COMPRESSION_TYPE_LZ4H, "lz4h");
  ATOM(erocksdb::ATOM_COMPRESSION_TYPE_NONE, "none");

  // Related to Compaction Style
  ATOM(erocksdb::ATOM_COMPACTION_STYLE_LEVEL, "level");
  ATOM(erocksdb::ATOM_COMPACTION_STYLE_UNIVERSAL, "universal");
  ATOM(erocksdb::ATOM_COMPACTION_STYLE_FIFO, "fifo");
  ATOM(erocksdb::ATOM_COMPACTION_STYLE_NONE, "none");

  // Related to WAL Recovery Mode
  ATOM(erocksdb::ATOM_WAL_TOLERATE_CORRUPTED_TAIL_RECORDS, "tolerate_corrupted_tail_records");
  ATOM(erocksdb::ATOM_WAL_ABSOLUTE_CONSISTENCY, "absolute_consistency");
  ATOM(erocksdb::ATOM_WAL_POINT_IN_TIME_RECOVERY, "point_in_time_recovery");
  ATOM(erocksdb::ATOM_WAL_SKIP_ANY_CORRUPTED_RECORDS, "skip_any_corrupted_records");

  // Related to Error Codes
  ATOM(erocksdb::ATOM_ERROR_DB_OPEN,"db_open");
  ATOM(erocksdb::ATOM_ERROR_DB_PUT, "db_put");
  ATOM(erocksdb::ATOM_ERROR_DB_DELETE, "db_delete");
  ATOM(erocksdb::ATOM_ERROR_DB_WRITE, "db_write");
  ATOM(erocksdb::ATOM_ERROR_DB_DESTROY, "error_db_destroy");
  ATOM(erocksdb::ATOM_ERROR_DB_REPAIR, "error_db_repair");
  ATOM(erocksdb::ATOM_BAD_WRITE_ACTION, "bad_write_action");
  ATOM(erocksdb::ATOM_KEEP_RESOURCE_FAILED, "keep_resource_failed");
  ATOM(erocksdb::ATOM_ITERATOR_CLOSED, "iterator_closed");
  ATOM(erocksdb::ATOM_INVALID_ITERATOR, "invalid_iterator");

  // Related to NIF initialize parameters
  ATOM(erocksdb::ATOM_WRITE_THREADS, "write_threads");
#undef ATOM

  // read options that apply to global erocksdb environment
  if(enif_is_list(env, load_info))
  {
      erocksdb::DbOptions load_options;
      fold(env, load_info, parse_init_option, load_options);
      /* Spin up the thread pool, set up all private data: */
      erocksdb::PrivData *priv = new erocksdb::PrivData(load_options);
      *priv_data = priv;
      return 0;
  } 
  return 1;
}
catch(std::exception& e)
{
    /* Refuse to load the NIF module (I see no way right now to return a more specific exception
    or log extra information): */
    return -1;
}
catch(...)
{
    return -1;
}

extern "C" {
    ERL_NIF_INIT(rocksdb, nif_funcs, &on_load, NULL, &on_upgrade, &on_unload);
}
