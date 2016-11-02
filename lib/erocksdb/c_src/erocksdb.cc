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

#include "rocksdb/db.h"
#include "rocksdb/comparator.h"
#include "rocksdb/env.h"
#include "rocksdb/write_batch.h"
#include "rocksdb/cache.h"
#include "rocksdb/table.h"
#include "rocksdb/filter_policy.h"
#include "rocksdb/slice_transform.h"

#ifndef INCL_THREADING_H
    #include "threading.h"
#endif

#ifndef INCL_WORKITEMS_H
    #include "workitems.h"
#endif

#ifndef ATOMS_H
    #include "atoms.h"
#endif

#include "work_result.hpp"

#include "detail.hpp"

static ErlNifFunc nif_funcs[] =
{
    {"close", 1, erocksdb_close},
    {"iterator_close", 1, erocksdb_iterator_close},
    {"status", 2, erocksdb_status},
    {"destroy", 2, erocksdb_destroy},
    {"repair", 2, erocksdb_repair},
    {"is_empty", 1, erocksdb_is_empty},

    {"async_open", 4, erocksdb::async_open},
    {"async_write", 4, erocksdb::async_write},
    {"async_get", 4, erocksdb::async_get},

    {"async_snapshot", 2, erocksdb::async_snapshot},
    {"async_release_snapshot", 2, erocksdb::async_release_snapshot},
    {"async_checkpoint", 3, erocksdb::async_checkpoint},

    {"async_iterator", 3, erocksdb::async_iterator},
    {"async_iterator", 4, erocksdb::async_iterator},

    {"async_iterator_move", 3, erocksdb::async_iterator_move}

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
ERL_NIF_TERM ATOM_EXPANDED_COMPACTION_FACTOR;
ERL_NIF_TERM ATOM_SOURCE_COMPACTION_FACTOR;
ERL_NIF_TERM ATOM_MAX_GRANDPARENT_OVERLAP_FACTOR;
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

struct erocksdb_itr_handle;

class erocksdb_thread_pool;
class erocksdb_priv_data;



// Erlang helpers:
ERL_NIF_TERM error_einval(ErlNifEnv* env)
{
    return enif_make_tuple2(env, erocksdb::ATOM_ERROR, erocksdb::ATOM_EINVAL);
}

static ERL_NIF_TERM error_tuple(ErlNifEnv* env, ERL_NIF_TERM error, rocksdb::Status& status)
{
    ERL_NIF_TERM reason = enif_make_string(env, status.ToString().c_str(),
                                           ERL_NIF_LATIN1);
    return enif_make_tuple2(env, erocksdb::ATOM_ERROR,
                            enif_make_tuple2(env, error, reason));
}

static ERL_NIF_TERM slice_to_binary(ErlNifEnv* env, rocksdb::Slice s)
{
    ERL_NIF_TERM result;
    unsigned char* value = enif_make_new_binary(env, s.size(), &result);
    memcpy(value, s.data(), s.size());
    return result;
}

/** struct for grabbing erocksdb environment options via fold
 *   ... then loading said options into erocksdb_priv_data
 */
struct ErocksdbOptions
{
    int m_ErocksdbThreads;

    ErocksdbOptions()
        : m_ErocksdbThreads(71)
        {};

    void Dump()
    {
        syslog(LOG_ERR, "         m_ErocksdbThreads: %d\n", m_ErocksdbThreads);
    }   // Dump
};  // struct ErocksdbOptions


static int kCapacity = 4194304; // default values, can be overridden

/** Module-level private data:
 *    singleton instance held by erlang and passed on API calls
 */
class erocksdb_priv_data
{
public:
    ErocksdbOptions m_Opts;
    erocksdb::erocksdb_thread_pool thread_pool;
    std::shared_ptr<rocksdb::Cache> block_cache;

    explicit erocksdb_priv_data(ErocksdbOptions & Options)
    : m_Opts(Options), thread_pool(Options.m_ErocksdbThreads)
        { block_cache = rocksdb::NewLRUCache(kCapacity); }

private:
    erocksdb_priv_data();                                      // no default constructor
    erocksdb_priv_data(const erocksdb_priv_data&);             // nocopy
    erocksdb_priv_data& operator=(const erocksdb_priv_data&);  // nocopyassign

};


ERL_NIF_TERM parse_init_option(ErlNifEnv* env, ERL_NIF_TERM item, ErocksdbOptions& opts)
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
                    opts.m_ErocksdbThreads = temp;
                }   // if
            }   // if
        }   // if
    }

    return erocksdb::ATOM_OK;
}

ERL_NIF_TERM parse_bbt_option(ErlNifEnv* env, ERL_NIF_TERM item, rocksdb::BlockBasedTableOptions& opts) {
    int arity;
    const ERL_NIF_TERM* option;
    if (enif_get_tuple(env, item, &arity, &option) && 2==arity)
    {
        if (option[0] == erocksdb::ATOM_NO_BLOCK_CACHE) {
            opts.no_block_cache = (option[1] == erocksdb::ATOM_TRUE);
        } else if (option[0] == erocksdb::ATOM_BLOCK_SIZE) {
            int block_size;
            if (enif_get_int(env, option[1], &block_size))
                opts.block_size = block_size;
        } else if (option[0] == erocksdb::ATOM_BLOCK_CACHE_SIZE) {
            ErlNifUInt64 block_cache_size;
            if (enif_get_uint64(env, option[1], &block_cache_size)) {
                erocksdb_priv_data& priv = *static_cast<erocksdb_priv_data *>(enif_priv_data(env));
                priv.block_cache->SetCapacity(block_cache_size);
                opts.block_cache = priv.block_cache;
            }
        } else if (option[0] == erocksdb::ATOM_BLOOM_FILTER_POLICY) {
            int bits_per_key;
            if (enif_get_int(env, option[1], &bits_per_key))
                opts.filter_policy = std::shared_ptr<const rocksdb::FilterPolicy>(rocksdb::NewBloomFilterPolicy(bits_per_key));
        } else if (option[0] == erocksdb::ATOM_FORMAT_VERSION) {
            int format_version;
            if (enif_get_int(env, option[1], &format_version))
                opts.format_version = format_version;
        } else if (option[0] == erocksdb::ATOM_SKIP_TABLE_BUILDER_FLUSH) {
            opts.skip_table_builder_flush = (option[1] == erocksdb::ATOM_TRUE);
        } else if (option[0] == erocksdb::ATOM_CACHE_INDEX_AND_FILTER_BLOCKS) {
            opts.cache_index_and_filter_blocks = (option[1] == erocksdb::ATOM_TRUE);
        }
    }

    return erocksdb::ATOM_OK;
}

ERL_NIF_TERM parse_db_option(ErlNifEnv* env, ERL_NIF_TERM item, rocksdb::Options& opts)
{
    int arity;
    const ERL_NIF_TERM* option;
    if (enif_get_tuple(env, item, &arity, &option) && 2==arity)
    {
        if (option[0] == erocksdb::ATOM_TOTAL_THREADS)
        {
            int total_threads;
            if (enif_get_int(env, option[1], &total_threads))
                opts.IncreaseParallelism(total_threads);
        }
        else if (option[0] == erocksdb::ATOM_CREATE_IF_MISSING)
            opts.create_if_missing = (option[1] == erocksdb::ATOM_TRUE);
        else if (option[0] == erocksdb::ATOM_CREATE_MISSING_COLUMN_FAMILIES)
            opts.create_missing_column_families = (option[1] == erocksdb::ATOM_TRUE);
        else if (option[0] == erocksdb::ATOM_ERROR_IF_EXISTS)
            opts.error_if_exists = (option[1] == erocksdb::ATOM_TRUE);
        else if (option[0] == erocksdb::ATOM_PARANOID_CHECKS)
            opts.paranoid_checks = (option[1] == erocksdb::ATOM_TRUE);
        else if (option[0] == erocksdb::ATOM_MAX_OPEN_FILES)
        {
            int max_open_files;
            if (enif_get_int(env, option[1], &max_open_files))
                opts.max_open_files = max_open_files;
        }
        else if (option[0] == erocksdb::ATOM_MAX_TOTAL_WAL_SIZE)
        {
            ErlNifUInt64 max_total_wal_size;
            if (enif_get_uint64(env, option[1], &max_total_wal_size))
                opts.max_total_wal_size = max_total_wal_size;
        }
        else if (option[0] == erocksdb::ATOM_DISABLE_DATA_SYNC)
        {
            opts.disableDataSync = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_USE_FSYNC)
        {
            opts.use_fsync = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_DB_PATHS)
        {
            ERL_NIF_TERM head;
            ERL_NIF_TERM tail;
            char db_name[4096];
            while(enif_get_list_cell(env, option[1], &head, &tail)) {
                if (enif_get_string(env, head, db_name, sizeof(db_name), ERL_NIF_LATIN1))
                {
                    std::string str_db_name(db_name);
                    rocksdb::DbPath db_path(str_db_name, 0);
                    opts.db_paths.push_back(db_path);
                }
            }
        }
        else if (option[0] == erocksdb::ATOM_DB_LOG_DIR)
        {
            char db_log_dir[4096];
            if (enif_get_string(env, option[1], db_log_dir, sizeof(db_log_dir), ERL_NIF_LATIN1))
                opts.db_log_dir = std::string(db_log_dir);
        }
        else if (option[0] == erocksdb::ATOM_WAL_DIR)
        {
            char wal_dir[4096];
            if (enif_get_string(env, option[1], wal_dir, sizeof(wal_dir), ERL_NIF_LATIN1))
                opts.wal_dir = std::string(wal_dir);
        }
        else if (option[0] == erocksdb::ATOM_DELETE_OBSOLETE_FILES_PERIOD_MICROS)
        {
            ErlNifUInt64 delete_obsolete_files_period_micros;
            if (enif_get_uint64(env, option[1], &delete_obsolete_files_period_micros))
                opts.delete_obsolete_files_period_micros = delete_obsolete_files_period_micros;
        }
        else if (option[0] == erocksdb::ATOM_MAX_BACKGROUND_COMPACTIONS)
        {
            int max_background_compactions;
            if (enif_get_int(env, option[1], &max_background_compactions))
                opts.max_background_compactions = max_background_compactions;
        }
        else if (option[0] == erocksdb::ATOM_MAX_BACKGROUND_FLUSHES)
        {
            int max_background_flushes;
            if (enif_get_int(env, option[1], &max_background_flushes))
                opts.max_background_flushes = max_background_flushes;
        }
        else if (option[0] == erocksdb::ATOM_MAX_LOG_FILE_SIZE)
        {
            unsigned int max_log_file_size;
            if (enif_get_uint(env, option[1], &max_log_file_size))
                opts.max_log_file_size = max_log_file_size;
        }
        else if (option[0] == erocksdb::ATOM_LOG_FILE_TIME_TO_ROLL)
        {
            unsigned int log_file_time_to_roll;
            if (enif_get_uint(env, option[1], &log_file_time_to_roll))
                opts.log_file_time_to_roll = log_file_time_to_roll;
        }
        else if (option[0] == erocksdb::ATOM_KEEP_LOG_FILE_NUM)
        {
            unsigned int keep_log_file_num;
            if (enif_get_uint(env, option[1], &keep_log_file_num))
                opts.keep_log_file_num= keep_log_file_num;
        }
        else if (option[0] == erocksdb::ATOM_MAX_MANIFEST_FILE_SIZE)
        {
            ErlNifUInt64 max_manifest_file_size;
            if (enif_get_uint64(env, option[1], &max_manifest_file_size))
                opts.max_manifest_file_size = max_manifest_file_size;
        }
        else if (option[0] == erocksdb::ATOM_TABLE_CACHE_NUMSHARDBITS)
        {
            int table_cache_numshardbits;
            if (enif_get_int(env, option[1], &table_cache_numshardbits))
                opts.table_cache_numshardbits = table_cache_numshardbits;
        }
        else if (option[0] == erocksdb::ATOM_WAL_TTL_SECONDS)
        {
            ErlNifUInt64 WAL_ttl_seconds;
            if (enif_get_uint64(env, option[1], &WAL_ttl_seconds))
                opts.WAL_ttl_seconds = WAL_ttl_seconds;
        }
        else if (option[0] == erocksdb::ATOM_WAL_SIZE_LIMIT_MB)
        {
            ErlNifUInt64 WAL_size_limit_MB;
            if (enif_get_uint64(env, option[1], &WAL_size_limit_MB))
                opts.WAL_size_limit_MB = WAL_size_limit_MB;
        }
        else if (option[0] == erocksdb::ATOM_MANIFEST_PREALLOCATION_SIZE)
        {
            unsigned int manifest_preallocation_size;
            if (enif_get_uint(env, option[1], &manifest_preallocation_size))
                opts.manifest_preallocation_size = manifest_preallocation_size;
        }
        else if (option[0] == erocksdb::ATOM_ALLOW_OS_BUFFER)
        {
            opts.allow_os_buffer = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_ALLOW_MMAP_READS)
        {
            opts.allow_mmap_reads = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_ALLOW_MMAP_WRITES)
        {
            opts.allow_mmap_writes = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_IS_FD_CLOSE_ON_EXEC)
        {
            opts.is_fd_close_on_exec = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_SKIP_LOG_ERROR_ON_RECOVERY)
        {
            opts.skip_log_error_on_recovery = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_STATS_DUMP_PERIOD_SEC)
        {
            unsigned int stats_dump_period_sec;
            if (enif_get_uint(env, option[1], &stats_dump_period_sec))
                opts.stats_dump_period_sec = stats_dump_period_sec;
        }
        else if (option[0] == erocksdb::ATOM_ADVISE_RANDOM_ON_OPEN)
        {
            opts.advise_random_on_open = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_ACCESS_HINT)
        {
            if (option[1] == erocksdb::ATOM_ACCESS_HINT_NORMAL) {
                opts.access_hint_on_compaction_start = rocksdb::DBOptions::AccessHint::NORMAL;
            }
            else if (option[1] == erocksdb::ATOM_ACCESS_HINT_SEQUENTIAL) {
                opts.access_hint_on_compaction_start = rocksdb::DBOptions::AccessHint::SEQUENTIAL;
            }
            else if (option[1] == erocksdb::ATOM_ACCESS_HINT_WILLNEED) {
                opts.access_hint_on_compaction_start = rocksdb::DBOptions::AccessHint::WILLNEED;
            }
            else if (option[1] == erocksdb::ATOM_ACCESS_HINT_NONE) {
                opts.access_hint_on_compaction_start = rocksdb::DBOptions::AccessHint::NONE;
            }
        }
        else if (option[0] == erocksdb::ATOM_COMPACTION_READAHEAD_SIZE)
        {
            unsigned int compaction_readahead_size;
            if (enif_get_uint(env, option[1], &compaction_readahead_size))
                opts.compaction_readahead_size = compaction_readahead_size;
        }
        else if (option[0] == erocksdb::ATOM_USE_ADAPTIVE_MUTEX)
        {
            opts.use_adaptive_mutex = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_BYTES_PER_SYNC)
        {
            ErlNifUInt64 bytes_per_sync;
            if (enif_get_uint64(env, option[1], &bytes_per_sync))
                opts.bytes_per_sync = bytes_per_sync;
        }
        else if (option[0] == erocksdb::ATOM_SKIP_STATS_UPDATE_ON_DB_OPEN)
        {
            opts.skip_stats_update_on_db_open = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_WAL_RECOVERY_MODE)
        {
            if (option[1] == erocksdb::ATOM_WAL_TOLERATE_CORRUPTED_TAIL_RECORDS) {
                opts.wal_recovery_mode = rocksdb::WALRecoveryMode::kTolerateCorruptedTailRecords;
            }
            else if (option[1] == erocksdb::ATOM_WAL_ABSOLUTE_CONSISTENCY) {
                opts.wal_recovery_mode = rocksdb::WALRecoveryMode::kAbsoluteConsistency;
            }
            else if (option[1] == erocksdb::ATOM_WAL_POINT_IN_TIME_RECOVERY) {
                opts.wal_recovery_mode = rocksdb::WALRecoveryMode::kPointInTimeRecovery;
            }
            else if (option[1] == erocksdb::ATOM_WAL_SKIP_ANY_CORRUPTED_RECORDS) {
                opts.wal_recovery_mode = rocksdb::WALRecoveryMode::kSkipAnyCorruptedRecords;
            }
        }
        else if (option[0] == erocksdb::ATOM_ALLOW_CONCURRENT_MEMTABLE_WRITE)
        {
            opts.allow_concurrent_memtable_write = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_ENABLE_WRITE_THREAD_ADAPTATIVE_YIELD)
        {
            opts.enable_write_thread_adaptive_yield = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_IN_MEMORY)
        {
            if (option[1] == erocksdb::ATOM_TRUE)
            {
                opts.env = rocksdb::NewMemEnv(rocksdb::Env::Default());
                opts.create_if_missing = true;
                opts.table_factory = std::shared_ptr<rocksdb::TableFactory>(rocksdb::NewPlainTableFactory());
            }
        }
    }

    return erocksdb::ATOM_OK;
}

ERL_NIF_TERM parse_cf_option(ErlNifEnv* env, ERL_NIF_TERM item, rocksdb::Options& opts)
{
    int arity;
    const ERL_NIF_TERM* option;
    if (enif_get_tuple(env, item, &arity, &option) && 2==arity)
    {
        if (option[0] == erocksdb::ATOM_BLOCK_CACHE_SIZE_MB_FOR_POINT_LOOKUP)
            // @TODO ignored now
            ;
        else if (option[0] == erocksdb::ATOM_MEMTABLE_MEMORY_BUDGET)
        {
            ErlNifUInt64 memtable_memory_budget;
            if (enif_get_uint64(env, option[1], &memtable_memory_budget))
                opts.OptimizeLevelStyleCompaction(memtable_memory_budget);
        }
        else if (option[0] == erocksdb::ATOM_WRITE_BUFFER_SIZE)
        {
            unsigned int write_buffer_size;
            if (enif_get_uint(env, option[1], &write_buffer_size))
                opts.write_buffer_size = write_buffer_size;
        }
        else if (option[0] == erocksdb::ATOM_MAX_WRITE_BUFFER_NUMBER)
        {
            int max_write_buffer_number;
            if (enif_get_int(env, option[1], &max_write_buffer_number))
                opts.max_write_buffer_number = max_write_buffer_number;
        }
        else if (option[0] == erocksdb::ATOM_MIN_WRITE_BUFFER_NUMBER_TO_MERGE)
        {
            int min_write_buffer_number_to_merge;
            if (enif_get_int(env, option[1], &min_write_buffer_number_to_merge))
                opts.min_write_buffer_number_to_merge = min_write_buffer_number_to_merge;
        }
        else if (option[0] == erocksdb::ATOM_COMPRESSION)
        {
            if (option[1] == erocksdb::ATOM_COMPRESSION_TYPE_SNAPPY) {
                opts.compression = rocksdb::CompressionType::kSnappyCompression;
            }
            else if (option[1] == erocksdb::ATOM_COMPRESSION_TYPE_ZLIB) {
                opts.compression = rocksdb::CompressionType::kZlibCompression;
            }
            else if (option[1] == erocksdb::ATOM_COMPRESSION_TYPE_BZIP2) {
                opts.compression = rocksdb::CompressionType::kBZip2Compression;
            }
            else if (option[1] == erocksdb::ATOM_COMPRESSION_TYPE_LZ4) {
                opts.compression = rocksdb::CompressionType::kLZ4Compression;
            }
            else if (option[1] == erocksdb::ATOM_COMPRESSION_TYPE_LZ4H) {
                opts.compression = rocksdb::CompressionType::kLZ4HCCompression;
            }
            else if (option[1] == erocksdb::ATOM_COMPRESSION_TYPE_NONE) {
                opts.compression = rocksdb::CompressionType::kNoCompression;
            }
        }
        else if (option[0] == erocksdb::ATOM_NUM_LEVELS)
        {
            int num_levels;
            if (enif_get_int(env, option[1], &num_levels))
                opts.num_levels = num_levels;
        }
        else if (option[0] == erocksdb::ATOM_LEVEL0_FILE_NUM_COMPACTION_TRIGGER)
        {
            int level0_file_num_compaction_trigger;
            if (enif_get_int(env, option[1], &level0_file_num_compaction_trigger))
                opts.level0_file_num_compaction_trigger = level0_file_num_compaction_trigger;
        }
        else if (option[0] == erocksdb::ATOM_LEVEL0_SLOWDOWN_WRITES_TRIGGER)
        {
            int level0_slowdown_writes_trigger;
            if (enif_get_int(env, option[1], &level0_slowdown_writes_trigger))
                opts.level0_slowdown_writes_trigger = level0_slowdown_writes_trigger;
        }
        else if (option[0] == erocksdb::ATOM_LEVEL0_STOP_WRITES_TRIGGER)
        {
            int level0_stop_writes_trigger;
            if (enif_get_int(env, option[1], &level0_stop_writes_trigger))
                opts.level0_stop_writes_trigger = level0_stop_writes_trigger;
        }
        else if (option[0] == erocksdb::ATOM_MAX_MEM_COMPACTION_LEVEL)
        {
            int max_mem_compaction_level;
            if (enif_get_int(env, option[1], &max_mem_compaction_level))
                opts.max_mem_compaction_level = max_mem_compaction_level;
        }
        else if (option[0] == erocksdb::ATOM_TARGET_FILE_SIZE_BASE)
        {
            ErlNifUInt64 target_file_size_base;
            if (enif_get_uint64(env, option[1], &target_file_size_base))
                opts.target_file_size_base = target_file_size_base;
        }
        else if (option[0] == erocksdb::ATOM_TARGET_FILE_SIZE_MULTIPLIER)
        {
            int target_file_size_multiplier;
            if (enif_get_int(env, option[1], &target_file_size_multiplier))
                opts.target_file_size_multiplier = target_file_size_multiplier;
        }
        else if (option[0] == erocksdb::ATOM_MAX_BYTES_FOR_LEVEL_BASE)
        {
            ErlNifUInt64 max_bytes_for_level_base;
            if (enif_get_uint64(env, option[1], &max_bytes_for_level_base))
                opts.max_bytes_for_level_base = max_bytes_for_level_base;
        }
        else if (option[0] == erocksdb::ATOM_MAX_BYTES_FOR_LEVEL_MULTIPLIER)
        {
            int max_bytes_for_level_multiplier;
            if (enif_get_int(env, option[1], &max_bytes_for_level_multiplier))
                opts.max_bytes_for_level_multiplier = max_bytes_for_level_multiplier;
        }
        else if (option[0] == erocksdb::ATOM_EXPANDED_COMPACTION_FACTOR)
        {
            int expanded_compaction_factor;
            if (enif_get_int(env, option[1], &expanded_compaction_factor))
                opts.expanded_compaction_factor = expanded_compaction_factor;
        }
        else if (option[0] == erocksdb::ATOM_SOURCE_COMPACTION_FACTOR)
        {
            int source_compaction_factor;
            if (enif_get_int(env, option[1], &source_compaction_factor))
                opts.source_compaction_factor = source_compaction_factor;
        }
        else if (option[0] == erocksdb::ATOM_MAX_GRANDPARENT_OVERLAP_FACTOR)
        {
            int max_grandparent_overlap_factor;
            if (enif_get_int(env, option[1], &max_grandparent_overlap_factor))
                opts.max_grandparent_overlap_factor = max_grandparent_overlap_factor;
        }
        else if (option[0] == erocksdb::ATOM_SOFT_RATE_LIMIT)
        {
            double soft_rate_limit;
            if (enif_get_double(env, option[1], &soft_rate_limit))
                opts.soft_rate_limit = soft_rate_limit;
        }
        else if (option[0] == erocksdb::ATOM_HARD_RATE_LIMIT)
        {
            double hard_rate_limit;
            if (enif_get_double(env, option[1], &hard_rate_limit))
                opts.hard_rate_limit = hard_rate_limit;
        }
        else if (option[0] == erocksdb::ATOM_ARENA_BLOCK_SIZE)
        {
            unsigned int arena_block_size;
            if (enif_get_uint(env, option[1], &arena_block_size))
                opts.arena_block_size = arena_block_size;
        }
        else if (option[0] == erocksdb::ATOM_DISABLE_AUTO_COMPACTIONS)
        {
            opts.disable_auto_compactions = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_PURGE_REDUNDANT_KVS_WHILE_FLUSH)
        {
            opts.purge_redundant_kvs_while_flush = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_COMPACTION_STYLE)
        {
            if (option[1] == erocksdb::ATOM_COMPACTION_STYLE_LEVEL) {
                opts.compaction_style = rocksdb::CompactionStyle::kCompactionStyleLevel;
            }
            else if (option[1] == erocksdb::ATOM_COMPACTION_STYLE_UNIVERSAL) {
                opts.compaction_style = rocksdb::CompactionStyle::kCompactionStyleUniversal;
            }
            else if (option[1] == erocksdb::ATOM_COMPACTION_STYLE_FIFO) {
                opts.compaction_style = rocksdb::CompactionStyle::kCompactionStyleFIFO;
            }
            else if (option[1] == erocksdb::ATOM_COMPACTION_STYLE_NONE) {
                opts.compaction_style = rocksdb::CompactionStyle::kCompactionStyleNone;
            }
        }
        else if (option[0] == erocksdb::ATOM_VERIFY_CHECKSUMS_IN_COMPACTION)
        {
            opts.verify_checksums_in_compaction = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_MAX_SEQUENTIAL_SKIP_IN_ITERATIONS)
        {
            ErlNifUInt64 max_sequential_skip_in_iterations;
            if (enif_get_uint64(env, option[1], &max_sequential_skip_in_iterations))
                opts.max_sequential_skip_in_iterations = max_sequential_skip_in_iterations;
        }
        else if (option[0] == erocksdb::ATOM_INPLACE_UPDATE_SUPPORT)
        {
            opts.inplace_update_support = (option[1] == erocksdb::ATOM_TRUE);
        }
        else if (option[0] == erocksdb::ATOM_INPLACE_UPDATE_NUM_LOCKS)
        {
            unsigned int inplace_update_num_locks;
            if (enif_get_uint(env, option[1], &inplace_update_num_locks))
                opts.inplace_update_num_locks= inplace_update_num_locks;
        }
        else if (option[0] == erocksdb::ATOM_TABLE_FACTORY_BLOCK_CACHE_SIZE)
        {
            ErlNifUInt64 table_factory_block_cache_size;
            if (enif_get_uint64(env, option[1], &table_factory_block_cache_size))
            {
                erocksdb_priv_data& priv = *static_cast<erocksdb_priv_data *>(enif_priv_data(env));
                priv.block_cache->SetCapacity(table_factory_block_cache_size);
                rocksdb::BlockBasedTableOptions bbtOpts;
                bbtOpts.block_cache = priv.block_cache;
                bbtOpts.filter_policy = std::shared_ptr<const rocksdb::FilterPolicy>(rocksdb::NewBloomFilterPolicy(10));

                opts.table_factory = std::shared_ptr<rocksdb::TableFactory>(rocksdb::NewBlockBasedTableFactory(bbtOpts));
            }
        }
        else if (option[0] == erocksdb::ATOM_BLOCK_BASED_TABLE_OPTIONS) {
            rocksdb::BlockBasedTableOptions bbtOpts;
            fold(env, option[1], parse_bbt_option, bbtOpts);
            opts.table_factory = std::shared_ptr<rocksdb::TableFactory>(rocksdb::NewBlockBasedTableFactory(bbtOpts));
        }
        else if (option[0] == erocksdb::ATOM_IN_MEMORY_MODE)
        {
            if (option[1] == erocksdb::ATOM_TRUE)
            {
                // Set recommended defaults
                opts.prefix_extractor = std::shared_ptr<const rocksdb::SliceTransform>(rocksdb::NewFixedPrefixTransform(10));
                opts.table_factory = std::shared_ptr<rocksdb::TableFactory>(rocksdb::NewPlainTableFactory());
                opts.allow_mmap_reads = true;
                opts.compression = rocksdb::CompressionType::kNoCompression;
                opts.memtable_prefix_bloom_size_ratio = 0.06;
                opts.compaction_style = rocksdb::CompactionStyle::kCompactionStyleUniversal;
                opts.compaction_options_universal.size_ratio = 10;
                opts.compaction_options_universal.min_merge_width = 2;
                opts.compaction_options_universal.max_size_amplification_percent = 50;
                opts.level0_file_num_compaction_trigger = 0;
                opts.level0_slowdown_writes_trigger = 8;
                opts.level0_stop_writes_trigger = 16;
                opts.bloom_locality = 1;
                opts.max_open_files = -1;
                opts.write_buffer_size = 32 << 20;
                opts.max_write_buffer_number = 2;
                opts.min_write_buffer_number_to_merge = 1;
                opts.disableDataSync = 1;
                opts.bytes_per_sync = 2 << 20;
            }
        }
    }
    else if (option[0] == erocksdb::ATOM_IN_MEMORY)
    {
        if (option[1] == erocksdb::ATOM_TRUE)
        {
            opts.env = rocksdb::NewMemEnv(rocksdb::Env::Default());
            opts.create_if_missing = true;
            opts.table_factory = std::shared_ptr<rocksdb::TableFactory>(rocksdb::NewPlainTableFactory());
        }
    }
    return erocksdb::ATOM_OK;
}

ERL_NIF_TERM parse_read_option(ErlNifEnv* env, ERL_NIF_TERM item, rocksdb::ReadOptions& opts)
{
    int arity;
    const ERL_NIF_TERM* option;
    if (enif_get_tuple(env, item, &arity, &option) && 2==arity)
    {
        if (option[0] == erocksdb::ATOM_VERIFY_CHECKSUMS)
            opts.verify_checksums = (option[1] == erocksdb::ATOM_TRUE);
        else if (option[0] == erocksdb::ATOM_FILL_CACHE)
            opts.fill_cache = (option[1] == erocksdb::ATOM_TRUE);
        else if (option[0] == erocksdb::ATOM_ITERATE_UPPER_BOUND)
            // @TODO Who should be the Slice owner?
            ;
        else if (option[0] == erocksdb::ATOM_TAILING)
            opts.tailing = (option[1] == erocksdb::ATOM_TRUE);
        else if (option[0] == erocksdb::ATOM_TOTAL_ORDER_SEEK)
            opts.total_order_seek = (option[1] == erocksdb::ATOM_TRUE);
        else if (option[0] == erocksdb::ATOM_SNAPSHOT)
        {
            erocksdb::ReferencePtr<erocksdb::SnapshotObject> snapshot_ptr;
            snapshot_ptr.assign(erocksdb::SnapshotObject::RetrieveSnapshotObject(env, option[1]));

            if(NULL==snapshot_ptr.get())
                return erocksdb::ATOM_BADARG;

            opts.snapshot = snapshot_ptr->m_Snapshot;
        }
    }

    return erocksdb::ATOM_OK;
}

ERL_NIF_TERM parse_write_option(ErlNifEnv* env, ERL_NIF_TERM item, rocksdb::WriteOptions& opts)
{
    int arity;
    const ERL_NIF_TERM* option;
    if (enif_get_tuple(env, item, &arity, &option) && 2==arity)
    {
        if (option[0] == erocksdb::ATOM_SYNC)
            opts.sync = (option[1] == erocksdb::ATOM_TRUE);
        else if (option[0] == erocksdb::ATOM_DISABLE_WAL)
            opts.disableWAL = (option[1] == erocksdb::ATOM_TRUE);
        else if (option[0] == erocksdb::ATOM_TIMEOUT_HINT_US)
        {
            ErlNifUInt64 timeout_hint_us;
            if (enif_get_uint64(env, option[1], &timeout_hint_us))
                opts.timeout_hint_us = timeout_hint_us;
        }
        else if (option[0] == erocksdb::ATOM_IGNORE_MISSING_COLUMN_FAMILIES)
            opts.ignore_missing_column_families = (option[1] == erocksdb::ATOM_TRUE);
    }

    return erocksdb::ATOM_OK;
}

ERL_NIF_TERM write_batch_item(ErlNifEnv* env, ERL_NIF_TERM item, rocksdb::WriteBatch& batch)
{
    int arity;
    const ERL_NIF_TERM* action;
    if (enif_get_tuple(env, item, &arity, &action) ||
        enif_is_atom(env, item))
    {
        if (item == erocksdb::ATOM_CLEAR)
        {
            batch.Clear();
            return erocksdb::ATOM_OK;
        }

        ErlNifBinary key, value;

        if (action[0] == erocksdb::ATOM_PUT && arity == 3 &&
            enif_inspect_binary(env, action[1], &key) &&
            enif_inspect_binary(env, action[2], &value))
        {
            rocksdb::Slice key_slice((const char*)key.data, key.size);
            rocksdb::Slice value_slice((const char*)value.data, value.size);
            batch.Put(key_slice, value_slice);
            return erocksdb::ATOM_OK;
        }

        if (action[0] == erocksdb::ATOM_DELETE && arity == 2 &&
            enif_inspect_binary(env, action[1], &key))
        {
            rocksdb::Slice key_slice((const char*)key.data, key.size);
            batch.Delete(key_slice);
            return erocksdb::ATOM_OK;
        }
    }

    // Failed to match clear/put/delete; return the failing item
    return item;
}



namespace erocksdb {

ERL_NIF_TERM send_reply(ErlNifEnv *env, ERL_NIF_TERM ref, ERL_NIF_TERM reply)
{
    ErlNifPid pid;
    ErlNifEnv *msg_env = enif_alloc_env();
    ERL_NIF_TERM msg = enif_make_tuple2(msg_env,
                                        enif_make_copy(msg_env, ref),
                                        enif_make_copy(msg_env, reply));
    enif_self(env, &pid);
    enif_send(env, &pid, msg_env, msg);
    enif_free_env(msg_env);
    return ATOM_OK;
}

ERL_NIF_TERM
async_open(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    char db_name[4096];

    if(!enif_get_string(env, argv[1], db_name, sizeof(db_name), ERL_NIF_LATIN1) ||
       !enif_is_list(env, argv[2]) ||
       !enif_is_list(env, argv[3]))
    {
        return enif_make_badarg(env);
    }   // if

    ERL_NIF_TERM caller_ref = argv[0];

    erocksdb_priv_data& priv = *static_cast<erocksdb_priv_data *>(enif_priv_data(env));

    rocksdb::Options *opts = new rocksdb::Options;
    fold(env, argv[2], parse_db_option, *opts);
    fold(env, argv[3], parse_cf_option, *opts);

    erocksdb::WorkTask *work_item = new erocksdb::OpenTask(env, caller_ref, db_name, opts);

    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                          enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }

    return erocksdb::ATOM_OK;

}   // async_open


ERL_NIF_TERM
async_snapshot(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{

    const ERL_NIF_TERM& caller_ref = argv[0];
    const ERL_NIF_TERM& handle_ref = argv[1];

    ReferencePtr<DbObject> db_ptr;

    db_ptr.assign(DbObject::RetrieveDbObject(env, handle_ref));

    if(NULL==db_ptr.get())
    {
        return enif_make_badarg(env);
    }

    // is this even possible?
    if(NULL == db_ptr->m_Db)
        return send_reply(env, caller_ref, error_einval(env));

    erocksdb_priv_data& priv = *static_cast<erocksdb_priv_data *>(enif_priv_data(env));

    erocksdb::WorkTask* work_item = new erocksdb::GetSnapshotTask(env, caller_ref, db_ptr.get());

    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }

    return erocksdb::ATOM_OK;
}   // async_snapshot


ERL_NIF_TERM
async_release_snapshot(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{

    const ERL_NIF_TERM& caller_ref = argv[0];
    const ERL_NIF_TERM& handle_ref = argv[1];

    ReferencePtr<SnapshotObject> snapshot_ptr;

    snapshot_ptr.assign(SnapshotObject::RetrieveSnapshotObject(env, handle_ref));

    if(NULL==snapshot_ptr.get())
    {
        return send_reply(env, caller_ref, erocksdb::ATOM_OK);
    }

    erocksdb_priv_data& priv = *static_cast<erocksdb_priv_data *>(enif_priv_data(env));

    erocksdb::WorkTask* work_item = new erocksdb::ReleaseSnapshotTask(env, caller_ref, snapshot_ptr.get());

    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                          enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }

    return erocksdb::ATOM_OK;
}   // async_snapshot


ERL_NIF_TERM
async_write(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM& caller_ref = argv[0];
    const ERL_NIF_TERM& handle_ref = argv[1];
    const ERL_NIF_TERM& action_ref = argv[2];
    const ERL_NIF_TERM& opts_ref   = argv[3];

    ReferencePtr<DbObject> db_ptr;

    db_ptr.assign(DbObject::RetrieveDbObject(env, handle_ref));

    if(NULL==db_ptr.get()
       || !enif_is_list(env, action_ref)
       || !enif_is_list(env, opts_ref))
    {
        return enif_make_badarg(env);
    }

    // is this even possible?
    if(NULL == db_ptr->m_Db)
        return send_reply(env, caller_ref, error_einval(env));

    erocksdb_priv_data& priv = *static_cast<erocksdb_priv_data *>(enif_priv_data(env));

    // Construct a write batch:
    rocksdb::WriteBatch* batch = new rocksdb::WriteBatch;

    // Seed the batch's data:
    ERL_NIF_TERM result = fold(env, argv[2], write_batch_item, *batch);
    if(erocksdb::ATOM_OK != result)
    {
        return send_reply(env, caller_ref,
                          enif_make_tuple3(env, erocksdb::ATOM_ERROR, caller_ref,
                                           enif_make_tuple2(env, erocksdb::ATOM_BAD_WRITE_ACTION,
                                                            result)));
    }   // if

    rocksdb::WriteOptions* opts = new rocksdb::WriteOptions;
    fold(env, argv[3], parse_write_option, *opts);

    erocksdb::WorkTask* work_item = new erocksdb::WriteTask(env, caller_ref,
                                                            db_ptr.get(), batch, opts);

    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                          enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }   // if

    return erocksdb::ATOM_OK;
}


ERL_NIF_TERM
async_get(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM& caller_ref = argv[0];
    const ERL_NIF_TERM& dbh_ref    = argv[1];
    const ERL_NIF_TERM& key_ref    = argv[2];
    const ERL_NIF_TERM& opts_ref   = argv[3];

    rocksdb::ReadOptions *opts = new rocksdb::ReadOptions();

    ReferencePtr<DbObject> db_ptr;
    db_ptr.assign(DbObject::RetrieveDbObject(env, dbh_ref));

    if(NULL==db_ptr.get()
       || !enif_is_list(env, opts_ref)
       || !enif_is_binary(env, key_ref))
    {
        return enif_make_badarg(env);
    }

    if(NULL == db_ptr->m_Db)
        return send_reply(env, caller_ref, error_einval(env));

    ERL_NIF_TERM fold_result;

    fold_result = fold(env, opts_ref, parse_read_option, *opts);
    if(fold_result!=erocksdb::ATOM_OK)
        return enif_make_badarg(env);

    erocksdb::WorkTask *work_item = new erocksdb::GetTask(env, caller_ref,
                                                          db_ptr.get(), key_ref, opts);

    erocksdb_priv_data& priv = *static_cast<erocksdb_priv_data *>(enif_priv_data(env));

    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                          enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }   // if

    return erocksdb::ATOM_OK;

}   // async_get


ERL_NIF_TERM
async_iterator(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM& caller_ref  = argv[0];
    const ERL_NIF_TERM& dbh_ref     = argv[1];
    const ERL_NIF_TERM& options_ref = argv[2];

    const bool keys_only = ((argc == 4) && (argv[3] == ATOM_KEYS_ONLY));

    rocksdb::ReadOptions *opts = new rocksdb::ReadOptions;

    ReferencePtr<DbObject> db_ptr;
    db_ptr.assign(DbObject::RetrieveDbObject(env, dbh_ref));


    if(NULL==db_ptr.get()
       || !enif_is_list(env, options_ref))
     {
        return enif_make_badarg(env);
     }

    // likely useless
    if(NULL == db_ptr->m_Db)
        return send_reply(env, caller_ref, error_einval(env));

    ERL_NIF_TERM fold_result;

    fold_result = fold(env, options_ref, parse_read_option, *opts);
    if(fold_result!=erocksdb::ATOM_OK)
        return enif_make_badarg(env);

    erocksdb::WorkTask *work_item = new erocksdb::IterTask(env, caller_ref,
                                                           db_ptr.get(), keys_only, opts);

    // Now-boilerplate setup (we'll consolidate this pattern soon, I hope):
    erocksdb_priv_data& priv = *static_cast<erocksdb_priv_data *>(enif_priv_data(env));

    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref, enif_make_tuple2(env, ATOM_ERROR, caller_ref));
    }   // if

    return ATOM_OK;

}   // async_iterator


ERL_NIF_TERM
async_iterator_move(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    // const ERL_NIF_TERM& caller_ref       = argv[0];
    const ERL_NIF_TERM& itr_handle_ref   = argv[1];
    const ERL_NIF_TERM& action_or_target = argv[2];
    ERL_NIF_TERM ret_term;

    bool submit_new_request(true);

    ReferencePtr<ItrObject> itr_ptr;

    itr_ptr.assign(ItrObject::RetrieveItrObject(env, itr_handle_ref));

    if(NULL==itr_ptr.get())
        return enif_make_badarg(env);

    // Reuse ref from iterator creation
    const ERL_NIF_TERM& caller_ref = itr_ptr->m_Iter->itr_ref;

    /* We can be invoked with two different arities from Erlang. If our "action_atom" parameter is not
       in fact an atom, then it is actually a seek target. Let's find out which we are: */
    erocksdb::MoveTask::action_t action = erocksdb::MoveTask::SEEK;

    // If we have an atom, it's one of these (action_or_target's value is ignored):
    if(enif_is_atom(env, action_or_target))
    {
        if(ATOM_FIRST == action_or_target)  action = erocksdb::MoveTask::FIRST;
        if(ATOM_LAST == action_or_target)   action = erocksdb::MoveTask::LAST;
        if(ATOM_NEXT == action_or_target)   action = erocksdb::MoveTask::NEXT;
        if(ATOM_PREV == action_or_target)   action = erocksdb::MoveTask::PREV;
        // if(ATOM_PREFETCH == action_or_target)   action = erocksdb::MoveTask::PREFETCH;
    }   // if


    //
    // Three situations:
    //  #1 not a PREFETCH next call
    //  #2 PREFETCH call and no prefetch waiting
    //  #3 PREFETCH call and prefetch is waiting

    // case #1
    if (erocksdb::MoveTask::PREFETCH != action)
    {
        // current move object could still be in later stages of
        //  worker thread completion ... race condition ...don't reuse
        itr_ptr->ReleaseReuseMove();

        submit_new_request=true;
        ret_term = enif_make_copy(env, itr_ptr->m_Iter->itr_ref);

        // force reply to be a message
        itr_ptr->m_Iter->m_HandoffAtomic=1;
    }   // if

    // case #2
    // before we launch a background job for "next iteration", see if there is a
    //  prefetch waiting for us
    else if (erocksdb::compare_and_swap(&itr_ptr->m_Iter->m_HandoffAtomic, 0, 1))
    {
        // nope, no prefetch ... await a message to erlang queue
        ret_term = enif_make_copy(env, itr_ptr->m_Iter->itr_ref);

        // is this truly a wait for prefetch ... or actually the first prefetch request
        if (!itr_ptr->m_Iter->m_PrefetchStarted)
        {
            submit_new_request=true;
            itr_ptr->m_Iter->m_PrefetchStarted=true;
            itr_ptr->ReleaseReuseMove();

            // first must return via message
            itr_ptr->m_Iter->m_HandoffAtomic=1;
        }   // if

        else
        {
            // await message that is already in the making
            submit_new_request=false;
        }   // else
    }   // else if

    // case #3
    else
    {
        // why yes there is.  copy the key/value info into a return tuple before
        //  we launch the iterator for "next" again
        if(!itr_ptr->m_Iter->Valid())
            ret_term=enif_make_tuple2(env, ATOM_ERROR, ATOM_INVALID_ITERATOR);

        else if (itr_ptr->m_Iter->m_KeysOnly)
            ret_term=enif_make_tuple2(env, ATOM_OK, slice_to_binary(env, itr_ptr->m_Iter->key()));
        else
            ret_term=enif_make_tuple3(env, ATOM_OK,
                                      slice_to_binary(env, itr_ptr->m_Iter->key()),
                                      slice_to_binary(env, itr_ptr->m_Iter->value()));

        // reset for next race
        itr_ptr->m_Iter->m_HandoffAtomic=0;

        // old MoveItem could still be active on its thread, cannot
        //  reuse ... but the current Iterator is good
        itr_ptr->ReleaseReuseMove();

        submit_new_request=true;
    }   // else


    // only build request if actually need to submit it
    if (submit_new_request)
    {
        erocksdb::MoveTask * move_item;

        move_item = new erocksdb::MoveTask(env, caller_ref,
                                           itr_ptr->m_Iter.get(), action);

        // prevent deletes during worker loop
        move_item->RefInc();
        itr_ptr->reuse_move=move_item;

        move_item->action=action;

        if (erocksdb::MoveTask::SEEK == action)
        {
            ErlNifBinary key;

            if(!enif_inspect_binary(env, action_or_target, &key))
            {
                itr_ptr->ReleaseReuseMove();
		itr_ptr->reuse_move=NULL;
                return enif_make_tuple2(env, ATOM_EINVAL, caller_ref);
            }   // if

            move_item->seek_target.assign((const char *)key.data, key.size);
        }   // else

        erocksdb_priv_data& priv = *static_cast<erocksdb_priv_data *>(enif_priv_data(env));

        if(false == priv.thread_pool.submit(move_item))
        {
            itr_ptr->ReleaseReuseMove();
	    itr_ptr->reuse_move=NULL;
            return enif_make_tuple2(env, ATOM_ERROR, caller_ref);
        }   // if
    }   // if

    return ret_term;

}   // async_iter_move


ERL_NIF_TERM
async_checkpoint(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM& caller_ref = argv[0];
    const ERL_NIF_TERM& handle_ref = argv[1];

    char path[4096];


    ReferencePtr<DbObject> db_ptr;

    db_ptr.assign(DbObject::RetrieveDbObject(env, handle_ref));

    if(NULL==db_ptr.get())
    {
        return enif_make_badarg(env);
    }

    // is this even possible?
    if(NULL == db_ptr->m_Db)
        return send_reply(env, caller_ref, error_einval(env));

    if(!enif_get_string(env, argv[2], path, sizeof(path), ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    erocksdb_priv_data& priv = *static_cast<erocksdb_priv_data *>(enif_priv_data(env));

    erocksdb::WorkTask* work_item = new erocksdb::CheckpointTask(env, caller_ref, db_ptr.get(), path);

    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                          enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }

    return erocksdb::ATOM_OK;

}   // async_checkpoint


} // namespace erocksdb


/***
 * HEY YOU, please convert this to an async operation
 */

ERL_NIF_TERM
erocksdb_close(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    erocksdb::DbObject * db_ptr;
    ERL_NIF_TERM ret_term;

    ret_term=erocksdb::ATOM_OK;

    db_ptr=erocksdb::DbObject::RetrieveDbObject(env, argv[0]);

    if (NULL!=db_ptr)
    {
        // set closing flag
        erocksdb::ErlRefObject::InitiateCloseRequest(db_ptr);

        db_ptr=NULL;

        ret_term=erocksdb::ATOM_OK;
    }   // if
    else
    {
        ret_term=enif_make_badarg(env);
    }   // else

    return(ret_term);

}  // erocksdb_close


ERL_NIF_TERM
erocksdb_iterator_close(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    erocksdb::ItrObject * itr_ptr;
    ERL_NIF_TERM ret_term;

    ret_term=erocksdb::ATOM_OK;

    itr_ptr=erocksdb::ItrObject::RetrieveItrObject(env, argv[0], true);

    if (NULL!=itr_ptr)
    {
        itr_ptr->ReleaseReuseMove();

        // set closing flag ... atomic likely unnecessary (but safer)
        erocksdb::ErlRefObject::InitiateCloseRequest(itr_ptr);

        itr_ptr=NULL;

        ret_term=erocksdb::ATOM_OK;
    }   // if
    else
    {
        ret_term=enif_make_badarg(env);
    }   // else

    return(ret_term);

}   // elveldb_iterator_close


ERL_NIF_TERM
erocksdb_status(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    ErlNifBinary name_bin;
    erocksdb::ReferencePtr<erocksdb::DbObject> db_ptr;

    db_ptr.assign(erocksdb::DbObject::RetrieveDbObject(env, argv[0]));

    if(NULL!=db_ptr.get()
       && enif_inspect_binary(env, argv[1], &name_bin))
    {
        if (db_ptr->m_Db == NULL)
        {
            return error_einval(env);
        }

        rocksdb::Slice name((const char*)name_bin.data, name_bin.size);
        std::string value;
        if (db_ptr->m_Db->GetProperty(name, &value))
        {
            ERL_NIF_TERM result;
            unsigned char* result_buf = enif_make_new_binary(env, value.size(), &result);
            memcpy(result_buf, value.c_str(), value.size());

            return enif_make_tuple2(env, erocksdb::ATOM_OK, result);
        }
        else
        {
            return erocksdb::ATOM_ERROR;
        }
    }
    else
    {
        return enif_make_badarg(env);
    }
}   // erocksdb_status


ERL_NIF_TERM
erocksdb_repair(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    char name[4096];
    if (enif_get_string(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1) &&
        enif_is_list(env, argv[1]))
    {
        // Parse out the options
        rocksdb::Options opts;
        fold(env, argv[1], parse_db_option, opts);

        rocksdb::Status status = rocksdb::RepairDB(name, opts);
        if (!status.ok())
        {
            return error_tuple(env, erocksdb::ATOM_ERROR_DB_REPAIR, status);
        }
        else
        {
            return erocksdb::ATOM_OK;
        }
    }
    else
    {
        return enif_make_badarg(env);
    }
}   // erocksdb_repair

/**
 * HEY YOU ... please make async
 */
ERL_NIF_TERM
erocksdb_destroy(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    char name[4096];
    if (enif_get_string(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1) &&
        enif_is_list(env, argv[1]))
    {
        // Parse out the options
        rocksdb::Options opts;
        fold(env, argv[1], parse_db_option, opts);

        rocksdb::Status status = rocksdb::DestroyDB(name, opts);
        if (!status.ok())
        {
            return error_tuple(env, erocksdb::ATOM_ERROR_DB_DESTROY, status);
        }
        else
        {
            return erocksdb::ATOM_OK;
        }
    }
    else
    {
        return enif_make_badarg(env);
    }

}   // erocksdb_destroy


ERL_NIF_TERM
erocksdb_is_empty(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    erocksdb::ReferencePtr<erocksdb::DbObject> db_ptr;

    db_ptr.assign(erocksdb::DbObject::RetrieveDbObject(env, argv[0]));

    if(NULL!=db_ptr.get())
    {
        if (db_ptr->m_Db == NULL)
        {
            return error_einval(env);
        }

        rocksdb::ReadOptions opts;
        rocksdb::Iterator* itr = db_ptr->m_Db->NewIterator(opts);
        itr->SeekToFirst();
        ERL_NIF_TERM result;
        if (itr->Valid())
        {
            result = erocksdb::ATOM_FALSE;
        }
        else
        {
            result = erocksdb::ATOM_TRUE;
        }
        delete itr;

        return result;
    }
    else
    {
        return enif_make_badarg(env);
    }
}   // erocksdb_is_empty


static void on_unload(ErlNifEnv *env, void *priv_data)
{
    erocksdb_priv_data *p = static_cast<erocksdb_priv_data *>(priv_data);
    delete p;
}


static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
try
{
    int ret_val;

    ret_val=0;
    *priv_data = NULL;

    // inform erlang of our two resource types
    erocksdb::DbObject::CreateDbObjectType(env);
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
    ATOM(erocksdb::ATOM_EXPANDED_COMPACTION_FACTOR, "expanded_compaction_factor");
    ATOM(erocksdb::ATOM_SOURCE_COMPACTION_FACTOR, "source_compaction_factor");
    ATOM(erocksdb::ATOM_MAX_GRANDPARENT_OVERLAP_FACTOR, "max_grandparent_overlap_factor");
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
        ErocksdbOptions load_options;

        fold(env, load_info, parse_init_option, load_options);

        /* Spin up the thread pool, set up all private data: */
        erocksdb_priv_data *priv = new erocksdb_priv_data(load_options);

        *priv_data = priv;

    }   // if

    else
    {
        // anything non-zero is "fail"
        ret_val=1;
    }   // else
    // Initialize common atoms

    return ret_val;
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
    ERL_NIF_INIT(erocksdb, nif_funcs, &on_load, NULL, NULL, &on_unload);
}
