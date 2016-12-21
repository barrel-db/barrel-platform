// -------------------------------------------------------------------
// Copyright (c) 2016 Benoit Chesneau. All Rights Reserved.
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

#include <vector>

#include "erocksdb.h"

#include "rocksdb/db.h"
#include "rocksdb/env.h"
#include "rocksdb/cache.h"
#include "rocksdb/table.h"
#include "rocksdb/filter_policy.h"
#include "rocksdb/slice_transform.h"

#ifndef ATOMS_H
    #include "atoms.h"
#endif

#ifndef INCL_THREADING_H
    #include "threading.h"
#endif

#ifndef INCL_WORKITEMS_H
    #include "workitems.h"
#endif

#ifndef INCL_REFOBJECTS_H
    #include "refobjects.h"
#endif

#include "work_result.hpp"
#include "detail.hpp"

#ifndef INCL_UTIL_H
    #include "util.h"
#endif

#ifndef INCL_ENV_H
  #include "env.h"
#endif

#ifndef INCL_EROCKSB_DB_H
    #include "erocksdb_db.h"
#endif

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
                erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));
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
        else if (option[0] == erocksdb::ATOM_BLOCK_CACHE_SIZE_MB_FOR_POINT_LOOKUP)
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
        else if (option[0] == erocksdb::ATOM_MAX_COMPACTION_BYTES)
        {
            int max_compaction_bytes;
            if (enif_get_int(env, option[1], &max_compaction_bytes))
                opts.max_compaction_bytes = max_compaction_bytes;
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
                rocksdb::BlockBasedTableOptions bbtOpts;
                bbtOpts.block_cache = rocksdb::NewLRUCache(table_factory_block_cache_size);
                bbtOpts.filter_policy = std::shared_ptr<const rocksdb::FilterPolicy>(rocksdb::NewBloomFilterPolicy(10));

                opts.table_factory = std::shared_ptr<rocksdb::TableFactory>(rocksdb::NewBlockBasedTableFactory(bbtOpts));
            }
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
                opts.memtable_prefix_bloom_size_ratio = 0.25;
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

ERL_NIF_TERM parse_cf_option(ErlNifEnv* env, ERL_NIF_TERM item, rocksdb::ColumnFamilyOptions& opts)
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
        
        else if (option[0] == erocksdb::ATOM_MAX_COMPACTION_BYTES)
        {
            int max_compaction_bytes;
            if (enif_get_int(env, option[1], &max_compaction_bytes))
                opts.max_compaction_bytes = max_compaction_bytes;
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
                erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));
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
                opts.compression = rocksdb::CompressionType::kNoCompression;
                opts.memtable_prefix_bloom_size_ratio = 0.25;
                opts.compaction_style = rocksdb::CompactionStyle::kCompactionStyleUniversal;
                opts.compaction_options_universal.size_ratio = 10;
                opts.compaction_options_universal.min_merge_width = 2;
                opts.compaction_options_universal.max_size_amplification_percent = 50;
                opts.level0_file_num_compaction_trigger = 0;
                opts.level0_slowdown_writes_trigger = 8;
                opts.level0_stop_writes_trigger = 16;
                opts.bloom_locality = 1;
                opts.write_buffer_size = 32 << 20;
                opts.max_write_buffer_number = 2;
                opts.min_write_buffer_number_to_merge = 1;
            }
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
        erocksdb::ReferencePtr<erocksdb::ColumnFamilyObject> cf_ptr;

        if (action[0] == erocksdb::ATOM_PUT)
        {
            if(arity == 3  &&
               enif_inspect_binary(env, action[1], &key) &&
               enif_inspect_binary(env, action[2], &value))
            {
                rocksdb::Slice key_slice((const char*)key.data, key.size);
                rocksdb::Slice value_slice((const char*)value.data, value.size);
                batch.Put(key_slice, value_slice);
                return erocksdb::ATOM_OK;
            }
            else
            {
                const ERL_NIF_TERM& cf_ref = action[1];
                cf_ptr.assign(erocksdb::ColumnFamilyObject::RetrieveColumnFamilyObject(env, cf_ref));
                erocksdb::ColumnFamilyObject* cf = cf_ptr.get();

                if(NULL!=cf_ptr.get() &&
                   enif_inspect_binary(env, action[2], &key) &&
                   enif_inspect_binary(env, action[3], &value))
                {
                    rocksdb::Slice key_slice((const char*)key.data, key.size);
                    rocksdb::Slice value_slice((const char*)value.data, value.size);
                    batch.Put(cf->m_ColumnFamily, key_slice, value_slice);
                    return erocksdb::ATOM_OK;
                }
            }
        }

        if (action[0] == erocksdb::ATOM_DELETE)
        {
            if(arity == 2 && enif_inspect_binary(env, action[1], &key))
            {
                rocksdb::Slice key_slice((const char*)key.data, key.size);
                batch.Delete(key_slice);
                return erocksdb::ATOM_OK;
            }
            else
            {
                const ERL_NIF_TERM& cf_ref = action[1];
                cf_ptr.assign(erocksdb::ColumnFamilyObject::RetrieveColumnFamilyObject(env, cf_ref));
                erocksdb::ColumnFamilyObject* cf = cf_ptr.get();

                if(NULL!=cf_ptr.get() && enif_inspect_binary(env, action[2], &key))
                {
                    rocksdb::Slice key_slice((const char*)key.data, key.size);
                    batch.Delete(cf->m_ColumnFamily, key_slice);
                    return erocksdb::ATOM_OK;
                }
            }
        }
    }

    // Failed to match clear/put/delete; return the failing item
    return item;
}

ERL_NIF_TERM
parse_cf_descriptor(ErlNifEnv* env, ERL_NIF_TERM item,
                    std::vector<rocksdb::ColumnFamilyDescriptor>& column_families)
{
    char cf_name[4096];
    rocksdb::ColumnFamilyOptions *opts = new rocksdb::ColumnFamilyOptions;
    int arity;
    const ERL_NIF_TERM *cf;

    if (enif_get_tuple(env, item, &arity, &cf) && 2 == arity) {
        if(!enif_get_string(env, cf[0], cf_name, sizeof(cf_name), ERL_NIF_LATIN1) ||
           !enif_is_list(env, cf[1]))
        {
            return enif_make_badarg(env);
        }
        ERL_NIF_TERM result = fold(env, cf[1], parse_cf_option, *opts);
        if (result != erocksdb::ATOM_OK)
        {
            return result;
        }

        column_families.push_back(rocksdb::ColumnFamilyDescriptor(cf_name, *opts));
    }

    return erocksdb::ATOM_OK;
}


namespace erocksdb {

ERL_NIF_TERM
AsyncOpen(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    char db_name[4096];

    if(!enif_get_string(env, argv[1], db_name, sizeof(db_name), ERL_NIF_LATIN1) || !enif_is_list(env, argv[2]))
    {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM caller_ref = argv[0];

    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));

    rocksdb::Options *opts = new rocksdb::Options;
    fold(env, argv[2], parse_db_option, *opts);

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
AsyncOpenWithCf(
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

    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));

    // read db options
    rocksdb::Options *opts = new rocksdb::Options;
    fold(env, argv[2], parse_db_option, *opts);

    std::vector<rocksdb::ColumnFamilyDescriptor> column_families;
    ERL_NIF_TERM head, tail = argv[3];
    while(enif_get_list_cell(env, tail, &head, &tail))
    {
        ERL_NIF_TERM result = parse_cf_descriptor(env, head, column_families);
        if (result != ATOM_OK)
        {
            return result;
        }
    }

    unsigned int num_cols;
    enif_get_list_length(env, argv[3], &num_cols);

    // send task
    erocksdb::WorkTask *work_item = new erocksdb::OpenCfTask(env, caller_ref, db_name, opts, column_families, num_cols);
    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                          enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }
    return erocksdb::ATOM_OK;
}   // async_open


ERL_NIF_TERM
AsyncClose(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    erocksdb::DbObject * db_ptr;
    const ERL_NIF_TERM& caller_ref = argv[0];
    const ERL_NIF_TERM& handle_ref = argv[1];

    db_ptr = erocksdb::DbObject::RetrieveDbObject(env, handle_ref);
    if (db_ptr==NULL || 0!=db_ptr->m_CloseRequested)
        return enif_make_badarg(env);

    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));
    erocksdb::WorkTask* work_item = new erocksdb::CloseTask(env, caller_ref, db_ptr);
    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                          enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }
    return erocksdb::ATOM_OK;
}  // erocksdb::AsyncClose

ERL_NIF_TERM
GetProperty(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    ErlNifBinary name_bin;
    ERL_NIF_TERM name_ref;

    ReferencePtr<DbObject> db_ptr;
    ReferencePtr<ColumnFamilyObject> cf_ptr;

    db_ptr.assign(DbObject::RetrieveDbObject(env, argv[0]));
    if(NULL==db_ptr.get())
        return enif_make_badarg(env);

    if(argc  == 3)
    {
      name_ref = argv[2];
      // we use a column family assign the value
      cf_ptr.assign(ColumnFamilyObject::RetrieveColumnFamilyObject(env, argv[1]));
    }
    else
    {
      name_ref = argv[1];
    }

    if (!enif_inspect_binary(env, name_ref, &name_bin))
        return enif_make_badarg(env);


    rocksdb::Slice name((const char*)name_bin.data, name_bin.size);
    std::string value;
    if (db_ptr->m_Db->GetProperty(name, &value))
    {
        ERL_NIF_TERM result;
        unsigned char* result_buf = enif_make_new_binary(env, value.size(), &result);
        memcpy(result_buf, value.c_str(), value.size());
        return enif_make_tuple2(env, erocksdb::ATOM_OK, result);
    }
    return erocksdb::ATOM_ERROR;
}   // erocksdb_status


ERL_NIF_TERM
AsyncWrite(
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

    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));

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
} // erocksdb::AsyncWrite

ERL_NIF_TERM
AsyncGet(
  ErlNifEnv* env,
  int argc,
  const ERL_NIF_TERM argv[])
{
  const ERL_NIF_TERM& caller_ref = argv[0];
  const ERL_NIF_TERM& dbh_ref    = argv[1];
  ERL_NIF_TERM cf_ref;
  ERL_NIF_TERM key_ref;
  ERL_NIF_TERM opts_ref;

  ReferencePtr<DbObject> db_ptr;
  ReferencePtr<ColumnFamilyObject> cf_ptr;

  db_ptr.assign(DbObject::RetrieveDbObject(env, dbh_ref));
  if(NULL==db_ptr.get())
    return enif_make_badarg(env);

  if(argc  == 4)
  {
      key_ref = argv[2];
      opts_ref = argv[3];
  }
  else
  {
      cf_ref = argv[2];
      key_ref = argv[3];
      opts_ref = argv[4];
      // we use a column family assign the value
      cf_ptr.assign(ColumnFamilyObject::RetrieveColumnFamilyObject(env, cf_ref));
  }

  if(NULL==db_ptr.get()
       || !enif_is_list(env, opts_ref)
       || !enif_is_binary(env, key_ref))
  {
      return enif_make_badarg(env);
  }

  if(NULL == db_ptr->m_Db)
      return send_reply(env, caller_ref, error_einval(env));

  rocksdb::ReadOptions opts;
  ERL_NIF_TERM fold_result = fold(env, opts_ref, parse_read_option, opts);
  if(fold_result!=erocksdb::ATOM_OK)
      return enif_make_badarg(env);

  erocksdb::WorkTask *work_item;
  if(argc==4)
  {
    work_item = new erocksdb::GetTask(env, caller_ref, db_ptr.get(), key_ref, opts);
  }
  else
  {
    work_item = new erocksdb::GetTask(env, caller_ref, db_ptr.get(), cf_ptr.get(), key_ref, opts);
  }
  
  erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));

  if(false == priv.thread_pool.submit(work_item))
  {
      delete work_item;
      return send_reply(env, caller_ref,
                        enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
  }   // if

  return erocksdb::ATOM_OK;

}   // erocksdb::AsyncGet


ERL_NIF_TERM
AsyncCheckpoint(
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

    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));
    erocksdb::WorkTask* work_item = new erocksdb::CheckpointTask(env, caller_ref, db_ptr.get(), path);
    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                          enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }
    return erocksdb::ATOM_OK;

}   // async_checkpoint

ERL_NIF_TERM
AsyncDestroy(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    char db_name[4096];
    const ERL_NIF_TERM& caller_ref = argv[0];
    if (!enif_get_string(env, argv[1], db_name, sizeof(db_name), ERL_NIF_LATIN1) &&
        ! enif_is_list(env, argv[2]))
    {
        return enif_make_badarg(env);
    }

    // Parse out the options
    rocksdb::Options opts;
    fold(env, argv[2], parse_db_option, opts);
    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));
    erocksdb::WorkTask* work_item = new erocksdb::DestroyTask(env, caller_ref, db_name, opts);
    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                            enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }
    return erocksdb::ATOM_OK;
}   // erocksdb_destroy

ERL_NIF_TERM
AsyncRepair(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    char db_name[4096];
    const ERL_NIF_TERM& caller_ref = argv[0];
    if (!enif_get_string(env, argv[1], db_name, sizeof(db_name), ERL_NIF_LATIN1) &&
        ! enif_is_list(env, argv[2]))
    {
        return enif_make_badarg(env);
    }

    // Parse out the options
    rocksdb::Options opts;
    fold(env, argv[2], parse_db_option, opts);
    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));
    erocksdb::WorkTask* work_item = new erocksdb::RepairTask(env, caller_ref, db_name, opts);
    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                            enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }
    return erocksdb::ATOM_OK;
}   // erocksdb_repair

ERL_NIF_TERM
AsyncIsEmpty(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM& caller_ref = argv[0];

    ReferencePtr<DbObject> db_ptr;
    if(!enif_get_db(env, argv[1], &db_ptr))
        return enif_make_badarg(env);

    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));
    erocksdb::WorkTask* work_item = new erocksdb::IsEmptyTask(env, caller_ref, db_ptr.get());
    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                          enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }
    return erocksdb::ATOM_OK;
}   // erocksdb_is_empty



}

/**
 * HEY YOU ... please make async
 */

