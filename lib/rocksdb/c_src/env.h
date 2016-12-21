// -------------------------------------------------------------------
// Copyright (c) 2016 Benoit Chesneau. All Rights Reserved.
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

#ifndef INCL_ENV_H
#define INCL_ENV_H

#include <syslog.h>
#include "rocksdb/db.h"
#include "rocksdb/cache.h"

#ifndef INCL_THREADING_H
    #include "threading.h"
#endif

namespace erocksdb {
 /** struct for grabbing erocksdb environment options via fold
 *   ... then loading said options into erocksdb_priv_data
 */
struct DbOptions
{
    int m_DbThreads;

    DbOptions()
        : m_DbThreads(71)
        {};

    void Dump()
    {
        syslog(LOG_ERR, "         m_ErocksdbThreads: %d\n", m_DbThreads);
    }   // Dump
};  // struct erocksdb::DbOptions

static int kCapacity = 4194304; // default values, can be overridden

/** Module-level private data:
 *    singleton instance held by erlang and passed on API calls
 */
class PrivData
{
public:
    DbOptions m_Opts;
    erocksdb::erocksdb_thread_pool thread_pool;
    std::shared_ptr<rocksdb::Cache> block_cache;

    explicit PrivData(DbOptions & Options)
    : m_Opts(Options),
      thread_pool(Options.m_DbThreads)
        { block_cache = rocksdb::NewLRUCache(kCapacity); }

private:
    PrivData();                                      // no default constructor
    PrivData(const PrivData&);             // nocopy
    PrivData& operator=(const PrivData&);  // nocopyassign

};

}

#endif