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

namespace erocksdb {

ERL_NIF_TERM
AsyncListColumnFamilies(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char db_name[4096];

    if(!enif_get_string(env, argv[1], db_name, sizeof(db_name), ERL_NIF_LATIN1) ||
       !enif_is_list(env, argv[2]))
    {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM caller_ref = argv[0];
    // parse main db options
    rocksdb::Options *opts = new rocksdb::Options;
    fold(env, argv[2], parse_db_option, *opts);

    // send task
    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));
    erocksdb::WorkTask* work_item = new erocksdb::ListColumnFamilyTask(env, caller_ref, db_name, opts);
    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                          enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }
    return erocksdb::ATOM_OK;
} // erocksdb::AsyncListColumnFamilies

ERL_NIF_TERM
AsyncCreateColumnFamily(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ReferencePtr<DbObject> db_ptr;

    if(!enif_get_db(env, argv[1], &db_ptr))
        return enif_make_badarg(env);

    if(NULL==db_ptr.get() || 0!=db_ptr->m_CloseRequested)
      return enif_make_badarg(env);

    char cf_name[4096];
    rocksdb::ColumnFamilyOptions opts;

    if(!enif_get_string(env, argv[2], cf_name, sizeof(cf_name), ERL_NIF_LATIN1) ||
       !enif_is_list(env, argv[3]))
    {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM caller_ref = argv[0];
    ERL_NIF_TERM result = fold(env, argv[2], parse_cf_option, opts);
    if (result != erocksdb::ATOM_OK)
    {
        return result;
    }

    // send task
    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));
    erocksdb::WorkTask* work_item = new erocksdb::CreateColumnFamilyTask(env, caller_ref, cf_name, opts, db_ptr);
    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                          enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }
    return erocksdb::ATOM_OK;

}   // erocksdb::AsyncCreateColumnFamily

ERL_NIF_TERM
AsyncDropColumnFamily(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ReferencePtr<ColumnFamilyObject> cf_ptr;
    if(!enif_get_cf(env, argv[1], &cf_ptr))
        return enif_make_badarg(env);

    ERL_NIF_TERM caller_ref = argv[0];

    // send task
    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));
    erocksdb::WorkTask* work_item = new erocksdb::DropColumnFamilyTask(env, caller_ref, cf_ptr);
    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                          enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }
    return erocksdb::ATOM_OK;
}   // erocksdb::DropColumnFamilyTask


}