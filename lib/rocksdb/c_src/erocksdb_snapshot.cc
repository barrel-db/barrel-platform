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
#ifndef INCL_REFOBJECTS_H
    #include "refobjects.h"
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

#ifndef ATOMS_H
    #include "atoms.h"
#endif

#include "work_result.hpp"
#include "detail.hpp"

#ifndef INCL_UTIL_H
    #include "util.h"
#endif

#ifndef INCL_ENV_H
  #include "env.h"
#endif


namespace erocksdb {

ERL_NIF_TERM
AsyncSnapshot(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{

    const ERL_NIF_TERM& caller_ref = argv[0];
    const ERL_NIF_TERM& handle_ref = argv[1];

    ReferencePtr<DbObject> db_ptr;

    db_ptr.assign(DbObject::RetrieveDbObject(env, handle_ref));

    if(NULL==db_ptr.get() || 0!=db_ptr->m_CloseRequested)
    {
        return enif_make_badarg(env);
    }

    // is this even possible?
    if(NULL == db_ptr->m_Db)
        return send_reply(env, caller_ref, error_einval(env));

    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));

    erocksdb::WorkTask* work_item = new erocksdb::GetSnapshotTask(env, caller_ref, db_ptr.get());

    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }

    return erocksdb::ATOM_OK;
}   // erocksdb::AsyncSnapShot


ERL_NIF_TERM
AsyncReleaseSnapshot(
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

    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));
    erocksdb::WorkTask* work_item = new erocksdb::ReleaseSnapshotTask(env, caller_ref, snapshot_ptr.get());

    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref,
                          enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }

    return erocksdb::ATOM_OK;
}   // erocksdb::AsyncReleaseSnapShot

} 