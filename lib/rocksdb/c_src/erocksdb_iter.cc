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
#include "rocksdb/slice_transform.h"

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

#ifndef INCL_EROCKSB_DB_H
    #include "erocksdb_db.h"
#endif

namespace erocksdb {
  ERL_NIF_TERM
AsyncIterator(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM& caller_ref  = argv[0];
    const ERL_NIF_TERM& dbh_ref     = argv[1];
    const ERL_NIF_TERM& options_ref = argv[2];

    const bool keys_only = ((argc == 4) && (argv[3] == ATOM_KEYS_ONLY));

    rocksdb::ReadOptions opts;

    ReferencePtr<DbObject> db_ptr;
    db_ptr.assign(DbObject::RetrieveDbObject(env, dbh_ref));

    if(NULL==db_ptr.get() || 0!=db_ptr->m_CloseRequested
       || !enif_is_list(env, options_ref))
     {
        return enif_make_badarg(env);
     }

    // likely useless
    if(NULL == db_ptr->m_Db)
        return send_reply(env, caller_ref, error_einval(env));

    ERL_NIF_TERM fold_result;

    fold_result = fold(env, options_ref, parse_read_option, opts);
    if(fold_result!=erocksdb::ATOM_OK)
        return enif_make_badarg(env);

    erocksdb::WorkTask *work_item = new erocksdb::IterTask(env, caller_ref,
                                                           db_ptr.get(), keys_only, opts);

    // Now-boilerplate setup (we'll consolidate this pattern soon, I hope):
    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));
    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref, enif_make_tuple2(env, ATOM_ERROR, caller_ref));
    }   // if

    return ATOM_OK;

}   // erocksdb::AsyncIterator

ERL_NIF_TERM
AsyncIterators(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM caller_ref  = argv[0];

    const bool keys_only = ((argc == 5) && (argv[4] == ATOM_KEYS_ONLY));

    ReferencePtr<DbObject> db_ptr;
    if(!enif_get_db(env, argv[1], &db_ptr))
        return enif_make_badarg(env);

    if(NULL==db_ptr.get() || 0!=db_ptr->m_CloseRequested
       ||!enif_is_list(env, argv[2]) || !enif_is_list(env, argv[3]))
    {
        return enif_make_badarg(env);
    }

    rocksdb::ReadOptions opts;
    fold(env, argv[3], parse_read_option, opts);


    std::vector<rocksdb::ColumnFamilyHandle*> column_families;
    ERL_NIF_TERM head, tail = argv[2];
    while(enif_get_list_cell(env, tail, &head, &tail))
    {
        ReferencePtr<ColumnFamilyObject> cf_ptr;
        cf_ptr.assign(ColumnFamilyObject::RetrieveColumnFamilyObject(env, head));
        ColumnFamilyObject* cf = cf_ptr.get();
        column_families.push_back(cf->m_ColumnFamily);
    }

    erocksdb::WorkTask *work_item = new erocksdb::IteratorsTask(
        env, caller_ref, db_ptr.get(), column_families, keys_only, opts);

    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));
    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env, caller_ref, enif_make_tuple2(env, ATOM_ERROR, caller_ref));
    }   // if

    return ATOM_OK;
}

ERL_NIF_TERM
AsyncIteratorMove(
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

    if(NULL==itr_ptr.get() || 0!=itr_ptr->m_CloseRequested)
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

        erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));

        if(false == priv.thread_pool.submit(move_item))
        {
            itr_ptr->ReleaseReuseMove();
	    itr_ptr->reuse_move=NULL;
            return enif_make_tuple2(env, ATOM_ERROR, caller_ref);
        }   // if
    }   // if

    return ret_term;

}   // erocksdb::AsyncIteratorMove


ERL_NIF_TERM
AsyncIteratorClose(
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM& caller_ref = argv[0];

    ItrObject * itr_ptr;
    itr_ptr=erocksdb::ItrObject::RetrieveItrObject(env, argv[1], true);

    if(NULL==itr_ptr)
        return enif_make_badarg(env);

    erocksdb::PrivData& priv = *static_cast<erocksdb::PrivData *>(enif_priv_data(env));
    erocksdb::WorkTask* work_item = new erocksdb::CloseIteratorTask(env, caller_ref, itr_ptr);
    if(false == priv.thread_pool.submit(work_item))
    {
        delete work_item;
        return send_reply(env,caller_ref, enif_make_tuple2(env, erocksdb::ATOM_ERROR, caller_ref));
    }
    return erocksdb::ATOM_OK;
}   // erocksdb:AsyncIteratorClose

}