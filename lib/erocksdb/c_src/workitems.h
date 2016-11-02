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

#ifndef INCL_WORKITEMS_H
#define INCL_WORKITEMS_H

#include <stdint.h>

#include "rocksdb/db.h"
#include "rocksdb/write_batch.h"
#include "rocksdb/utilities/checkpoint.h"


#ifndef INCL_MUTEX_H
    #include "mutex.h"
#endif

#ifndef __WORK_RESULT_HPP
    #include "work_result.hpp"
#endif

#ifndef ATOMS_H
    #include "atoms.h"
#endif

#ifndef INCL_REFOBJECTS_H
    #include "refobjects.h"
#endif


namespace erocksdb {

/* Type returned from a work task: */
typedef leofs::async_nif::work_result   work_result;



/**
 * Virtual base class for async NIF work items:
 */
class WorkTask : public RefObject
{
public:

protected:
    ReferencePtr<DbObject> m_DbPtr;             //!< access to database, and holds reference

    ErlNifEnv      *local_env_;
    ERL_NIF_TERM   caller_ref_term;
    ERL_NIF_TERM   caller_pid_term;
    bool           terms_set;

    bool resubmit_work;           //!< true if this work item is loaded for prefetch

    ErlNifPid local_pid;   // maintain for task lifetime (JFW)

 public:

    WorkTask(ErlNifEnv *caller_env, ERL_NIF_TERM& caller_ref);

    WorkTask(ErlNifEnv *caller_env, ERL_NIF_TERM& caller_ref, DbObject * DbPtr);

    virtual ~WorkTask();

    virtual void prepare_recycle();
    virtual void recycle();

    virtual ErlNifEnv *local_env()         { return local_env_; }

    // call local_env() since the virtual creates the data in MoveTask
    const ERL_NIF_TERM& caller_ref()       { local_env(); return caller_ref_term; }
    const ERL_NIF_TERM& pid()              { local_env(); return caller_pid_term; }
    bool resubmit() const {return(resubmit_work);}

    virtual work_result operator()()     = 0;

private:
 WorkTask();
 WorkTask(const WorkTask &);
 WorkTask & operator=(const WorkTask &);

};  // class WorkTask


/**
 * Background object for async open of a rocksdb instance
 */

class OpenTask : public WorkTask
{
protected:
    std::string         db_name;
    rocksdb::Options   *options;  // associated with db handle, we don't free it

public:
    OpenTask(ErlNifEnv* caller_env, ERL_NIF_TERM& _caller_ref,
             const std::string& db_name_,
             rocksdb::Options *Options_);


    virtual ~OpenTask() {};

    virtual work_result operator()();

private:
    OpenTask();
    OpenTask(const OpenTask &);
    OpenTask & operator=(const OpenTask &);

};  // class OpenTask


/**
 * Background object for async snapshot creation
 */

class GetSnapshotTask : public WorkTask
{
public:
    GetSnapshotTask(ErlNifEnv *_caller_env,
                    ERL_NIF_TERM _caller_ref,
                    DbObject *_db_handle)
                : WorkTask(_caller_env, _caller_ref, _db_handle)
    {

    };

    virtual ~GetSnapshotTask() {};

    virtual work_result operator()()
    {
        SnapshotObject* snapshot_ptr;

        const rocksdb::Snapshot* snapshot;
        snapshot = m_DbPtr->m_Db->GetSnapshot();

        snapshot_ptr=SnapshotObject::CreateSnapshotObject(m_DbPtr.get(), snapshot);

        // create a resource reference to send erlang
        ERL_NIF_TERM result = enif_make_resource(local_env(), snapshot_ptr);

        // clear the automatic reference from enif_alloc_resource in CreateDbObject
        enif_release_resource(snapshot_ptr);

        snapshot = NULL;

        return work_result(local_env(), ATOM_OK, result);
    }   // operator()

};  // class GetSnapshotTask

class ReleaseSnapshotTask : public WorkTask
{

protected:
    ReferencePtr<SnapshotObject> m_SnapshotPtr;

public:
    ReleaseSnapshotTask(ErlNifEnv *_caller_env, ERL_NIF_TERM _caller_ref,
                        SnapshotObject * Snapshot)
                  : WorkTask(_caller_env, _caller_ref), m_SnapshotPtr(Snapshot)
    {};

    virtual ~ReleaseSnapshotTask() {};

    virtual work_result operator()()
    {
        SnapshotObject* snapshot = m_SnapshotPtr.get();

        snapshot->m_DbPtr->m_Db->ReleaseSnapshot(snapshot->m_Snapshot);

        // set closing flag
        ErlRefObject::InitiateCloseRequest(snapshot);

        return work_result(ATOM_OK);

    }   // operator()

};  // class GetSnapshotTask


/**
 * Background object for async snapshot creation
 */

class CheckpointTask : public WorkTask
{
protected:
    std::string path;

public:
    CheckpointTask(ErlNifEnv *_caller_env,
                    ERL_NIF_TERM _caller_ref,
                    DbObject *_db_handle,
                    const std::string& path_)
                : WorkTask(_caller_env, _caller_ref, _db_handle),
                path(path_)
    {

    };

    virtual ~CheckpointTask() {};

    virtual work_result operator()()
    {
        rocksdb::Checkpoint* checkpoint;
        rocksdb::Status status;

        status = rocksdb::Checkpoint::Create(m_DbPtr->m_Db, &checkpoint);

        if (status.ok()) {
            status = checkpoint->CreateCheckpoint(path);
            if (status.ok())
            {
                return work_result(ATOM_OK);
            }
        }
        delete checkpoint;

        return work_result(local_env(), ATOM_ERROR, status);
    }   // operator()

};  // class CheckpointTask

/**
 * Background object for async write
 */

class WriteTask : public WorkTask
{
protected:
    rocksdb::WriteBatch*    batch;
    rocksdb::WriteOptions*  options;

public:

    WriteTask(ErlNifEnv* _owner_env, ERL_NIF_TERM _caller_ref,
                DbObject * _db_handle,
                rocksdb::WriteBatch* _batch,
                rocksdb::WriteOptions* _options)
        : WorkTask(_owner_env, _caller_ref, _db_handle),
       batch(_batch),
       options(_options)
    {}

    virtual ~WriteTask()
    {
        delete batch;
        delete options;
    }

    virtual work_result operator()()
    {
        rocksdb::Status status = m_DbPtr->m_Db->Write(*options, batch);

        return (status.ok() ? work_result(ATOM_OK) : work_result(local_env(), ATOM_ERROR_DB_WRITE, status));
    }

};  // class WriteTask


/**
 * Background object for async get,
 */

class GetTask : public WorkTask
{
protected:
    std::string                        m_Key;
    rocksdb::ReadOptions*              options;

public:
    GetTask(ErlNifEnv *_caller_env,
            ERL_NIF_TERM _caller_ref,
            DbObject *_db_handle,
            ERL_NIF_TERM _key_term,
            rocksdb::ReadOptions *_options)
        : WorkTask(_caller_env, _caller_ref, _db_handle),
        options(_options)
        {
            ErlNifBinary key;

            enif_inspect_binary(_caller_env, _key_term, &key);
            m_Key.assign((const char *)key.data, key.size);
        }

    virtual ~GetTask()
    {
        delete options;
    }

    virtual work_result operator()()
    {
        ERL_NIF_TERM value_bin;
        std::string value;
        rocksdb::Slice key_slice(m_Key);

        rocksdb::Status status = m_DbPtr->m_Db->Get(*options, key_slice, &value);

        if(!status.ok())
            return work_result(ATOM_NOT_FOUND);

        unsigned char* v = enif_make_new_binary(local_env(), value.size(), &value_bin);
        memcpy(v, value.c_str(), value.size());

        return work_result(local_env(), ATOM_OK, value_bin);
    }

};  // class GetTask



/**
 * Background object to open/start an iteration
 */

class IterTask : public WorkTask
{
protected:

    const bool keys_only;
    rocksdb::ReadOptions *options;

public:

    IterTask(ErlNifEnv *_caller_env,
             ERL_NIF_TERM _caller_ref,
             DbObject *_db_handle,
             const bool _keys_only,
             rocksdb::ReadOptions *_options)
        : WorkTask(_caller_env, _caller_ref, _db_handle),
        keys_only(_keys_only), options(_options)
    {}

    virtual ~IterTask()
    {
        // options should be NULL at this point
        delete options;
    }

    virtual work_result operator()()
    {
        ItrObject * itr_ptr;
        rocksdb::Iterator * iterator;

        // NOTE: transfering ownership of options to ItrObject
        itr_ptr=ItrObject::CreateItrObject(m_DbPtr.get(), keys_only, options);


        iterator = m_DbPtr->m_Db->NewIterator(*options);
        itr_ptr->m_Iter.assign(new RocksIteratorWrapper(m_DbPtr.get(), iterator, keys_only));

        // Copy caller_ref to reuse in future iterator_move calls
        itr_ptr->m_Iter->itr_ref_env = enif_alloc_env();
        itr_ptr->m_Iter->itr_ref = enif_make_copy(itr_ptr->m_Iter->itr_ref_env, caller_ref());

        ERL_NIF_TERM result = enif_make_resource(local_env(), itr_ptr);

        // release reference created during CreateItrObject()
        enif_release_resource(itr_ptr);
        options=NULL;  // ptr ownership given to ItrObject

        return work_result(local_env(), ATOM_OK, result);
    }   // operator()

};  // class IterTask


class MoveTask : public WorkTask
{
public:
    typedef enum { FIRST, LAST, NEXT, PREV, SEEK, PREFETCH } action_t;

protected:
    ReferencePtr<RocksIteratorWrapper> m_ItrWrap;             //!< access to database, and holds reference

public:
    action_t                                       action;
    std::string                                 seek_target;

public:

    // No seek target:
    MoveTask(ErlNifEnv *_caller_env, ERL_NIF_TERM _caller_ref,
             RocksIteratorWrapper * IterWrap, action_t& _action)
        : WorkTask(NULL, _caller_ref),
        m_ItrWrap(IterWrap), action(_action)
    {
        // special case construction
        local_env_=NULL;
        enif_self(_caller_env, &local_pid);
    }

    // With seek target:
    MoveTask(ErlNifEnv *_caller_env, ERL_NIF_TERM _caller_ref,
             RocksIteratorWrapper * IterWrap, action_t& _action,
             std::string& _seek_target)
        : WorkTask(NULL, _caller_ref),
        m_ItrWrap(IterWrap), action(_action),
        seek_target(_seek_target)
        {
            // special case construction
            local_env_=NULL;
            enif_self(_caller_env, &local_pid);
        }
    virtual ~MoveTask() {};

    virtual work_result operator()();

    virtual ErlNifEnv *local_env();

    virtual void prepare_recycle();
    virtual void recycle();

};  // class MoveTask

} // namespace erocksdb


#endif  // INCL_WORKITEMS_H
