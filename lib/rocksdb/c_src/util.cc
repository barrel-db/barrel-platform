
// -------------------------------------------------------------------
// Copyright (c) 2011-2013 Basho Technologies, Inc. All Rights Reserved.
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

#ifndef INCL_UTIL_H
    #include "util.h"
#endif

#ifndef ATOMS_H
    #include "atoms.h"
#endif


// Erlang helpers:
ERL_NIF_TERM error_einval(ErlNifEnv* env)
{
    return enif_make_tuple2(env, erocksdb::ATOM_ERROR, erocksdb::ATOM_EINVAL);
}

ERL_NIF_TERM error_tuple(ErlNifEnv* env, ERL_NIF_TERM error,
rocksdb::Status& status)
{
    ERL_NIF_TERM reason = enif_make_string(env, status.ToString().c_str(),
                                           ERL_NIF_LATIN1);
    return enif_make_tuple2(env, erocksdb::ATOM_ERROR,
                            enif_make_tuple2(env, error, reason));
}

ERL_NIF_TERM slice_to_binary(ErlNifEnv* env, rocksdb::Slice s)
{
    ERL_NIF_TERM result;
    unsigned char* value = enif_make_new_binary(env, s.size(), &result);
    memcpy(value, s.data(), s.size());
    return result;
}

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
    return erocksdb::ATOM_OK;
}

int
enif_get_db(ErlNifEnv* env, ERL_NIF_TERM dbval, erocksdb::ReferencePtr<erocksdb::DbObject>* db_ptr)
{
    db_ptr->assign(erocksdb::DbObject::RetrieveDbObject(env, dbval));

    if(NULL==db_ptr->get())
        return 0;

    if(NULL==db_ptr->get()->m_Db)
        return 0;

    return 1;
}

int
enif_get_cf(ErlNifEnv* env, ERL_NIF_TERM cfval, erocksdb::ReferencePtr<erocksdb::ColumnFamilyObject>* cf_ptr)
{
    cf_ptr->assign(erocksdb::ColumnFamilyObject::RetrieveColumnFamilyObject(env, cfval));

    if(NULL==cf_ptr->get())
        return 0;

    return 1;
}
