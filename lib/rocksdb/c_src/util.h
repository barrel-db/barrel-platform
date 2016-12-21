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
#define INCL_UTIL_H

#include "erl_nif.h"

#ifndef ATOMS_H
    #include "atoms.h"
#endif

#ifndef INCL_REFOBJECTS_H
    #include "refobjects.h"
#endif


#include "rocksdb/db.h"
#include "rocksdb/slice_transform.h"

ERL_NIF_TERM error_einval(ErlNifEnv* env);
ERL_NIF_TERM error_tuple(ErlNifEnv* env, ERL_NIF_TERM error,
rocksdb::Status& status);
ERL_NIF_TERM slice_to_binary(ErlNifEnv* env, rocksdb::Slice s);
ERL_NIF_TERM send_reply(ErlNifEnv *env, ERL_NIF_TERM ref, ERL_NIF_TERM reply);

int enif_get_db(ErlNifEnv* env, ERL_NIF_TERM dbval, erocksdb::ReferencePtr<erocksdb::DbObject>* db_ptr);
int enif_get_cf(ErlNifEnv* env, ERL_NIF_TERM dbval, erocksdb::ReferencePtr<erocksdb::ColumnFamilyObject>* cf_ptr);


#endif
