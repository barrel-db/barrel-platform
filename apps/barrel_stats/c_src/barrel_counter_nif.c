/*
 * Copyright (c) 2014  Machine Zone, Inc.
 *
 * Original author: Lev Walkin <lwalkin@machinezone.com>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <stdint.h>
#include <sys/time.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>

#include "erl_nif.h"
#include "barrel_counter_atomic.h"

static ERL_NIF_TERM ATOM_OK;

typedef struct {
    ErlNifResourceType *mzc_ctx_type;
} priv_data_t;

typedef struct {
    atomic_wide_t value;
} mzc_ctx_t;

static inline ErlNifResourceType* get_mzc_ctx_type(ErlNifEnv* env) {
    priv_data_t *pd = (priv_data_t*)enif_priv_data(env);
    return pd->mzc_ctx_type;
}

ERL_NIF_TERM create_counter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType* ctx_type = get_mzc_ctx_type(env);

    if (argc != 0) {
        return enif_make_badarg(env);
    }

    mzc_ctx_t* ctx = (mzc_ctx_t*)enif_alloc_resource(ctx_type, sizeof(mzc_ctx_t));
    ctx->value._atomic_val = 0;
    ERL_NIF_TERM result = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
    return enif_make_tuple2(env, ATOM_OK, result);
}

ERL_NIF_TERM update_counter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    mzc_ctx_t* ctx = NULL;
    int64_t value;


    ErlNifResourceType* ctx_type = get_mzc_ctx_type(env);
    if (argc != 2 ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx) ||
        !enif_get_int64(env, argv[1], (ErlNifSInt64*)&value))
    {
        return enif_make_badarg(env);
    }

    atomic_add(&ctx->value, value);
    return ATOM_OK;
}

ERL_NIF_TERM reset_counter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    mzc_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = get_mzc_ctx_type(env);
    if (argc != 1 ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }

    atomic_add(&ctx->value, -atomic_wide_get(&ctx->value));

    return ATOM_OK;
}

ERL_NIF_TERM get_counter_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    mzc_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = get_mzc_ctx_type(env);
    if (argc != 1 ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }

    return enif_make_int64(env, atomic_wide_get(&ctx->value));
}

static void _mzc_ctx_dtor(ErlNifEnv* env, void* obj) {
}

static void init(ErlNifEnv* env) {
    ATOM_OK = enif_make_atom(env, "ok");
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    priv_data_t *pd;

    init(env);

    pd = enif_alloc(sizeof(*pd));

    if(!pd) {
        return 1;
    }

    pd->mzc_ctx_type = enif_open_resource_type(env, NULL, "mzc_ctx_t", _mzc_ctx_dtor, ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);

    if(pd->mzc_ctx_type == NULL) {
        return 1;
    }

    *priv_data = (void*)pd;

    return 0;
}

static int on_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

static void on_unload(ErlNifEnv* env, void *priv_data) {
    if (priv_data != NULL)
    {
        priv_data_t* pd = (priv_data_t*)priv_data;
        enif_free(pd);
    }
}

static ErlNifFunc nif_funcs[] =
{
    {"create_counter", 0, create_counter},
    {"update_counter", 2, update_counter},
    {"get_counter_value", 1, get_counter_value},
    {"reset_counter", 1, reset_counter}
};

ERL_NIF_INIT(barrel_stats_counter, nif_funcs, &on_load, NULL, &on_upgrade, &on_unload)
