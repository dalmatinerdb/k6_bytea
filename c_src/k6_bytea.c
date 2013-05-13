#include "erl_nif.h"
#include <string.h>

/* NOTE: it's unclear whether I need to use process-independent environments to
 * house the bytea_t resources themselves.  Testing required. */

typedef struct {
    ErlNifResourceType *resource_type;

    ErlNifMutex *count_mutex;
    int count;
} privdata_t;

static void increment_count(privdata_t *priv) {
    enif_mutex_lock(priv->count_mutex);
    ++priv->count;
    enif_mutex_unlock(priv->count_mutex);
}

static void decrement_count(privdata_t *priv) {
    enif_mutex_lock(priv->count_mutex);
    --priv->count;
    enif_mutex_unlock(priv->count_mutex);
}

static int get_count(privdata_t *priv) {
    enif_mutex_lock(priv->count_mutex);
    int c = priv->count;
    enif_mutex_unlock(priv->count_mutex);
    return c;
}

typedef struct {
    int size;
    unsigned char *array;
} bytea_t;

static int bytea_dispose(privdata_t *priv, bytea_t *ba) {
    if (!ba->array) {
        return 0;
    }

    decrement_count(priv);
    free(ba->array);
    ba->array = NULL;
    return 1;
}

static void bytea_dtor(ErlNifEnv *env, void *obj) {
    privdata_t *priv = enif_priv_data(env);
    bytea_dispose(priv, obj);
}

static ERL_NIF_TERM new_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int i;
    if (!enif_get_int(env, argv[0], &i)) {
        return enif_make_badarg(env);
    }

    privdata_t *priv = enif_priv_data(env);
    bytea_t *ba = enif_alloc_resource(priv->resource_type, sizeof(bytea_t));
    ba->size = i;
    ba->array = malloc(i);
    memset(ba->array, 0, i);
    ERL_NIF_TERM term = enif_make_resource(env, ba);
    enif_release_resource(ba);

    increment_count(priv);
    return term;
}

static ERL_NIF_TERM delete_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    privdata_t *priv = enif_priv_data(env);
    void *obj;
    if (!enif_get_resource(env, argv[0], priv->resource_type, &obj)) {
        return enif_make_badarg(env);
    }

    if (!bytea_dispose(priv, obj)) {
        return enif_make_badarg(env);
    }
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM size_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    privdata_t *priv = enif_priv_data(env);
    void *obj;
    if (!enif_get_resource(env, argv[0], priv->resource_type, &obj)) {
        return enif_make_badarg(env);
    }
    bytea_t *ba = obj;

    if (!ba->array) {
        return enif_make_badarg(env);
    }
    return enif_make_int(env, ba->size);
}

static ERL_NIF_TERM count_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    privdata_t *priv = enif_priv_data(env);
    return enif_make_int(env, get_count(priv));
}

static ERL_NIF_TERM get_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    privdata_t *priv = enif_priv_data(env);
    void *obj;
    if (!enif_get_resource(env, argv[0], priv->resource_type, &obj)) {
        return enif_make_badarg(env);
    }
    bytea_t *ba = obj;

    if (!ba->array) {
        return enif_make_badarg(env);
    }

    int from, len;
    if (!enif_get_int(env, argv[1], &from) || !enif_get_int(env, argv[2], &len)) {
        return enif_make_badarg(env);
    }

    if (from < 0 || from >= ba->size ||
            len < 0 || from + len > ba->size /* sic */) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM term;
    unsigned char *dest = enif_make_new_binary(env, len, &term);
    memcpy(dest, ba->array + from, len);
    return term;
}

static ERL_NIF_TERM set_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    privdata_t *priv = enif_priv_data(env);
    void *obj;
    if (!enif_get_resource(env, argv[0], priv->resource_type, &obj)) {
        return enif_make_badarg(env);
    }
    bytea_t *ba = obj;

    if (!ba->array) {
        return enif_make_badarg(env);
    }

    int from;
    ErlNifBinary bin;
    if (!enif_get_int(env, argv[1], &from) || !enif_inspect_binary(env, argv[2], &bin)) {
        return enif_make_badarg(env);
    }

    if (from < 0 || from >= ba->size ||
            from + bin.size > ba->size /* sic */) {
        return enif_make_badarg(env);
    }

    memcpy(ba->array + from, bin.data, bin.size);
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"new", 1, new_nif},
    {"delete", 1, delete_nif},
    {"size", 1, size_nif},
    {"count", 0, count_nif},
    {"get", 3, get_nif},
    {"set", 3, set_nif},
};

static int nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    privdata_t *priv = malloc(sizeof(*priv));
    priv->resource_type = enif_open_resource_type(
        env,
        NULL, "k6_bytea",
        bytea_dtor,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
        NULL);
    priv->count_mutex = enif_mutex_create("k6_bytea count_mutex");
    priv->count = 0;

    *priv_data = priv;
    return 0;
}

static int nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info) {
    *priv_data = *old_priv_data;
    return 0;
}

static void nif_unload(ErlNifEnv *env, void *priv_data) {
    privdata_t *priv = priv_data;
    enif_mutex_destroy(priv->count_mutex);
}

ERL_NIF_INIT(k6_bytea, nif_funcs, nif_load, NULL, nif_upgrade, nif_unload)

// vim: set ts=4 sw=4 et:
