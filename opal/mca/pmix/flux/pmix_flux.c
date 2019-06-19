/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"
#include "opal/types.h"

#include "opal_stdint.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"

#include <string.h>
#if defined (HAVE_FLUX_PMI_LIBRARY)
#include <pmi.h>
#else
#include <dlfcn.h>
#endif

#include "opal/mca/pmix/base/base.h"
#include "opal/mca/pmix/base/pmix_base_fns.h"
#include "opal/mca/pmix/base/pmix_base_hash.h"
#include "pmix_flux.h"

static int flux_init(opal_list_t *ilist);
static int flux_fini(void);
static int flux_initialized(void);
static int flux_abort(int flag, const char msg[],
                    opal_list_t *procs);
static int flux_commit(void);
static int flux_fence(opal_list_t *procs, int collect_data);
static int flux_put(opal_pmix_scope_t scope,
                  opal_value_t *kv);
static int flux_get(const opal_process_name_t *id,
                  const char *key, opal_list_t *info,
                  opal_value_t **kv);
static int flux_publish(opal_list_t *info);
static int flux_lookup(opal_list_t *data, opal_list_t *info);
static int flux_unpublish(char **keys, opal_list_t *info);
static int flux_spawn(opal_list_t *jobinfo, opal_list_t *apps, opal_jobid_t *jobid);
static int flux_job_connect(opal_list_t *procs);
static int flux_job_disconnect(opal_list_t *procs);
static int flux_store_local(const opal_process_name_t *proc,
                          opal_value_t *val);
static const char *flux_get_nspace(opal_jobid_t jobid);
static void flux_register_jobid(opal_jobid_t jobid, const char *nspace);

const opal_pmix_base_module_t opal_pmix_flux_module = {
    .init = flux_init,
    .finalize = flux_fini,
    .initialized = flux_initialized,
    .abort = flux_abort,
    .commit = flux_commit,
    .fence = flux_fence,
    .put = flux_put,
    .get = flux_get,
    .publish = flux_publish,
    .lookup = flux_lookup,
    .unpublish = flux_unpublish,
    .spawn = flux_spawn,
    .connect = flux_job_connect,
    .disconnect = flux_job_disconnect,
    .register_evhandler = opal_pmix_base_register_handler,
    .deregister_evhandler = opal_pmix_base_deregister_handler,
    .store_local = flux_store_local,
    .get_nspace = flux_get_nspace,
    .register_jobid = flux_register_jobid
};

// usage accounting
static int pmix_init_count = 0;

// PMI constant values:
static int pmix_kvslen_max = 0;
static int pmix_keylen_max = 0;
static int pmix_vallen_max = 0;
static int pmix_vallen_threshold = INT_MAX;

// Job environment description
static char *pmix_kvs_name = NULL;
static bool flux_committed = false;
static char* pmix_packed_data = NULL;
static int pmix_packed_data_offset = 0;
static char* pmix_packed_encoded_data = NULL;
static int pmix_packed_encoded_data_offset = 0;
static int pmix_pack_key = 0;
static opal_process_name_t flux_pname;
static int *lranks = NULL, nlranks;

static char* pmix_error(int pmix_err);
#define OPAL_PMI_ERROR(pmi_err, pmi_func)                       \
    do {                                                        \
        opal_output(0, "%s [%s:%d:%s]: %s\n",                   \
            pmi_func, __FILE__, __LINE__, __func__,     \
            pmix_error(pmi_err));                        \
    } while(0);


#if !defined (HAVE_FLUX_PMI_LIBRARY)
//
// Wrapper functions for dlopened() PMI library.
//
#define PMI_SUCCESS                  0
#define PMI_FAIL                    -1
#define PMI_ERR_INIT                 1
#define PMI_ERR_NOMEM                2
#define PMI_ERR_INVALID_ARG          3
#define PMI_ERR_INVALID_KEY          4
#define PMI_ERR_INVALID_KEY_LENGTH   5
#define PMI_ERR_INVALID_VAL          6
#define PMI_ERR_INVALID_VAL_LENGTH   7
#define PMI_ERR_INVALID_LENGTH       8
#define PMI_ERR_INVALID_NUM_ARGS     9
#define PMI_ERR_INVALID_ARGS        10
#define PMI_ERR_INVALID_NUM_PARSED  11
#define PMI_ERR_INVALID_KEYVALP     12
#define PMI_ERR_INVALID_SIZE        13

static void *dso = NULL;

static int PMI_Init (int *spawned)
{
    int (*f)(int *);
    if (!dso) {
        const char *path;
        if ((path = getenv ("FLUX_PMI_LIBRARY_PATH")))
            dso = dlopen (path, RTLD_NOW | RTLD_GLOBAL);
        if (!dso)
            return PMI_FAIL;
    }
    *(void **)(&f) = dlsym (dso, "PMI_Init");
    return f ? f (spawned) : PMI_FAIL;
}

static int PMI_Initialized (int *initialized)
{
    int (*f)(int *);
    if (!dso) {
        if (initialized)
            *initialized = 0;
        return PMI_SUCCESS;
    }
    *(void **)(&f) = dlsym (dso, "PMI_Initialized");
    return f ? f (initialized) : PMI_FAIL;
}

static int PMI_Finalize (void)
{
    int (*f)(void);
    int rc;
    if (!dso)
        return PMI_SUCCESS;
    *(void **)(&f) = dlsym (dso, "PMI_Finalize");
    rc = f ? f () : PMI_FAIL;
    dlclose (dso);
    return rc;
}

static int PMI_Get_size (int *size)
{
    int (*f)(int *);
    *(void **)(&f) = dso ? dlsym (dso, "PMI_Get_size") : NULL;
    return f ? f (size) : PMI_FAIL;
}

static int PMI_Get_rank (int *rank)
{
    int (*f)(int *);
    *(void **)(&f) = dso ? dlsym (dso, "PMI_Get_rank") : NULL;
    return f ? f (rank) : PMI_FAIL;
}

static int PMI_Get_universe_size (int *size)
{
    int (*f)(int *);
    *(void **)(&f) = dso ? dlsym (dso, "PMI_Get_universe_size") : NULL;
    return f ? f (size) : PMI_FAIL;
}

static int PMI_Get_appnum (int *appnum)
{
    int (*f)(int *);
    *(void **)(&f) = dso ? dlsym (dso, "PMI_Get_appnum") : NULL;
    return f ? f (appnum) : PMI_FAIL;
}

static int PMI_Barrier (void)
{
    int (*f)(void);
    *(void **)(&f) = dso ? dlsym (dso, "PMI_Barrier") : NULL;
    return f ? f () : PMI_FAIL;
}

static int PMI_Abort (int exit_code, const char *error_msg)
{
    int (*f)(int, const char *);
    *(void **)(&f) = dso ? dlsym (dso, "PMI_Abort") : NULL;
    return f ? f (exit_code, error_msg) : PMI_FAIL;
}

static int PMI_KVS_Get_my_name (char *kvsname, int length)
{
    int (*f)(char *, int);
    *(void **)(&f) = dso ? dlsym (dso, "PMI_KVS_Get_my_name") : NULL;
    return f ? f (kvsname, length) : PMI_FAIL;
}

static int PMI_KVS_Get_name_length_max (int *length)
{
    int (*f)(int *);
    *(void **)(&f) = dso ? dlsym (dso, "PMI_KVS_Get_name_length_max") : NULL;
    return f ? f (length) : PMI_FAIL;
}

static int PMI_KVS_Get_key_length_max (int *length)
{
    int (*f)(int *);
    *(void **)(&f) = dso ? dlsym (dso, "PMI_KVS_Get_key_length_max") : NULL;
    return f ? f (length) : PMI_FAIL;
}

static int PMI_KVS_Get_value_length_max (int *length)
{
    int (*f)(int *);
    *(void **)(&f) = dso ? dlsym (dso, "PMI_KVS_Get_value_length_max") : NULL;
    return f ? f (length) : PMI_FAIL;
}

static int PMI_KVS_Put (const char *kvsname, const char *key, const char *value)
{
    int (*f)(const char *, const char *, const char *);
    *(void **)(&f) = dso ? dlsym (dso, "PMI_KVS_Put") : NULL;
    return f ? f (kvsname, key, value) : PMI_FAIL;
}

static int PMI_KVS_Commit (const char *kvsname)
{
    int (*f)(const char *);
    *(void **)(&f) = dso ? dlsym (dso, "PMI_KVS_Commit") : NULL;
    return f ? f (kvsname) : PMI_FAIL;
}

static int PMI_KVS_Get (const char *kvsname, const char *key,
                        char *value, int len)
{
    int (*f)(const char *, const char *, char *, int);
    *(void **)(&f) = dso ? dlsym (dso, "PMI_KVS_Get") : NULL;
    return f ? f (kvsname, key, value, len) : PMI_FAIL;
}

static int PMI_Get_clique_size (int *size)
{
    int (*f)(int *);
    *(void **)(&f) = dso ? dlsym (dso, "PMI_Get_clique_size") : NULL;
    return f ? f (size) : PMI_FAIL;
}

static int PMI_Get_clique_ranks (int *ranks, int length)
{
    int (*f)(int *, int);
    *(void **)(&f) = dso ? dlsym (dso, "PMI_Get_clique_ranks") : NULL;
    return f ? f (ranks, length) : PMI_FAIL;
}

#endif /* !HAVE_FLUX_PMI_LIBRARY */

static int kvs_get(const char key[], char value [], int maxvalue)
{
    int rc;
    rc = PMI_KVS_Get(pmix_kvs_name, key, value, maxvalue);
    if( PMI_SUCCESS != rc ){
        /* silently return an error - might be okay */
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int kvs_put(const char key[], const char value[])
{
    int rc;
    rc = PMI_KVS_Put(pmix_kvs_name, key, value);
    if( PMI_SUCCESS != rc ){
        OPAL_PMI_ERROR(rc, "PMI_KVS_Put");
        return OPAL_ERROR;
    }
    return rc;
}

static int cache_put_uint(opal_process_name_t *id, int type,
                          const char key[], uint64_t val)
{
    char *cpy;
    opal_value_t kv;
    int ret;

    if (!(cpy = strdup (key))) {
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        goto done;
    }
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = cpy;
    kv.type = type;
    switch (type) {
        case OPAL_UINT16:
            kv.data.uint16 = val;
            break;
        case OPAL_UINT32:
            kv.data.uint32 = val;
            break;
        case OPAL_UINT64:
            kv.data.uint64 = val;
            break;
        default:
            ret = OPAL_ERROR;
            goto done_free;
    }
    ret = opal_pmix_base_store(id, &kv);
done_free:
    OBJ_DESTRUCT(&kv);
done:
    if (OPAL_SUCCESS != ret)
        OPAL_ERROR_LOG(ret);
    return ret;
}

static int cache_put_string (opal_process_name_t *id,
                             const char key[], char *val)
{
    char *cpy;
    opal_value_t kv;
    int ret;

    if (!(cpy = strdup (key))) {
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        goto done;
    }
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = cpy;
    kv.type = OPAL_STRING;
    kv.data.string = val;
    ret = opal_pmix_base_store(id, &kv);
    OBJ_DESTRUCT(&kv);
done:
    if (OPAL_SUCCESS != ret)
        OPAL_ERROR_LOG(ret);
    return ret;
}

static int flux_init(opal_list_t *ilist)
{
    int initialized;
    int spawned;
    int rc, ret = OPAL_ERROR;
    int i, rank, lrank, nrank;
    char tmp[64];
    const char *jobid;
    opal_process_name_t ldr;
    char **localranks=NULL;
    opal_process_name_t wildcard_rank;
    char *str;

    if (0 < pmix_init_count) {
        pmix_init_count++;
        return OPAL_SUCCESS;
    }

    if (PMI_SUCCESS != (rc = PMI_Initialized(&initialized))) {
        OPAL_PMI_ERROR(rc, "PMI_Initialized");
        return OPAL_ERROR;
    }

    if (!initialized && PMI_SUCCESS != (rc = PMI_Init(&spawned))) {
        OPAL_PMI_ERROR(rc, "PMI_Init");
        return OPAL_ERROR;
    }

    // setup hash table
    opal_pmix_base_hash_init();

    // Initialize space demands
    rc = PMI_KVS_Get_value_length_max(&pmix_vallen_max);
    if (PMI_SUCCESS != rc) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_value_length_max");
        goto err_exit;
    }
    pmix_vallen_threshold = pmix_vallen_max * 3;
    pmix_vallen_threshold >>= 2;

    rc = PMI_KVS_Get_name_length_max(&pmix_kvslen_max);
    if (PMI_SUCCESS != rc) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_name_length_max");
        goto err_exit;
    }

    rc = PMI_KVS_Get_key_length_max(&pmix_keylen_max);
    if (PMI_SUCCESS != rc) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_key_length_max");
        goto err_exit;
    }

    /* get our rank */
    rc = PMI_Get_rank(&rank);
    if (PMI_SUCCESS != rc) {
        OPAL_PMI_ERROR(rc, "PMI_Get_rank");
        goto err_exit;
    }

    /* get integer job id */
    if (!(jobid = getenv ("FLUX_JOB_ID"))) {
        opal_output(0, "getenv FLUX_JOB_ID [%s:%d:%s]: failed\n",
                    __FILE__, __LINE__, __func__);
        goto err_exit;
    }
    flux_pname.jobid = strtoul(jobid, NULL, 10);
    ldr.jobid = flux_pname.jobid;
    flux_pname.vpid = rank;
    /* store our name in the opal_proc_t so that
     * debug messages will make sense - an upper
     * layer will eventually overwrite it, but that
     * won't do any harm */
    opal_proc_set_name(&flux_pname);
    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:flux: assigned tmp name",
                        OPAL_NAME_PRINT(flux_pname));

    /* setup wildcard rank*/
    wildcard_rank = OPAL_PROC_MY_NAME;
    wildcard_rank.vpid = OPAL_VPID_WILDCARD;

    if (OPAL_SUCCESS != (ret = cache_put_uint (&wildcard_rank,
                                               OPAL_UINT32,
                                               OPAL_PMIX_JOBID,
                                               flux_pname.jobid)))
        goto err_exit;
    if (OPAL_SUCCESS != (ret = cache_put_uint (&OPAL_PROC_MY_NAME,
                                               OPAL_UINT32,
                                               OPAL_PMIX_RANK,
                                               rank)))
        goto err_exit;

    pmix_kvs_name = (char*)malloc(pmix_kvslen_max);
    if (pmix_kvs_name == NULL) {
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        goto err_exit;
    }

    rc = PMI_KVS_Get_my_name(pmix_kvs_name, pmix_kvslen_max);
    if (PMI_SUCCESS != rc) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_my_name");
        goto err_exit;
    }

    /* get our local proc info to find our local rank */
    if (PMI_SUCCESS != (rc = PMI_Get_clique_size(&nlranks))) {
        OPAL_PMI_ERROR(rc, "PMI_Get_clique_size");
        goto err_exit;
    }
    /* save the local size */
    if (OPAL_SUCCESS != (ret = cache_put_uint (&wildcard_rank,
                                               OPAL_UINT32,
                                               OPAL_PMIX_LOCAL_SIZE,
                                               nlranks)))
        goto err_exit;
    lrank = 0;
    nrank = 0;
    ldr.vpid = rank;
    if (0 < nlranks) {
        /* now get the specific ranks */
        lranks = (int*)calloc(nlranks, sizeof(int));
        if (NULL == lranks) {
            ret = OPAL_ERR_OUT_OF_RESOURCE;
            OPAL_ERROR_LOG(rc);
            goto err_exit;
        }
        if (PMI_SUCCESS != (rc = PMI_Get_clique_ranks(lranks, nlranks))) {
            OPAL_PMI_ERROR(rc, "PMI_Get_clique_ranks");
            free(lranks);
            goto err_exit;
        }
        /* note the local ldr */
        ldr.vpid = lranks[0];
        /* save this */
        memset(tmp, 0, 64);
        for (i=0; i < nlranks; i++) {
            (void)snprintf(tmp, 64, "%d", lranks[i]);
            opal_argv_append_nosize(&localranks, tmp);
            if (rank == lranks[i]) {
                lrank = i;
                nrank = i;
            }
        }
        str = opal_argv_join(localranks, ',');
        opal_argv_free(localranks);
        if (OPAL_SUCCESS != (ret = cache_put_string (&wildcard_rank,
                                                     OPAL_PMIX_LOCAL_PEERS,
                                                     str)))
            goto err_exit;
    }

    /* save the local leader */
    if (OPAL_SUCCESS != (ret = cache_put_uint (&OPAL_PROC_MY_NAME,
                                               OPAL_UINT64,
                                               OPAL_PMIX_LOCALLDR,
                                               *(uint64_t*)&ldr)))
        goto err_exit;
    /* save our local rank */
    if (OPAL_SUCCESS != (ret = cache_put_uint (&OPAL_PROC_MY_NAME,
                                               OPAL_UINT16,
                                               OPAL_PMIX_LOCAL_RANK,
                                               lrank)))
        goto err_exit;
    /* and our node rank */
    if (OPAL_SUCCESS != (ret = cache_put_uint (&OPAL_PROC_MY_NAME,
                                               OPAL_UINT16,
                                               OPAL_PMIX_NODE_RANK,
                                               nrank)))
        goto err_exit;
    /* get universe size */
    rc = PMI_Get_universe_size(&i);
    if (PMI_SUCCESS != rc) {
        OPAL_PMI_ERROR(rc, "PMI_Get_universe_size");
        goto err_exit;
    }
    /* push this into the dstore for subsequent fetches */
    if (OPAL_SUCCESS != (ret = cache_put_uint (&wildcard_rank,
                                               OPAL_UINT32,
                                               OPAL_PMIX_UNIV_SIZE,
                                               i)))
        goto err_exit;
    if (OPAL_SUCCESS != (ret = cache_put_uint (&wildcard_rank,
                                               OPAL_UINT32,
                                               OPAL_PMIX_MAX_PROCS,
                                               i)))
        goto err_exit;
    /* get job size */
    rc = PMI_Get_size(&i);
    if (PMI_SUCCESS != rc) {
        OPAL_PMI_ERROR(rc, "PMI_Get_size");
        goto err_exit;
    }
    if (OPAL_SUCCESS != (ret = cache_put_uint (&wildcard_rank,
                                               OPAL_UINT32,
                                               OPAL_PMIX_JOB_SIZE,
                                               i)))
        goto err_exit;

    /* get appnum */
    rc = PMI_Get_appnum(&i);
    if (PMI_SUCCESS != rc) {
        OPAL_PMI_ERROR(rc, "PMI_Get_appnum");
        goto err_exit;
    }
    if (OPAL_SUCCESS != (ret = cache_put_uint (&OPAL_PROC_MY_NAME,
                                               OPAL_UINT32,
                                               OPAL_PMIX_APPNUM,
                                               i)))
        goto err_exit;

    /* increment the init count */
    ++pmix_init_count;

    return OPAL_SUCCESS;

err_exit:
    PMI_Finalize();
    return (ret == OPAL_SUCCESS ? OPAL_ERROR : ret);
}

static int flux_fini(void) {
    if (0 == pmix_init_count) {
        return OPAL_SUCCESS;
    }

    if (0 == --pmix_init_count) {
        PMI_Finalize ();
        // teardown hash table
        opal_pmix_base_hash_finalize();
    }

    return OPAL_SUCCESS;
}

static int flux_initialized(void)
{
    if (0 < pmix_init_count) {
        return 1;
    }
    return 0;
}

static int flux_abort(int flag, const char msg[],
                    opal_list_t *procs)
{
    PMI_Abort(flag, msg);
    return OPAL_SUCCESS;
}

static int flux_spawn(opal_list_t *jobinfo, opal_list_t *apps, opal_jobid_t *jobid)
{
    /*
    int rc;
    size_t preput_vector_size;
    const int info_keyval_sizes[1];
    info_keyval_sizes[0] = (int)opal_list_get_size(info_keyval_vector);
    //FIXME what's the size of array of lists?
    preput_vector_size = opal_list_get_size(preput_keyval_vector);
    rc = PMI_Spawn_multiple(count, cmds, argcs, argvs, maxprocs, info_keyval_sizes, info_keyval_vector, (int)preput_vector_size, preput_keyval_vector);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_Spawn_multiple");
        return OPAL_ERROR;
    }*/
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int flux_put(opal_pmix_scope_t scope,
                  opal_value_t *kv)
{
    int rc;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:flux put for key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), kv->key);

    if (OPAL_SUCCESS != (rc = opal_pmix_base_store_encoded (kv->key, (void*)&kv->data, kv->type, &pmix_packed_data, &pmix_packed_data_offset))) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    if (pmix_packed_data_offset == 0) {
        /* nothing to write */
        return OPAL_SUCCESS;
    }

    if (((pmix_packed_data_offset/3)*4) + pmix_packed_encoded_data_offset < pmix_vallen_max) {
        /* this meta-key is still being filled,
         * nothing to put yet
         */
        return OPAL_SUCCESS;
    }

    rc = opal_pmix_base_partial_commit_packed (&pmix_packed_data, &pmix_packed_data_offset,
                                               &pmix_packed_encoded_data, &pmix_packed_encoded_data_offset,
                                               pmix_vallen_max, &pmix_pack_key, kvs_put);

    flux_committed = false;
    return rc;
}

static int flux_commit(void)
{
    int rc;

    /* check if there is partially filled meta key and put them */
    opal_pmix_base_commit_packed (&pmix_packed_data, &pmix_packed_data_offset,
                                  &pmix_packed_encoded_data, &pmix_packed_encoded_data_offset,
                                  pmix_vallen_max, &pmix_pack_key, kvs_put);

    if (PMI_SUCCESS != (rc = PMI_KVS_Commit(pmix_kvs_name))) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Commit");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int flux_fence(opal_list_t *procs, int collect_data)
{
    int rc;
    if (PMI_SUCCESS != (rc = PMI_Barrier())) {
        OPAL_PMI_ERROR(rc, "PMI_Barrier");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int flux_get(const opal_process_name_t *id,
                  const char *key, opal_list_t *info,
                  opal_value_t **kv)
{
    int rc;
    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:flux called get for key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), key);

    /* Keys presumed stored directly to cache by flux_init() under the
     * wildcard rank must not trigger PMI_KVS_Get() if not found. */
    if (id->vpid == OPAL_VPID_WILDCARD) {
        opal_list_t values;
        OBJ_CONSTRUCT(&values, opal_list_t);
        rc = opal_pmix_base_fetch (id, key, &values);
        OPAL_LIST_DESTRUCT(&values);
        if (OPAL_SUCCESS != rc) {
            return rc;
        }
    }

    rc = opal_pmix_base_cache_keys_locally(id, key, kv, pmix_kvs_name, pmix_vallen_max, kvs_get);
    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:flux got key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), key);

    return rc;
}

static int flux_publish(opal_list_t *info)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int flux_lookup(opal_list_t *data, opal_list_t *info)
{
    // Allocate mem for port here? Otherwise we won't get success!

    return OPAL_ERR_NOT_SUPPORTED;
}

static int flux_unpublish(char **keys, opal_list_t *info)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int flux_job_connect(opal_list_t *procs)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int flux_job_disconnect(opal_list_t *procs)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int flux_store_local(const opal_process_name_t *proc,
                          opal_value_t *val)
{
    opal_pmix_base_store(proc, val);

    return OPAL_SUCCESS;
}

static const char *flux_get_nspace(opal_jobid_t jobid)
{
    return "N/A";
}
static void flux_register_jobid(opal_jobid_t jobid, const char *nspace)
{
    return;
}

static char* pmix_error(int pmix_err)
{
    char * err_msg;

    switch(pmix_err) {
    case PMI_FAIL: err_msg = "Operation failed"; break;
    case PMI_ERR_INIT: err_msg = "PMI is not initialized"; break;
    case PMI_ERR_NOMEM: err_msg = "Input buffer not large enough"; break;
    case PMI_ERR_INVALID_ARG: err_msg = "Invalid argument"; break;
    case PMI_ERR_INVALID_KEY: err_msg = "Invalid key argument"; break;
    case PMI_ERR_INVALID_KEY_LENGTH: err_msg = "Invalid key length argument"; break;
    case PMI_ERR_INVALID_VAL: err_msg = "Invalid value argument"; break;
    case PMI_ERR_INVALID_VAL_LENGTH: err_msg = "Invalid value length argument"; break;
    case PMI_ERR_INVALID_LENGTH: err_msg = "Invalid length argument"; break;
    case PMI_ERR_INVALID_NUM_ARGS: err_msg = "Invalid number of arguments"; break;
    case PMI_ERR_INVALID_ARGS: err_msg = "Invalid args argument"; break;
    case PMI_ERR_INVALID_NUM_PARSED: err_msg = "Invalid num_parsed length argument"; break;
    case PMI_ERR_INVALID_KEYVALP: err_msg = "Invalid keyvalp argument"; break;
    case PMI_ERR_INVALID_SIZE: err_msg = "Invalid size argument"; break;
#if defined(PMI_ERR_INVALID_KVS)
        /* pmix.h calls this a valid return code but mpich doesn't define it */
    case PMI_ERR_INVALID_KVS: err_msg = "Invalid kvs argument"; break;
#endif
    case PMI_SUCCESS: err_msg = "Success"; break;
    default: err_msg = "Unkown error";
    }
    return err_msg;
}
