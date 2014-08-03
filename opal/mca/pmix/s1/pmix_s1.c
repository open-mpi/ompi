/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"
#include "opal/types.h"

#include "opal/mca/hwloc/base/base.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"

#include <string.h>
#include <pmi.h>

#include "opal/mca/pmix/base/base.h"
#include "pmix_s1.h"

static int s1_init(void);
static int s1_fini(void);
static bool s1_initialized(void);
static int s1_abort(int flag, const char msg[]);
static int s1_fence(opal_process_name_t *procs, size_t nprocs);
static int s1_fence_nb(opal_process_name_t *procs, size_t nprocs,
                       opal_pmix_cbfunc_t cbfunc, void *cbdata);
static int s1_put(opal_pmix_scope_t scope,
                  opal_value_t *kv);
static int s1_get(const opal_identifier_t *id,
                  const char *key,
                  opal_value_t **kv);
static void s1_get_nb(const opal_identifier_t *id,
                      const char *key,
                      opal_pmix_cbfunc_t cbfunc,
                      void *cbdata);
static int s1_publish(const char service_name[],
                      opal_list_t *info,
                      const char port[]);
static int s1_lookup(const char service_name[],
                     opal_list_t *info,
                     char port[], int portLen);
static int s1_unpublish(const char service_name[],
                        opal_list_t *info);
static bool s1_get_attr(const char *attr, opal_value_t **kv);
static int s1_get_attr_nb(const char *attr,
                          opal_pmix_cbfunc_t cbfunc,
                          void *cbdata);
static int s1_spawn(int count, const char * cmds[],
                    int argcs[], const char ** argvs[],
                    const int maxprocs[],
                    opal_list_t *info_keyval_vector,
                    opal_list_t *preput_keyval_vector,
                    char jobId[], int jobIdSize,
                    int errors[]);
static int s1_job_connect(const char jobId[]);
static int s1_job_disconnect(const char jobId[]);

const opal_pmix_base_module_t opal_pmix_s1_module = {
    s1_init,
    s1_fini,
    s1_initialized,
    s1_abort,
    s1_fence,
    s1_fence_nb,
    s1_put,
    s1_get,
    s1_get_nb,
    s1_publish,
    s1_lookup,
    s1_unpublish,
    s1_get_attr,
    s1_get_attr_nb,
    s1_spawn,
    s1_job_connect,
    s1_job_disconnect
};

// usage accounting
static int pmix_init_count = 0;

// PMI constant values:
static int pmix_kvslen_max = 0;
static int pmix_keylen_max = 0;
static int pmix_vallen_max = 0;

// Job environment description
static char *pmix_kvs_name = NULL;
static char *pmix_id = NULL;
static bool s1_committed = false;
static char* pmix_packed_data = NULL;
static int pmix_packed_data_offset = 0;
static int pmix_pack_key = 0;

static char* pmix_error(int pmix_err);
#define OPAL_PMI_ERROR(pmi_err, pmi_func)                       \
    do {                                                        \
        opal_output(0, "%s [%s:%d:%s]: %s\n",                   \
                    pmi_func, __FILE__, __LINE__, __func__,     \
                    pmix_error(pmi_err));                        \
    } while(0);

static int s1_init(void)
{
    PMI_BOOL initialized;
    int spawned;
    int rc, ret = OPAL_ERROR;

    if (PMI_SUCCESS != (rc = PMI_Initialized(&initialized))) {
        OPAL_PMI_ERROR(rc, "PMI_Initialized");
        return OPAL_ERROR;
    }

    if( PMI_TRUE != initialized && PMI_SUCCESS != (rc = PMI_Init(&spawned)) ) {
        OPAL_PMI_ERROR(rc, "PMI_Init");
        return OPAL_ERROR;
    }

    // Initialize space demands
    rc = PMI_KVS_Get_value_length_max(&pmix_vallen_max);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_value_length_max");
        goto err_exit;
    }

    rc = PMI_KVS_Get_name_length_max(&pmix_kvslen_max);
    if (PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_name_length_max");
        goto err_exit;
    }

    rc = PMI_KVS_Get_key_length_max(&pmix_keylen_max);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_key_length_max");
        goto err_exit;
    }

    // Initialize job environment information
    pmix_id = (char*)malloc(pmix_vallen_max);
    if( pmix_id == NULL ){
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        goto err_exit;
    }
    /* Get domain id */
    if (PMI_SUCCESS != (rc = PMI_Get_kvs_domain_id(pmix_id, pmix_vallen_max))) {
        free(pmix_id);
        goto err_exit;
    }

    pmix_kvs_name = (char*)malloc(pmix_kvslen_max);
    if( pmix_kvs_name == NULL ){
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        goto err_exit;
    }

    rc = PMI_KVS_Get_my_name(pmix_kvs_name, pmix_kvslen_max);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI2_Job_GetId");
        goto err_exit;
    }

    return OPAL_SUCCESS;

err_exit:
    PMI_Finalize();
    return ret;
}

static int s1_fini(void) {
    if (0 == pmix_init_count) {
        return OPAL_SUCCESS;
    }

    if (0 == --pmix_init_count) {
        PMI_Finalize ();
    }
    return OPAL_SUCCESS;
}

static bool s1_initialized(void)
{
    if (0 < pmix_init_count) {
        return true;
    }
    return false;
}

static int s1_abort(int flag, const char msg[])
{
    PMI_Abort(flag, msg);
    return OPAL_SUCCESS;
}

static int s1_spawn(int count, const char * cmds[],
                    int argcs[], const char ** argvs[],
                    const int maxprocs[],
                    opal_list_t *info_keyval_vector,
                    opal_list_t *preput_keyval_vector,
                    char jobId[], int jobIdSize,
                    int errors[])
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

static int s1_put(opal_pmix_scope_t scope,
                  opal_value_t *kv)
{
    int rc;
    char* buffer_to_put;
    int rem_offset = 0;
    int data_to_put = 0;
    if (OPAL_SUCCESS != (rc = pmix_store_encoded (kv->key, (void*)&kv->data, kv->type, &pmix_packed_data, &pmix_packed_data_offset))) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    if (pmix_packed_data_offset == 0) {
        /* nothing to write */
        return OPAL_SUCCESS;
    }

    if (pmix_packed_data_offset < pmix_vallen_max) {
        /* this meta-key is still being filled,
         * nothing to put yet
         */
        return OPAL_SUCCESS;
    }

    /* encode only full filled meta keys */
    rem_offset = pmix_packed_data_offset % pmix_vallen_max;
    data_to_put = pmix_packed_data_offset - rem_offset;
    buffer_to_put = (char*)malloc(data_to_put);
    memcpy(buffer_to_put, pmix_packed_data, data_to_put);

    pmix_commit_packed (buffer_to_put, data_to_put, pmix_vallen_max, &pmix_pack_key, kvs_put);

    free(buffer_to_put);
    pmix_packed_data_offset = rem_offset;
    if (0 == pmix_packed_data_offset) {
        free(pmix_packed_data);
        pmix_packed_data = NULL;
    } else {
        memmove (pmix_packed_data, pmix_packed_data + data_to_put, pmix_packed_data_offset);
        pmix_packed_data = realloc (pmix_packed_data, pmix_packed_data_offset);
    }

    s1_committed = false;
    return rc;
}

static int s1_fence(opal_process_name_t *procs, size_t nprocs)
{
    int rc;
    int i;
    int *ranks, ps;
    opal_process_name_t name;
    opal_value_t *kp, kvn;
    opal_hwloc_locality_t locality;
#if OPAL_HAVE_HWLOC
    char *cpuset;
    opal_list_t vals;
#endif

    /* check if there is partially filled meta key and put them */
    if (0 != pmix_packed_data_offset && NULL != pmix_packed_data) {
        pmix_commit_packed(pmix_packed_data, pmix_packed_data_offset, pmix_vallen_max, &pmix_pack_key, kvs_put);
        pmix_packed_data_offset = 0;
        free(pmix_packed_data);
        pmix_packed_data = NULL;
    }
    /* if we haven't already done it, ensure we have committed our values */
    if (!s1_committed) {
        if (PMI_SUCCESS != (rc = PMI_KVS_Commit(pmix_kvs_name))) {
            OPAL_PMI_ERROR(rc, "PMI_KVS_Commit");
            return OPAL_ERROR;
        }
    }

    /* use the PMI barrier function */
    if (PMI_SUCCESS != (rc = PMI_Barrier())) {
        OPAL_PMI_ERROR(rc, "PMI_Barrier");
        return OPAL_ERROR;
    }

    /*** RHC: you'll need to do add some logic here to get the
     * modex data from each local process so you can set the
     * localities */
    if (!s1_committed) {
        s1_committed = true;
        /* get our local proc info to find our local rank */
        if (PMI_SUCCESS != (rc = PMI_Get_clique_size(&ps))) {
            OPAL_PMI_ERROR(rc, "PMI_Get_clique_size");
            return rc;
        }
        /* now get the specific ranks */
        ranks = (int*)calloc(ps, sizeof(int));
        if (NULL == ranks) {
            OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
            return rc;
        }
        if (PMI_SUCCESS != (rc = PMI_Get_clique_ranks(ranks, ps))) {
            OPAL_PMI_ERROR(rc, "PMI_Get_clique_ranks");
            return rc;
        }

        /* set locality */
        name = OPAL_PROC_MY_NAME;
        for (i=0; i < ps; i++) {
            name = (name & 0xffff0000) | (ranks[i] & 0x0000ffff);
#if OPAL_HAVE_HWLOC
            OBJ_CONSTRUCT(&vals, opal_list_t);
            if (OPAL_SUCCESS != (rc = opal_dstore.fetch(opal_dstore_internal, &name,
                                                        OPAL_DSTORE_CPUSET, &vals))) {
                OPAL_LIST_DESTRUCT(&vals);
                continue;
            }
            kp = (opal_value_t*)opal_list_get_first(&vals);
            cpuset = kp->data.string;
            if (NULL == cpuset) {
                /* if we share a node, but we don't know anything more, then
                 * mark us as on the node as this is all we know
                 */
                locality = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
            } else {
                /* determine relative location on our node */
                locality = opal_hwloc_base_get_relative_locality(opal_hwloc_topology,
                                                                 opal_process_info.cpuset,
                                                                 (char *) cpuset);
            }
            OPAL_LIST_DESTRUCT(&vals);
#else
            /* all we know is we share a node */
            locality = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
#endif
            OPAL_OUTPUT_VERBOSE((1, opal_pmix_base_framework.framework_output,
                                 "%s pmix:native proc %s locality %s",
                                 OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                 OPAL_NAME_PRINT(name), opal_hwloc_base_print_locality(locality)));
    
            OBJ_CONSTRUCT(&kvn, opal_value_t);
            kvn.key = strdup(OPAL_DSTORE_LOCALITY);
            kvn.type = OPAL_UINT16;
            kvn.data.uint16 = locality;
            (void)opal_dstore.store(opal_dstore_internal, &name, &kvn);
            OBJ_DESTRUCT(&kvn);
        }
    }

    return OPAL_SUCCESS;
}

static int s1_fence_nb(opal_process_name_t *procs, size_t nprocs,
                       opal_pmix_cbfunc_t cbfunc, void *cbdata)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int kvs_get(const char key[], char value [], int maxvalue)
{
    int rc;
    rc = PMI_KVS_Get(pmix_kvs_name, key, value, maxvalue);
    if( PMI_SUCCESS != rc ){
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int s1_get(const opal_identifier_t *id,
                  const char *key,
                  opal_value_t **kv)
{
    int rc;
    rc = cache_keys_locally(id, key, kv, pmix_kvs_name, pmix_vallen_max, kvs_get);
    if (NULL == *kv) {
        return OPAL_ERROR;
    }
    return rc;
}

static void s1_get_nb(const opal_identifier_t *id,
                      const char *key,
                      opal_pmix_cbfunc_t cbfunc,
                      void *cbdata)
{
    return;
}

static int s1_publish(const char service_name[],
                      opal_list_t *info,
                      const char port[])
{
    int rc;

    if (PMI_SUCCESS != (rc = PMI_Publish_name(service_name, port))) {
        OPAL_PMI_ERROR(rc, "PMI_Publish_name");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int s1_lookup(const char service_name[],
                     opal_list_t *info,
                     char port[], int portLen)
{
    int rc;

    // Allocate mem for port here? Otherwise we won't get success!
    // SLURM PMIv1 doesn't implement this function
    /* I don't understand this comment. Is it still valid? */
    if (PMI_SUCCESS != (rc = PMI_Lookup_name(service_name, port))) {
        OPAL_PMI_ERROR(rc, "PMI_Lookup_name");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int s1_unpublish(const char service_name[],
                        opal_list_t *info)
{
    int rc;

    if (PMI_SUCCESS != (rc = PMI_Unpublish_name(service_name))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_unpublish");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static bool s1_get_attr(const char *attr, opal_value_t **kv)
{
    int rc, i;
    char *tmp, *t2;
    uint32_t jobid, jobfam, stepid;
    opal_value_t *kp;

    if (0 == strcmp(PMIX_JOBID, attr)) {
        /* Slurm PMI provides the job id as an integer followed
         * by a '.', followed by essentially a stepid. The first integer
         * defines an overall job number. The second integer is the number of
         * individual jobs we have run within that allocation. So we translate
         * this as the overall job number equating to our job family, and
         * the individual number equating to our local jobid
         */
        t2 = strdup(pmix_id);
        jobfam = strtoul(t2, &tmp, 10);
        if (NULL == tmp) {
            /* hmmm - no '.', so let's just use zero */
            stepid = 0;
        } else {
            tmp++; /* step over the '.' */
            stepid = strtoul(tmp, NULL, 10);
        }
        free(t2);
        /* now build the jobid */
        jobid = (jobfam << 16) | stepid;
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = jobid;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_RANK, attr)) {
        rc = PMI_Get_rank(&i);
        if( PMI_SUCCESS != rc ) {
            OPAL_PMI_ERROR(rc, "PMI_Get_rank");
            return false;
        }
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = i;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_UNIV_SIZE, attr)) {
        rc = PMI_Get_universe_size(&i);
        if( PMI_SUCCESS != rc ) {
            OPAL_PMI_ERROR(rc, "PMI_Get_universe_size");
            return false;
        }
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = i;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_JOB_SIZE, attr)) {
        rc = PMI_Get_size(&i);
        if( PMI_SUCCESS != rc ) {
            OPAL_PMI_ERROR(rc, "PMI_Get_size");
            return false;
        }
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = i;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_APPNUM, attr)) {
        rc = PMI_Get_appnum(&i);
        if( PMI_SUCCESS != rc ) {
            OPAL_PMI_ERROR(rc, "PMI_Get_appnum");
            return false;
        }
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = i;
        *kv = kp;
        return true;
    }

    return OPAL_SUCCESS;
}

static int s1_get_attr_nb(const char *attr,
                          opal_pmix_cbfunc_t cbfunc,
                          void *cbdata)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int s1_job_connect(const char jobId[])
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int s1_job_disconnect(const char jobId[])
{
    return OPAL_ERR_NOT_SUPPORTED;
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
	/* pmix.h calls this a valid return code but mpich doesn't define it (slurm does). */
        case PMI_ERR_INVALID_KVS: err_msg = "Invalid kvs argument"; break;
#endif
        case PMI_SUCCESS: err_msg = "Success"; break;
        default: err_msg = "Unkown error";
    }
    return err_msg;
}
