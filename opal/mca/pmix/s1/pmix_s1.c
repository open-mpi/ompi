/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "opal/util/opal_environ.h"
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
static int s1_put(opal_pmix_scope_t scope,
                  opal_value_t *kv);
static int s1_get(const opal_process_name_t *id,
                  const char *key,
                  opal_value_t **kv);
static int s1_publish(const char service_name[],
                      opal_list_t *info,
                      const char port[]);
static int s1_lookup(const char service_name[],
                     opal_list_t *info,
                     char port[], int portLen);
static int s1_unpublish(const char service_name[],
                        opal_list_t *info);
static bool s1_get_attr(const char *attr, opal_value_t **kv);
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
    NULL,
    s1_put,
    s1_get,
    NULL,
    s1_publish,
    s1_lookup,
    s1_unpublish,
    s1_get_attr,
    NULL,
    s1_spawn,
    s1_job_connect,
    s1_job_disconnect,
    NULL,
    NULL
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
static bool s1_committed = false;
static char* pmix_packed_data = NULL;
static int pmix_packed_data_offset = 0;
static char* pmix_packed_encoded_data = NULL;
static int pmix_packed_encoded_data_offset = 0;
static int pmix_pack_key = 0;
static uint32_t s1_jobid;
static int s1_rank;
static uint16_t s1_lrank;
static uint16_t s1_nrank;
static int s1_usize;
static int s1_jsize;
static int s1_appnum;
static int s1_nlranks;
static int *s1_lranks=NULL;
static opal_process_name_t s1_pname;

static bool got_modex_data = false;
static char* pmix_error(int pmix_err);
#define OPAL_PMI_ERROR(pmi_err, pmi_func)                       \
    do {                                                        \
        opal_output(0, "%s [%s:%d:%s]: %s\n",                   \
                    pmi_func, __FILE__, __LINE__, __func__,     \
                    pmix_error(pmi_err));                        \
    } while(0);

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

static int s1_init(void)
{
    PMI_BOOL initialized;
    int spawned;
    int rc, ret = OPAL_ERROR;
    int i;
    char *pmix_id, *tmp;
    uint32_t jobfam, stepid;
    opal_value_t kv;

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
    pmix_vallen_threshold = pmix_vallen_max * 3;
    pmix_vallen_threshold >>= 2;

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
    /* Slurm PMI provides the job id as an integer followed
     * by a '.', followed by essentially a stepid. The first integer
     * defines an overall job number. The second integer is the number of
     * individual jobs we have run within that allocation. So we translate
     * this as the overall job number equating to our job family, and
     * the individual number equating to our local jobid
     */
    jobfam = strtoul(pmix_id, &tmp, 10);
    if (NULL == tmp) {
        /* hmmm - no '.', so let's just use zero */
        stepid = 0;
    } else {
        tmp++; /* step over the '.' */
        stepid = strtoul(tmp, NULL, 10);
    }
    /* now build the jobid */
    s1_jobid = (jobfam << 16) | stepid;
    free(pmix_id);

    /* get our rank */
    ret = PMI_Get_rank(&s1_rank);
    if( PMI_SUCCESS != ret ) {
        OPAL_PMI_ERROR(ret, "PMI_Get_rank");
        goto err_exit;
    }
    /* store our name in the opal_proc_t so that
     * debug messages will make sense - an upper
     * layer will eventually overwrite it, but that
     * won't do any harm */
    s1_pname.jobid = s1_jobid;
    s1_pname.vpid = s1_rank;
    opal_proc_set_name(&s1_pname);
    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s1: assigned tmp name",
                        OPAL_NAME_PRINT(s1_pname));

    pmix_kvs_name = (char*)malloc(pmix_kvslen_max);
    if( pmix_kvs_name == NULL ){
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        goto err_exit;
    }

    rc = PMI_KVS_Get_my_name(pmix_kvs_name, pmix_kvslen_max);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_my_name");
        goto err_exit;
    }

    /* get our local proc info to find our local rank */
    if (PMI_SUCCESS != (rc = PMI_Get_clique_size(&s1_nlranks))) {
        OPAL_PMI_ERROR(rc, "PMI_Get_clique_size");
        return rc;
    }
    /* now get the specific ranks */
    s1_lranks = (int*)calloc(s1_nlranks, sizeof(int));
    if (NULL == s1_lranks) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        OPAL_ERROR_LOG(rc);
        return rc;
    }
    if (PMI_SUCCESS != (rc = PMI_Get_clique_ranks(s1_lranks, s1_nlranks))) {
        OPAL_PMI_ERROR(rc, "PMI_Get_clique_ranks");
        free(s1_lranks);
        return rc;
    }
    /* find ourselves */
    for (i=0; i < s1_nlranks; i++) {
        if (s1_rank == s1_lranks[i]) {
            s1_lrank = i;
            s1_nrank = i;
            break;
        }
    }

    /* get universe size */
    ret = PMI_Get_universe_size(&s1_usize);
    if (PMI_SUCCESS != ret) {
        OPAL_PMI_ERROR(ret, "PMI_Get_universe_size");
        goto err_exit;
    }
    /* push this into the dstore for subsequent fetches */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_DSTORE_UNIV_SIZE);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = s1_usize;
    if (OPAL_SUCCESS != (ret = opal_dstore.store(opal_dstore_internal, &OPAL_PROC_MY_NAME, &kv))) {
        OPAL_ERROR_LOG(ret);
        OBJ_DESTRUCT(&kv);
        goto err_exit;
    }
    OBJ_DESTRUCT(&kv);

    /* get job size */
    ret = PMI_Get_size(&s1_jsize);
    if (PMI_SUCCESS != ret) {
        OPAL_PMI_ERROR(ret, "PMI_Get_size");
        goto err_exit;
    }

    /* get appnum */
    ret = PMI_Get_appnum(&s1_appnum);
    if (PMI_SUCCESS != ret) {
        OPAL_PMI_ERROR(ret, "PMI_Get_appnum");
        goto err_exit;
    }

    /* increment the init count */
    ++pmix_init_count;
    
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

    if (NULL != s1_lranks) {
        free(s1_lranks);
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

static int s1_put(opal_pmix_scope_t scope,
                  opal_value_t *kv)
{
    int rc;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s1 put for key %s",
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

    s1_committed = false;
    return rc;
}

static int s1_fence(opal_process_name_t *procs, size_t nprocs)
{
    int rc;
    int32_t i;
    opal_value_t *kp, kvn;
    opal_hwloc_locality_t locality;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s1 called fence",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    /* check if there is partially filled meta key and put them */
    opal_pmix_base_commit_packed (&pmix_packed_data, &pmix_packed_data_offset,
                                  &pmix_packed_encoded_data, &pmix_packed_encoded_data_offset,
                                  pmix_vallen_max, &pmix_pack_key, kvs_put);

    /* if we haven't already done it, ensure we have committed our values */
    if (!s1_committed) {
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s pmix:s1 committing values",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
        if (PMI_SUCCESS != (rc = PMI_KVS_Commit(pmix_kvs_name))) {
            OPAL_PMI_ERROR(rc, "PMI_KVS_Commit");
            return OPAL_ERROR;
        }
    }

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s1 performing barrier",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    /* use the PMI barrier function */
    if (PMI_SUCCESS != (rc = PMI_Barrier())) {
        OPAL_PMI_ERROR(rc, "PMI_Barrier");
        return OPAL_ERROR;
    }

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s1 barrier complete",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    /* get the modex data from each local process and set the
     * localities to avoid having the MPI layer fetch data
     * for every process in the job */
    if (!got_modex_data) {
        got_modex_data = true;
        /* we only need to set locality for each local rank as "not found"
         * equates to "non-local" */
        for (i=0; i < s1_nlranks; i++) {
            s1_pname.vpid = s1_lranks[i];
            rc = opal_pmix_base_cache_keys_locally(&s1_pname, OPAL_DSTORE_CPUSET,
                                                   &kp, pmix_kvs_name, pmix_vallen_max, kvs_get);
            if (OPAL_SUCCESS != rc) {
                OPAL_ERROR_LOG(rc);
                return rc;
            }
#if OPAL_HAVE_HWLOC
            if (NULL == kp || NULL == kp->data.string) {
                /* if we share a node, but we don't know anything more, then
                 * mark us as on the node as this is all we know
                 */
                locality = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
            } else {
                /* determine relative location on our node */
                locality = opal_hwloc_base_get_relative_locality(opal_hwloc_topology,
                                                                 opal_process_info.cpuset,
                                                                 kp->data.string);
            }
            if (NULL != kp) {
                OBJ_RELEASE(kp);
            }
#else
            /* all we know is we share a node */
            locality = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
#endif
            OPAL_OUTPUT_VERBOSE((1, opal_pmix_base_framework.framework_output,
                                 "%s pmix:s1 proc %s locality %s",
                                 OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                 OPAL_NAME_PRINT(s1_pname),
                                 opal_hwloc_base_print_locality(locality)));
    
            OBJ_CONSTRUCT(&kvn, opal_value_t);
            kvn.key = strdup(OPAL_DSTORE_LOCALITY);
            kvn.type = OPAL_UINT16;
            kvn.data.uint16 = locality;
            (void)opal_dstore.store(opal_dstore_internal, &s1_pname, &kvn);
            OBJ_DESTRUCT(&kvn);
        }
    }

    return OPAL_SUCCESS;
}

static int s1_get(const opal_process_name_t *id,
                  const char *key,
                  opal_value_t **kv)
{
    int rc;
    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s1 called get for key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), key);

    rc = opal_pmix_base_cache_keys_locally(id, key, kv, pmix_kvs_name, pmix_vallen_max, kvs_get);
     opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s1 got key %s",
                         OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), key);

   return rc;
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
    opal_value_t *kp;

    if (0 == strcmp(PMIX_JOBID, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s1_jobid;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_RANK, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s1_rank;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_UNIV_SIZE, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s1_usize;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_JOB_SIZE, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s1_jsize;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_LOCAL_SIZE, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s1_nlranks;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_APPNUM, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s1_appnum;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_LOCAL_RANK, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s1_lrank;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_NODE_RANK, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s1_nrank;
        *kv = kp;
        return true;
    }

    return false;
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
