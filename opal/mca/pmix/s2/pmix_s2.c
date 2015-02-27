/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
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

#include "pmi2_pmap_parser.h"

#include <string.h>
#include <pmi.h>
#include <pmi2.h>

#include "opal/mca/pmix/base/base.h"
#include "pmix_s2.h"

static int s2_init(void);
static int s2_fini(void);
static bool s2_initialized(void);
static int s2_abort(int flag, const char msg[]);
static int s2_spawn(int count, const char * cmds[],
        int argcs[], const char ** argvs[],
        const int maxprocs[],
        opal_list_t *info_keyval_vector,
        opal_list_t *preput_keyval_vector,
        char jobId[], int jobIdSize,
        int errors[]);
static int s2_put(opal_pmix_scope_t scope,
                  opal_value_t *kv);
static int s2_fence(opal_process_name_t *procs, size_t nprocs);
static int s2_get(const opal_process_name_t *id,
                  const char *key,
                  opal_value_t **kv);
static int s2_publish(const char service_name[],
                      opal_list_t *info,
                      const char port[]);
static int s2_lookup(const char service_name[],
                     opal_list_t *info,
                     char port[], int portLen);
static int s2_unpublish(const char service_name[],
                        opal_list_t *info);
static bool s2_get_attr(const char *attr, opal_value_t **kv);
static int s2_job_connect(const char jobId[]);
static int s2_job_disconnect(const char jobId[]);

const opal_pmix_base_module_t opal_pmix_s2_module = {
    s2_init,
    s2_fini,
    s2_initialized,
    s2_abort,
    s2_fence,
    NULL,
    s2_put,
    s2_get,
    NULL,
    s2_publish,
    s2_lookup,
    s2_unpublish,
    s2_get_attr,
    NULL,
    s2_spawn,
    s2_job_connect,
    s2_job_disconnect,
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

static char* pmix_packed_data = NULL;
static int pmix_packed_data_offset = 0;
static char* pmix_packed_encoded_data = NULL;
static int pmix_packed_encoded_data_offset = 0;
static int pmix_pack_key = 0;

static uint32_t s2_jobid;
static int s2_rank;
static uint16_t s2_lrank;
static uint16_t s2_nrank;
static int s2_usize;
static int s2_jsize;
static int s2_appnum;
static int s2_nlranks;
static int *s2_lranks=NULL;
static opal_process_name_t s2_pname;

static bool got_modex_data = false;
static char* pmix_error(int pmix_err);
#define OPAL_PMI_ERROR(pmi_err, pmi_func)                       \
    do {                                                        \
        opal_output(0, "%s [%s:%d:%s]: %s\n",                   \
                    pmi_func, __FILE__, __LINE__, __func__,     \
                    pmix_error(pmi_err));                        \
    } while(0);

static int kvs_put(const char key[], const char value[])
{
    int rc;
    rc = PMI2_KVS_Put(key, value);
    if( PMI2_SUCCESS != rc ){
        OPAL_PMI_ERROR(rc, "PMI2_KVS_Put");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int kvs_get(const char key[], char value [], int maxvalue)
{
    int rc;
    int len;
    rc = PMI2_KVS_Get(pmix_kvs_name, PMI2_ID_NULL, key, value, maxvalue, &len);
    if( PMI2_SUCCESS != rc || len < 0){
        OPAL_PMI_ERROR(rc, "PMI2_KVS_Get");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int s2_init(void)
{
    int spawned, size, rank, appnum;
    int rc, ret = OPAL_ERROR;
    char buf[16];
    int found;
    int my_node;
    char *tmp;
    uint32_t jobfam, stepid;
    int i;

    /* if we can't startup PMI, we can't be used */
    if ( PMI2_Initialized () ) {
        return OPAL_SUCCESS;
    }
    size = -1;
    rank = -1;
    appnum = -1;
    if (PMI2_SUCCESS != (rc = PMI2_Init(&spawned, &size, &rank, &appnum))) {
        opal_show_help("help-pmix-base.txt", "pmix2-init-failed", true, rc);
        return OPAL_ERROR;
    }
    if( size < 0 || rank < 0 ){
        opal_show_help("help-pmix-base.txt", "pmix2-init-returned-bad-values", true);
        goto err_exit;
    }

    s2_jsize = size;
    s2_rank = rank;
    s2_appnum = appnum;

    pmix_vallen_max = PMI2_MAX_VALLEN;
    pmix_kvslen_max = PMI2_MAX_VALLEN; // FIX ME: What to put here for versatility?
    pmix_keylen_max = PMI2_MAX_KEYLEN;
    pmix_vallen_threshold = PMI2_MAX_VALLEN * 3;
    pmix_vallen_threshold >>= 2;

    rc = PMI2_Info_GetJobAttr("universeSize", buf, 16, &found);
    if( PMI2_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_Get_universe_size");
        goto err_exit;
    }
    s2_usize = atoi(buf);

    pmix_kvs_name = (char*)malloc(pmix_kvslen_max);
    if( pmix_kvs_name == NULL ){
        PMI2_Finalize();
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        goto err_exit;
    }
    rc = PMI2_Job_GetId(pmix_kvs_name, pmix_kvslen_max);
    if( PMI2_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI2_Job_GetId");
        goto err_exit;
    }

    /* Slurm PMI provides the job id as an integer followed
     * by a '.', followed by essentially a stepid. The first integer
     * defines an overall job number. The second integer is the number of
     * individual jobs we have run within that allocation. So we translate
     * this as the overall job number equating to our job family, and
     * the individual number equating to our local jobid
     */

    jobfam = strtoul(pmix_kvs_name, &tmp, 10);
    if (NULL == tmp) {
        /* hmmm - no '.', so let's just use zero */
        stepid = 0;
    } else {
        tmp++; /* step over the '.' */
        stepid = strtoul(tmp, NULL, 10);
    }
    /* now build the jobid */
    s2_jobid = (jobfam << 16) | stepid;

    /* store our name in the opal_proc_t so that
     * debug messages will make sense - an upper
     * layer will eventually overwrite it, but that
     * won't do any harm */
    s2_pname.jobid = s2_jobid;
    s2_pname.vpid = s2_rank;
    opal_proc_set_name(&s2_pname);
    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s2: assigned tmp name",
                        OPAL_NAME_PRINT(s2_pname));

    char *pmapping = (char*)malloc(PMI2_MAX_VALLEN);
    if( pmapping == NULL ){
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    rc = PMI2_Info_GetJobAttr("PMI_process_mapping", pmapping, PMI2_MAX_VALLEN, &found);
    if( !found || PMI2_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc,"PMI2_Info_GetJobAttr");
        return OPAL_ERROR;
    }

    s2_lranks = mca_common_pmi2_parse_pmap(pmapping, s2_pname.vpid, &my_node, &s2_nlranks);
    if (NULL == s2_lranks) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    free(pmapping);

    /* find ourselves */
    for (i=0; i < s2_nlranks; i++) {
        if (s2_rank == s2_lranks[i]) {
            s2_lrank = i;
            s2_nrank = i;
            break;
        }
    }

    /* increment the init count */
    ++pmix_init_count;
    
    return OPAL_SUCCESS;
 err_exit:
    PMI2_Finalize();
    return ret;
}

static int s2_fini(void) {
    if (0 == pmix_init_count) {
        return OPAL_SUCCESS;
    }

    if (0 == --pmix_init_count) {
        PMI2_Finalize();
    }
    if (NULL != pmix_kvs_name) {
        free(pmix_kvs_name);
        pmix_kvs_name = NULL;
    }
    if (NULL != s2_lranks) {
        free(s2_lranks);
    }
    return OPAL_SUCCESS;
}

static bool s2_initialized(void)
{
    if (0 < pmix_init_count) {
        return true;
    }
    return false;
}

static int s2_abort(int flag, const char msg[])
{
    PMI2_Abort(flag, msg);
    return OPAL_SUCCESS;
}

static int s2_spawn(int count, const char * cmds[],
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
    rc = PMI2_Job_Spawn(count, cmds, argcs, argvs, maxprocs, info_keyval_sizes, info_keyval_vector, (int)preput_vector_size, preput_keyval_vector, jobId, jobIdSize, errors);
    if( PMI2_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI2_Job_Spawn");
        return OPAL_ERROR;
    }*/
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s2_job_connect(const char jobId[])
{
    int rc;
    PMI2_Connect_comm_t conn;
    /*FIXME should change function prototype to add void* conn */
    rc = PMI2_Job_Connect(jobId, &conn);
    if( PMI2_SUCCESS != rc ){
        OPAL_PMI_ERROR(rc, "PMI2_Job_Connect");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int s2_job_disconnect(const char jobId[])
{
    int rc;
    rc = PMI2_Job_Disconnect(jobId);
    if( PMI2_SUCCESS != rc ){
        OPAL_PMI_ERROR(rc, "PMI2_Job_Disconnect");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int s2_put(opal_pmix_scope_t scope,
                  opal_value_t *kv)
{
    int rc;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
            "%s pmix:s2 put for key %s",
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

    return rc;
}

static int s2_fence(opal_process_name_t *procs, size_t nprocs)
{
    int rc;
    int32_t i;
    opal_value_t *kp, kvn;
    opal_hwloc_locality_t locality;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s2 called fence",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    /* check if there is partially filled meta key and put them */
    opal_pmix_base_commit_packed (&pmix_packed_data, &pmix_packed_data_offset,
                                  &pmix_packed_encoded_data, &pmix_packed_encoded_data_offset,
                                  pmix_vallen_max, &pmix_pack_key, kvs_put);

    if (PMI2_SUCCESS != (rc = PMI2_KVS_Fence())) {
        OPAL_PMI_ERROR(rc, "PMI2_KVS_Fence");
        return OPAL_ERROR;
    }

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:s2 kvs_fence complete",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    /* get the modex data from each local process and set the
     * localities to avoid having the MPI layer fetch data
     * for every process in the job */
    if (!got_modex_data) {
        got_modex_data = true;
        /* we only need to set locality for each local rank as "not found"
         * equates to "non-local" */
        for (i=0; i < s2_nlranks; i++) {
            s2_pname.vpid = s2_lranks[i];
            rc = opal_pmix_base_cache_keys_locally(&s2_pname, OPAL_DSTORE_CPUSET,
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
                                 "%s pmix:s2 proc %s locality %s",
                                 OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                 OPAL_NAME_PRINT(s2_pname),
                                 opal_hwloc_base_print_locality(locality)));

            OBJ_CONSTRUCT(&kvn, opal_value_t);
            kvn.key = strdup(OPAL_DSTORE_LOCALITY);
            kvn.type = OPAL_UINT16;
            kvn.data.uint16 = locality;
            (void)opal_dstore.store(opal_dstore_internal, &s2_pname, &kvn);
            OBJ_DESTRUCT(&kvn);
        }
    }

    return OPAL_SUCCESS;
}

static int s2_get(const opal_process_name_t *id,
                  const char *key,
                  opal_value_t **kv)
{
    int rc;
    rc = opal_pmix_base_cache_keys_locally(id, key, kv, pmix_kvs_name, pmix_vallen_max, kvs_get);
    return rc;
}

static int s2_publish(const char service_name[],
                      opal_list_t *info,
                      const char port[])
{
    int rc;

    if (PMI2_SUCCESS != (rc = PMI2_Nameserv_publish(service_name, NULL, port))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_publish");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int s2_lookup(const char service_name[],
                     opal_list_t *info,
                     char port[], int portLen)
{
    int rc;

    if (PMI2_SUCCESS != (rc = PMI2_Nameserv_lookup(service_name, NULL, port, portLen))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_lookup");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int s2_unpublish(const char service_name[],
                        opal_list_t *info)
{
    int rc;

    if (PMI2_SUCCESS != (rc = PMI2_Nameserv_unpublish(service_name, NULL))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_unpublish");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;;
}

static bool s2_get_attr(const char *attr, opal_value_t **kv)
{
    opal_value_t *kp;

    if (0 == strcmp(PMIX_JOBID, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s2_jobid;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_RANK, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s2_rank;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_UNIV_SIZE, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s2_usize;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_JOB_SIZE, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s2_jsize;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_LOCAL_SIZE, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s2_nlranks;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_APPNUM, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s2_appnum;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_LOCAL_RANK, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s2_lrank;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_NODE_RANK, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = s2_nrank;
        *kv = kp;
        return true;
    }

    return false;
}

static char* pmix_error(int pmix_err)
{
    char * err_msg;

    switch(pmix_err) {
        case PMI2_FAIL: err_msg = "Operation failed"; break;
        case PMI2_ERR_INIT: err_msg = "PMI is not initialized"; break;
        case PMI2_ERR_NOMEM: err_msg = "Input buffer not large enough"; break;
        case PMI2_ERR_INVALID_ARG: err_msg = "Invalid argument"; break;
        case PMI2_ERR_INVALID_KEY: err_msg = "Invalid key argument"; break;
        case PMI2_ERR_INVALID_KEY_LENGTH: err_msg = "Invalid key length argument"; break;
        case PMI2_ERR_INVALID_VAL: err_msg = "Invalid value argument"; break;
        case PMI2_ERR_INVALID_VAL_LENGTH: err_msg = "Invalid value length argument"; break;
        case PMI2_ERR_INVALID_LENGTH: err_msg = "Invalid length argument"; break;
        case PMI2_ERR_INVALID_NUM_ARGS: err_msg = "Invalid number of arguments"; break;
        case PMI2_ERR_INVALID_ARGS: err_msg = "Invalid args argument"; break;
        case PMI2_ERR_INVALID_NUM_PARSED: err_msg = "Invalid num_parsed length argument"; break;
        case PMI2_ERR_INVALID_KEYVALP: err_msg = "Invalid keyvalp argument"; break;
        case PMI2_ERR_INVALID_SIZE: err_msg = "Invalid size argument"; break;
        case PMI2_SUCCESS: err_msg = "Success"; break;
        default: err_msg = "Unkown error";
    }
    return err_msg;
}
