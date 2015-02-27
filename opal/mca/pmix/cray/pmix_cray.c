/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All
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
#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

#include <string.h>
#include <pmi.h>
#include <pmi2.h>

#include "opal/mca/pmix/base/base.h"
#include "pmix_cray.h"

static int cray_init(void);
static int cray_fini(void);
static bool cray_initialized(void);
static int cray_abort(int flag, const char msg[]);
static int cray_spawn(int count, const char * cmds[],
        int argcs[], const char ** argvs[],
        const int maxprocs[],
        opal_list_t *info_keyval_vector,
        opal_list_t *preput_keyval_vector,
        char jobId[], int jobIdSize,
        int errors[]);
static int cray_job_connect(const char jobId[]);
static int cray_job_disconnect(const char jobId[]);
static int cray_put(opal_pmix_scope_t scope, opal_value_t *kv);
static int cray_fence(opal_process_name_t *procs, size_t nprocs);
static int cray_get(const opal_process_name_t *id,
                    const char *key,
                    opal_value_t **kv);
static int cray_publish(const char service_name[],
                      opal_list_t *info,
                      const char port[]);
static int cray_lookup(const char service_name[],
                       opal_list_t *info,
                       char port[], int portLen);
static int cray_unpublish(const char service_name[],
                          opal_list_t *info);
static bool cray_get_attr(const char *attr, opal_value_t **kv);

const opal_pmix_base_module_t opal_pmix_cray_module = {
    cray_init,
    cray_fini,
    cray_initialized,
    cray_abort,
    cray_fence,
    NULL,
    cray_put,
    cray_get,
    NULL,
    cray_publish,
    cray_lookup,
    cray_unpublish,
    cray_get_attr,
    NULL,
    cray_spawn,
    cray_job_connect,
    cray_job_disconnect,
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
static int pmix_size = 0;
static int pmix_rank = 0;
static int pmix_lrank = 0;
static int pmix_nrank = 0;
static int pmix_nlranks = 0;
static int pmix_appnum = 0;
static int pmix_usize = 0;
static char *pmix_kvs_name = NULL;
static int *pmix_lranks = NULL;
static opal_process_name_t pmix_pname;
static uint32_t pmix_jobid = -1;


static char* pmix_error(int pmix_err);
#define OPAL_PMI_ERROR(pmi_err, pmi_func)                       \
    do {                                                        \
        opal_output(0, "%s [%s:%d:%s]: %s\n",                   \
                    pmi_func, __FILE__, __LINE__, __func__,     \
                    pmix_error(pmi_err));                       \
    } while(0);

static int cray_init(void)
{
    int i, spawned, size, rank, appnum, my_node;
    int rc, ret = OPAL_ERROR;
    char *pmapping = NULL;
    char buf[PMI2_MAX_ATTRVALUE];
    int found;
    uint32_t jobfam;

    ++pmix_init_count;

    /* if we can't startup PMI, we can't be used */
    if ( PMI2_Initialized () ) {
        return OPAL_SUCCESS;
    }
    size = -1;
    rank = -1;
    appnum = -1;
    if (PMI_SUCCESS != (rc = PMI2_Init(&spawned, &size, &rank, &appnum))) {
        opal_show_help("help-pmix-base.txt", "pmix2-init-failed", true, rc);
        return OPAL_ERROR;
    }
    if( size < 0 || rank < 0 ){
        opal_show_help("help-pmix-base.txt", "pmix2-init-returned-bad-values", true);
        goto err_exit;
    }

    pmix_size = size;
    pmix_rank = rank;
    pmix_appnum = appnum;

    pmix_vallen_max = PMI2_MAX_VALLEN;
    pmix_kvslen_max = PMI2_MAX_VALLEN; // FIX ME: What to put here for versatility?
    pmix_keylen_max = PMI2_MAX_KEYLEN;
    pmix_vallen_threshold = PMI2_MAX_VALLEN * 3;
    pmix_vallen_threshold >>= 2;

    rc = PMI2_Info_GetJobAttr("universeSize", buf, 16, &found);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_Get_universe_size");
        goto err_exit;
    }
    pmix_usize = atoi(buf);

    pmix_kvs_name = (char*)malloc(pmix_kvslen_max);
    if( pmix_kvs_name == NULL ){
         PMI2_Finalize();
         ret = OPAL_ERR_OUT_OF_RESOURCE;
         goto err_exit;
    }
    rc = PMI2_Job_GetId(pmix_kvs_name, pmix_kvslen_max);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI2_Job_GetId");
        goto err_exit;
    }

    rc = sscanf(pmix_kvs_name,"kvs_%u",&jobfam);
    if (rc != 1) {
        OPAL_PMI_ERROR(rc, "PMI2_Job_GetId");
        rc = OPAL_ERROR;
        goto err_exit;
    }

    pmix_jobid = jobfam << 16;

    /* store our name in the opal_proc_t so that
     * debug messages will make sense - an upper
     * layer will eventually overwrite it, but that
     * won't do any harm */
    pmix_pname.jobid = pmix_jobid;
    pmix_pname.vpid = pmix_rank;
    opal_proc_set_name(&pmix_pname);
    opal_output_verbose(10, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray: assigned tmp name %d %d pmix_kvs_name %s",
                        OPAL_NAME_PRINT(pmix_pname),pmix_pname.jobid,pmix_pname.vpid,pmix_kvs_name);

    pmapping = (char*)malloc(PMI2_MAX_VALLEN);
    if( pmapping == NULL ){
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    rc = PMI2_Info_GetJobAttr("PMI_process_mapping", pmapping, PMI2_MAX_VALLEN, &found);
    if( !found || PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc,"PMI2_Info_GetJobAttr");
        return OPAL_ERROR;
    }

    pmix_lranks = pmix_cray_parse_pmap(pmapping, pmix_rank, &my_node, &pmix_nlranks);
    if (NULL == pmix_lranks) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    free(pmapping);

    /* find ourselves */
    for (i=0; i < pmix_nlranks; i++) {
        if (pmix_rank == pmix_lranks[i]) {
            pmix_lrank = i;
            pmix_nrank = i;
            break;
        }
    }

    return OPAL_SUCCESS;
err_exit:
    PMI2_Finalize();
    return ret;
}

static int cray_fini(void) {

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

    if (NULL != pmix_lranks) {
        free(pmix_lranks);
        pmix_lranks = NULL;
    }

    return OPAL_SUCCESS;
}

static bool cray_initialized(void)
{
    if (0 < pmix_init_count) {
        return true;
    }
    return false;
}

static int cray_abort(int status, const char *msg)
{
    return PMI2_Abort(status, msg);
}

static int cray_spawn(int count, const char * cmds[],
        int argcs[], const char ** argvs[],
        const int maxprocs[],
        opal_list_t *info_keyval_vector,
        opal_list_t *preput_keyval_vector,
        char jobId[], int jobIdSize,
        int errors[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int cray_job_connect(const char jobId[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int cray_job_disconnect(const char jobId[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int cray_put(opal_pmix_scope_t scope,
                  opal_value_t *kv)
{
    int rc;

    opal_output_verbose(10, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray cray_put key %s scope %d\n",
                         OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), kv->key, scope);
    /*
     * for now just always just global cache
     */

    if (NULL == mca_pmix_cray_component.cache_global) {
        mca_pmix_cray_component.cache_global = OBJ_NEW(opal_buffer_t);
    }

    opal_output_verbose(20, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray put global data for key %s type %d",
                         OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), kv->key, kv->type);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(mca_pmix_cray_component.cache_global, &kv, 1, OPAL_VALUE))) {
        OPAL_PMI_ERROR(rc,"pmix:cray opal_dss.pack returned error");
        OPAL_ERROR_LOG(rc);
    }

    return rc;
}

static int cray_fence(opal_process_name_t *procs, size_t nprocs)
{
    int rc, cnt;
    int32_t i;
    int *all_lens = NULL;
    opal_value_t *kp;
    opal_buffer_t *send_buffer = NULL;
    opal_buffer_t *buf = NULL;
    void *sbuf_ptr;
    char *cptr, *rcv_buff = NULL;
    opal_process_name_t id;
    typedef struct {
        uint32_t pmix_rank;
        opal_process_name_t name;
        int32_t nbytes;
    } bytes_and_rank_t;
    int32_t rcv_nbytes_tot;
    bytes_and_rank_t s_bytes_and_rank;
    bytes_and_rank_t *r_bytes_and_ranks = NULL;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray executing fence on %u procs cache_global %p cache_local %p",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), (unsigned int)nprocs,
                        (void *)mca_pmix_cray_component.cache_global,
                        (void *)mca_pmix_cray_component.cache_local);

    /*
     * "unload" the cache_local/cache_global buffers, first copy
     * it so we can continue to use the local buffers if further
     * calls to put can be made
     */

    send_buffer = OBJ_NEW(opal_buffer_t);
    if (NULL == send_buffer) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    opal_dss.copy_payload(send_buffer, mca_pmix_cray_component.cache_global);
    opal_dss.unload(send_buffer, &sbuf_ptr, &s_bytes_and_rank.nbytes);
    s_bytes_and_rank.pmix_rank = pmix_rank;
    s_bytes_and_rank.name = OPAL_PROC_MY_NAME;

    r_bytes_and_ranks = (bytes_and_rank_t *)malloc(pmix_size * sizeof(bytes_and_rank_t));
    if (NULL == r_bytes_and_ranks) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto fn_exit;
    }

    /*
     * gather up all the buffer sizes and rank order.
     * doing this step below since the cray pmi PMI_Allgather doesn't deliver
     * the gathered data necessarily in PMI rank order, although the order stays
     * the same for the duration of a job - assuming no node failures.
     */

    if (PMI_SUCCESS != (rc = PMI_Allgather(&s_bytes_and_rank,r_bytes_and_ranks,sizeof(bytes_and_rank_t)))) {
        OPAL_PMI_ERROR(rc,"PMI_Allgather");
        rc = OPAL_ERR_COMM_FAILURE;
        goto fn_exit;
    }


    for (rcv_nbytes_tot=0,i=0; i < pmix_size; i++) {
        rcv_nbytes_tot += r_bytes_and_ranks[i].nbytes;
    }

    opal_output_verbose(20, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray total number of bytes to receive %d",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), rcv_nbytes_tot);

    rcv_buff = (char *) malloc(rcv_nbytes_tot * sizeof(char));
    if (NULL == rcv_buff) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto fn_exit;
    }

    all_lens = (int *)malloc(sizeof(int) * pmix_size);
    if (NULL == all_lens) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto fn_exit;
    }
    for (i=0; i< pmix_size; i++) {
        all_lens[r_bytes_and_ranks[i].pmix_rank] = r_bytes_and_ranks[i].nbytes;
    }

    if (PMI_SUCCESS != (rc = PMI_Allgatherv(sbuf_ptr,s_bytes_and_rank.nbytes,rcv_buff,all_lens))) {
        OPAL_PMI_ERROR(rc,"PMI_Allgatherv");
        rc = OPAL_ERR_COMM_FAILURE;
        goto fn_exit;
    }

    OBJ_RELEASE(send_buffer);
    send_buffer  = NULL;

    buf = OBJ_NEW(opal_buffer_t);
    if (buf == NULL) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto fn_exit;
    }

    for (cptr = rcv_buff, i=0; i < pmix_size; i++) {

        id = r_bytes_and_ranks[i].name;

        buf->base_ptr = NULL;  /* TODO: ugh */
        if (OPAL_SUCCESS != (rc = opal_dss.load(buf, (void *)cptr, r_bytes_and_ranks[i].nbytes))) {
            OPAL_PMI_ERROR(rc,"pmix:cray opal_dss.load failed");
            goto fn_exit;
        }

        /* unpack and stuff in to the dstore */

        cnt = 1;
        while (OPAL_SUCCESS == (rc = opal_dss.unpack(buf, &kp, &cnt, OPAL_VALUE))) {
            opal_output_verbose(20, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray unpacked kp with key %s type(%d) for id  %s", 
                         OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), kp->key, kp->type, OPAL_NAME_PRINT(id));
            if (OPAL_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal, 
                                                         &id, kp))) {
                OPAL_ERROR_LOG(rc);
                goto fn_exit;
            }
             OBJ_RELEASE(kp);
             cnt = 1;
        }

        cptr += r_bytes_and_ranks[i].nbytes;

    }

    buf->base_ptr = NULL;  /* TODO: ugh */
    OBJ_RELEASE(buf);

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray kvs_fence complete",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
fn_exit:
    if (all_lens != NULL) free(all_lens);
    if (rcv_buff != NULL) free(rcv_buff);
    if (r_bytes_and_ranks != NULL) free(r_bytes_and_ranks);
    return rc;
}

static int cray_get(const opal_process_name_t *id, const char *key, opal_value_t **kv)
{
    int rc;
    opal_list_t vals;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:cray getting value for proc %s key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        OPAL_NAME_PRINT(*id), key);

    OBJ_CONSTRUCT(&vals, opal_list_t);
    rc = opal_dstore.fetch(opal_dstore_internal, id, key, &vals);
    if (OPAL_SUCCESS == rc) {
        *kv = (opal_value_t*)opal_list_remove_first(&vals);
        return OPAL_SUCCESS;
    } else {
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                "%s pmix:cray fetch from dstore failed: %d",
                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), rc);
    }
    OPAL_LIST_DESTRUCT(&vals);

    return rc;
}

static int cray_publish(const char service_name[],
                        opal_list_t *info,
                        const char port[])
{
    int rc;

    if (PMI_SUCCESS != (rc = PMI2_Nameserv_publish(service_name, NULL, port))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_publish");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int cray_lookup(const char service_name[],
                       opal_list_t *info,
                       char port[], int portLen)
{
    int rc;

    if (PMI_SUCCESS != (rc = PMI2_Nameserv_lookup(service_name, NULL, port, portLen))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_lookup");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int cray_unpublish(const char service_name[],
                          opal_list_t *info)
{
    int rc;

    if (PMI_SUCCESS != (rc = PMI2_Nameserv_unpublish(service_name, NULL))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_unpublish");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;;
}

static bool cray_get_attr(const char *attr, opal_value_t **kv)
{
    int rc, i;
    opal_value_t *kp;

    if (0 == strcmp(PMIX_JOBID, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = pmix_jobid;
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

    if (0 == strcmp(PMIX_LOCAL_RANK, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = pmix_lrank;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_NODE_RANK, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = pmix_nrank;
        *kv = kp;
        return true;
    }

    if (0 == strcmp(PMIX_LOCAL_SIZE, attr)) {
        kp = OBJ_NEW(opal_value_t);
        kp->key = strdup(attr);
        kp->type = OPAL_UINT32;
        kp->data.uint32 = pmix_nlranks;
        *kv = kp;
        return true;
    }

    return OPAL_ERR_NOT_IMPLEMENTED;
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
	/* pmi.h calls this a valid return code but mpich doesn't define it (slurm does). */
        case PMI_ERR_INVALID_KVS: err_msg = "Invalid kvs argument"; break;
#endif
        case PMI_SUCCESS: err_msg = "Success"; break;
        default: err_msg = "Unkown error";
    }
    return err_msg;
}
