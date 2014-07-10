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

#include "opal/util/output.h"
#include "opal/util/show_help.h"

#include <string.h>
#include <pmi.h>

#include "pmix_s1.h"

static int s1_init(void);
static int s1_fini(void);
static bool s1_initialized(void);
static int s1_abort(int flag, const char msg[]);
static int s1_get_jobid(char jobId[], int jobIdSize);
static int s1_get_rank(int *rank);
static int s1_get_size(opal_pmix_scope_t scope, int *size);
static int s1_get_appnum(int *appnum);
static int s1_fence(void);
static int s1_fence_nb(opal_pmix_cbfunc_t cbfunc, void *cbdata);
static int s1_put(opal_identifier_t *id,
                  const char key[],
                  opal_pmix_scope_t scope,
                  opal_value_t *kv);
static int s1_get(opal_identifier_t *id,
                  const char *key,
                  opal_value_t *kv);
static void s1_get_nb(opal_identifier_t *id,
                      const char *key,
                      opal_pmix_cbfunc_t cbfunc,
                      void *cbdata);
static int s1_publish(opal_identifier_t *id,
        const char service_name[],
        opal_list_t *info,
        const char port[]);
static int s1_lookup(opal_identifier_t *id,
        const char service_name[],
        opal_list_t *info,
        char port[], int portLen);
static int s1_unpublish(opal_identifier_t *id,
        const char service_name[], 
        opal_list_t *info);
static int s1_local_info(int vpid, int **ranks_ret,
                         int *procs_ret, char **error);
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
    s1_get_jobid,
    s1_get_rank,
    s1_get_size,
    s1_get_appnum,
    s1_fence,
    s1_fence_nb,
    s1_put,
    s1_get,
    s1_get_nb,
    s1_publish,
    s1_lookup,
    s1_unpublish,
    s1_local_info,
    s1_spawn,
    s1_job_connect,
    s1_job_disconnect
};

// usage accounting
static int pmix_init_count = 0;

// per-launch selection between PMI versions
static int pmix_version = 0;

// PMI constant values:
static int pmix_kvslen_max = 0;
static int pmix_keylen_max = 0;
static int pmix_vallen_max = 0;

// Job environment description
static int pmix_size = 0;
static int pmix_rank = 0;
static int pmix_appnum = 0;
static int pmix_usize = 0;
static char *pmix_kvs_name = NULL;

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
    rc = PMI_Get_rank(&pmix_rank);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_Get_rank");
        return OPAL_ERROR;
    }
    rc = PMI_Get_universe_size(&pmix_usize);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_Get_universe_size");
        goto err_exit;
    }

    rc = PMI_Get_size(&pmix_size);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_Get_size");
        goto err_exit;
    }

    rc = PMI_Get_appnum(&pmix_appnum);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_Get_appnum");
        goto err_exit;
    }

    pmix_kvs_name = (char*)malloc(pmix_kvslen_max);
    if( pmix_kvs_name == NULL ){
         ret = OPAL_ERR_OUT_OF_RESOURCE;
         goto err_exit;
    }

    rc = PMI_KVS_Get_my_name(pmix_kvs_name,pmix_kvslen_max);
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
    return OPAL_ERR_NOT_SUPPORTED;
}

static int s1_get_jobid(char jobId[], int jobIdSize)
{
    return OPAL_SUCCESS;
}

static int s1_get_rank(int *rank)
{
    *rank = pmix_rank;
    return OPAL_SUCCESS;
}

static int s1_get_size(opal_pmix_scope_t scope, int *size)
{
    *size = pmix_size;
    return OPAL_SUCCESS;
}

static int s1_put(opal_identifier_t *id,
                  const char key[],
                  opal_pmix_scope_t scope,
                  opal_value_t *kv)
{
    int rc;
    if (OPAL_SUCCESS != (rc = pmi_store_encoded (id, kv->key, (void*)&kv->data, kv->type))) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    /*rc = PMI_KVS_Put(pmix_kvs_name, key, value);
    if( PMI_SUCCESS != rc ){
        OPAL_PMI_ERROR(rc, "PMI_KVS_Put");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;*/
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s1_fence(void)
{
    /* if we haven't already done it, ensure we have committed our values */
    /*if (!s1_committed) {
        if (PMI_SUCCESS != (rc = PMI_KVS_Commit(pmix_kvs_name))) {
            OPAL_PMI_ERROR(rc, "PMI_KVS_Commit");
            return OPAL_ERROR;
        }
        s1_committed = true;
    }

    // use the PMI barrier function 
    if (PMI_SUCCESS != (rc = PMI_Barrier())) {
        OPAL_PMI_ERROR(rc, "PMI_Barrier");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;*/
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s1_fence_nb(opal_pmix_cbfunc_t cbfunc, void *cbdata)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

//static int s1_get(const char *jobid,
//                  int src_pmix_id,
//                  const char key[],
//                  char value [],
//                  int maxvalue,
//                  int *vallen)
static int s1_get(opal_identifier_t *id,
                  const char *key,
                  opal_value_t *kv)
{
    int rc;
/*
    rc = PMI_KVS_Get(pmix_kvs_name, key, value, vallen);
    if( PMI_SUCCESS != rc ){
        OPAL_PMI_ERROR(rc, "PMI_KVS_Put");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;*/
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static void s1_get_nb(opal_identifier_t *id,
                      const char *key,
                      opal_pmix_cbfunc_t cbfunc,
                      void *cbdata)
{
    return;
}

static int s1_get_node_attr(const char name[],
                            char value[],
                            int valuelen,
                            int *found,
                            int waitfor)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int s1_get_node_attr_array(const char name[],
                                  int array[],
                                  int arraylen,
                                  int *outlen,
                                  int *found)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int s1_put_node_attr(const char name[], const char value[])
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int s1_get_job_attr(const char name[],
                           char value[],
                           int valuelen,
                           int *found)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int s1_get_job_attr_array(const char name[],
                                 int array[],
                                 int arraylen,
                                 int *outlen,
                                 int *found)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int s1_publish(opal_identifier_t *id,
        const char service_name[],
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

static int s1_lookup(opal_identifier_t *id,
        const char service_name[],
        opal_list_t *info,
        char port[], int portLen)
{
    int rc;
/*
    *port_ret = port;
    // Allocate mem for port here? Otherwise we won't get success!
    // SLURM PMIv1 doesn't implement this function

    if (PMI_SUCCESS != (rc = PMI_Lookup_name(service_name, port))) {
        OPAL_PMI_ERROR(rc, "PMI_Lookup_name");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;*/
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s1_unpublish(opal_identifier_t *id,
        const char service_name[], 
        opal_list_t *info)
{
    int rc;

    if (PMI_SUCCESS != (rc = PMI_Unpublish_name(service_name))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_unpublish");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;;
}

static int s1_local_info(int vpid, int **ranks_ret,
                          int *procs_ret, char **error)
{
    int *ranks;
    int procs = -1;
    int rc;

    /* get our local proc info to find our local rank */
    if (PMI_SUCCESS != (rc = PMI_Get_clique_size(&procs))) {
        OPAL_PMI_ERROR(rc, "PMI_Get_clique_size");
        *error = "mca_common_pmix_local_info: could not get PMI clique size";
        return OPAL_ERROR;
    }
    /* now get the specific ranks */
    ranks = (int*)calloc(procs, sizeof(int));
    if (NULL == ranks) {
        *error = "mca_common_pmix_local_info: could not get memory for local ranks";
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    if (PMI_SUCCESS != (rc = PMI_Get_clique_ranks(ranks, procs))) {
        OPAL_PMI_ERROR(rc, "PMI_Get_clique_ranks");
        *error = "mca_common_pmix_local_info: could not get clique ranks";
        return OPAL_ERROR;
    }

    *ranks_ret = ranks;
    *procs_ret = procs;
    return OPAL_SUCCESS;
}

static int s1_job_connect(const char jobId[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s1_job_disconnect(const char jobId[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s1_get_appnum(int *appnum)
{
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
	/* pmix.h calls this a valid return code but mpich doesn't define it (slurm does). */
        case PMI_ERR_INVALID_KVS: err_msg = "Invalid kvs argument"; break;
#endif
        case PMI_SUCCESS: err_msg = "Success"; break;
        default: err_msg = "Unkown error";
    }
    return err_msg;
}
