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

#include "pmi2_pmap_parser.h"

#include <string.h>
#include <pmi.h>

#include "pmi_s1.h"

static int s1_init(void);
static int s1_fini(void);
static bool s1_initialized(void);
static int s1_abort(int flag, const char msg[]);
static int s1_spawn(int count, const char * cmds[],
                    int argcs[], const char ** argvs[],
                    const int maxprocs[],
                    const int info_keyval_sizes[],
                    const struct MPID_Info *info_keyval_vectors[],
                    int preput_keyval_size,
                    const struct MPID_Info *preput_keyval_vector[],
                    char jobId[], int jobIdSize,
                    int errors[]);
static int s1_get_jobid(char jobId[], int jobIdSize);
static int s1_get_rank(int *rank);
static int s1_get_size(int *size);
static int s1_job_connect(const char jobId[],
                          PMI2_Connect_comm_t *conn);
static int s1_job_disconnect(const char jobId[]);
static int s1_put(const char key[], const char value[]);
static int s1_fence(void);
static int s1_get(const char *jobid,
                  int src_pmi_id,
                  const char key[],
                  char value [],
                  int maxvalue,
                  int *vallen);
static int s1_get_node_attr(const char name[],
                            char value[],
                            int valuelen,
                            int *found,
                            int waitfor);
static int s1_get_node_attr_array(const char name[],
                                  int array[],
                                  int arraylen,
                                  int *outlen,
                                  int *found);
static int s1_put_node_attr(const char name[], const char value[]);
static int s1_get_job_attr(const char name[],
                           char value[],
                           int valuelen,
                           int *found);
static int s1_get_job_attr_array(const char name[],
                                 int array[],
                                 int arraylen,
                                 int *outlen,
                                 int *found);
static int s1_publish(const char service_name[],
                      const struct MPID_Info *info_ptr,
                      const char port[]);
static int s1_lookup(const char service_name[],
                     const struct MPID_Info *info_ptr,
                     char port[], int portLen);
static int s1_unpublish(const char service_name[], 
                        const struct MPID_Info *info_ptr);
static int s1_local_info(int vpid, int **ranks_ret,
                         int *procs_ret, char **error);

opal_pmi_base_module_t opal_pmi_s1_module = {
    s1_init,
    s1_fini,
    s1_initialized,
    s1_abort,
    s1_spawn,
    s1_get_jobid,
    s1_get_rank,
    s1_get_size,
    s1_job_connect,
    s1_job_disconnect,
    s1_put,
    s1_fence,
    s1_get,
    s1_get_node_attr,
    s1_get_node_attr_array,
    s1_put_node_attr,
    s1_get_job_attr,
    s1_get_job_attr_array,
    s1_publish,
    s1_lookup,
    s1_unpublish,
    s1_local_info
};

// usage accounting
static int mca_common_pmi_init_count = 0;

// per-launch selection between PMI versions
static int mca_common_pmi_version = 0;

// PMI constant values:
static int pmi_kvslen_max = 0;
static int pmi_keylen_max = 0;
static int pmi_vallen_max = 0;

// Job environment description
static int pmi_size = 0;
static int pmi_rank = 0;
static int pmi_appnum = 0;
static int pmi_usize = 0;
static char *pmi_kvs_name = NULL;

static char* pmi_error(int pmi_err);
#define OPAL_PMI_ERROR(a) pmi_error(a)

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
    rc = PMI_KVS_Get_value_length_max(&pmi_vallen_max);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_value_length_max");
        goto err_exit;
    }

    rc = PMI_KVS_Get_name_length_max(&pmi_kvslen_max);
    if (PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_name_length_max");
        goto err_exit;
    }

    rc = PMI_KVS_Get_key_length_max(&pmi_keylen_max);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_key_length_max");
        goto err_exit;
    }

    // Initialize job environment information
    rc = PMI_Get_rank(&pmi_rank);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_Get_rank");
        return OPAL_ERROR;
    }
    rc = PMI_Get_universe_size(&pmi_usize);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_Get_universe_size");
        goto err_exit;
    }

    rc = PMI_Get_size(&pmi_size);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_Get_size");
        goto err_exit;
    }

    rc = PMI_Get_appnum(&pmi_appnum);
    if( PMI_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_Get_appnum");
        goto err_exit;
    }

    pmi_kvs_name = (char*)malloc(pmi_kvslen_max);
    if( pmi_kvs_name == NULL ){
         ret = OPAL_ERR_OUT_OF_RESOURCE;
         goto err_exit;
    }

    rc = PMI_KVS_Get_my_name(pmi_kvs_name,pmi_kvslen_max);
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
    if (0 == pmi_init_count) {
        return OPAL_SUCCESS;
    }

    if (0 == --pmi_init_count) {
        PMI_Finalize ();
    }
    return OPAL_SUCCESS;
}

static bool s1_initialized(void)
{
    if (0 < pmi_init_count) {
        return true;
    }
    return false;
}

static void s1_abort(int status, char *msg)
{
    PMI2_Abort(status, msg);
}

static int s1_spawn(int count, const char * cmds[],
                    int argcs[], const char ** argvs[],
                    const int maxprocs[],
                    const int info_keyval_sizes[],
                    const struct MPID_Info *info_keyval_vectors[],
                    int preput_keyval_size,
                    const struct MPID_Info *preput_keyval_vector[],
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
    *rank = pmi_rank;
    return OPAL_SUCCESS;
}

static int s1_get_size(int *size)
{
    *size = pmi_size;
    return OPAL_SUCCESS;
}

static int s1_put(const char key[], const char value[])
{
    int rc;

    rc = PMI_KVS_Put(pmi_kvs_name, key, value);
    if( PMI_SUCCESS != rc ){
        OPAL_PMI_ERROR(rc, "PMI_KVS_Put");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int s1_fence(void)
{
    /* if we haven't already done it, ensure we have committed our values */
    if (!s1_committed) {
        if (PMI_SUCCESS != (rc = PMI_KVS_Commit(pmi_kvs_name))) {
            OPAL_PMI_ERROR(rc, "PMI_KVS_Commit");
            return OPAL_ERROR;
        }
        s1_committed = true;
    }

    /* use the PMI barrier function */
    if (PMI_SUCCESS != (rc = PMI_Barrier())) {
        OPAL_PMI_ERROR(rc, "PMI_Barrier");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int s1_get(const char *jobid,
                  int src_pmi_id,
                  const char key[],
                  char value [],
                  int maxvalue,
                  int *vallen)
{
    int rc;

    rc = PMI_KVS_Get(pmi_kvs_name, key, value, vallen);
    if( PMI_SUCCESS != rc ){
        OPAL_PMI_ERROR(rc, "PMI_KVS_Put");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
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

static int s1_publish(const char service_name[],
                      const struct MPID_Info *info_ptr,
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
                     const struct MPID_Info *info_ptr,
                     char port[], int portLen)
{
    int rc;

    *port_ret = port;
    // Allocate mem for port here? Otherwise we won't get success!
    // SLURM PMIv1 doesn't implement this function

    if (PMI_SUCCESS != (rc = PMI_Lookup_name(service_name, port))) {
        OPAL_PMI_ERROR(rc, "PMI_Lookup_name");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int s1_unpublish(const char service_name[], 
                        const struct MPID_Info *info_ptr)
{
    int rc;

    if (PMI_SUCCESS != (rc = PMI_Unpublish_name(service_name))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_unpublish");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;;
}

static int pmi_local_info(int vpid, int **ranks_ret,
                          int *procs_ret, char **error)
{
    int *ranks;
    int procs = -1;
    int rc;

    /* get our local proc info to find our local rank */
    if (PMI_SUCCESS != (rc = PMI_Get_clique_size(&procs))) {
        OPAL_PMI_ERROR(rc, "PMI_Get_clique_size");
        *error = "mca_common_pmi_local_info: could not get PMI clique size";
        return OPAL_ERROR;
    }
    /* now get the specific ranks */
    ranks = (int*)calloc(procs, sizeof(int));
    if (NULL == ranks) {
        *error = "mca_common_pmi_local_info: could not get memory for local ranks";
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    if (PMI_SUCCESS != (rc = PMI_Get_clique_ranks(ranks, procs))) {
        OPAL_PMI_ERROR(rc, "PMI_Get_clique_ranks");
        *error = "mca_common_pmi_local_info: could not get clique ranks";
        return OPAL_ERROR;
    }

    *ranks_ret = ranks;
    *procs_ret = procs;
    return OPAL_SUCCESS;
}

static char* pmi_error(int pmi_err)
{
    char * err_msg;

    switch(pmi_err) {
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
