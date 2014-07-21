/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc.  All rights reserved.
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
#include <pmi2.h>

#include "pmix_s2.h"

static int s2_init(void);
static int s2_fini(void);
static bool s2_initialized(void);
static int s2_abort(int flag, const char msg[]);
static int s2_get_appnum(int *appnum);
static int s2_spawn(int count, const char * cmds[],
        int argcs[], const char ** argvs[],
        const int maxprocs[],
        opal_list_t *info_keyval_vector,
        opal_list_t *preput_keyval_vector,
        char jobId[], int jobIdSize,
        int errors[]);
static int s2_get_jobid(char jobId[], int jobIdSize);
static int s2_get_rank(int *rank);
static int s2_get_size(opal_pmix_scope_t scope, int *size);
static int s2_put(opal_pmix_scope_t scope,
                  opal_value_t *kv);
static int s2_fence(void);
static int s2_fence_nb(opal_pmix_cbfunc_t cbfunc, void *cbdata);
static int s2_get(opal_identifier_t *id,
        const char *key,
        opal_value_t *kv);
static void s2_get_nb(opal_identifier_t *id,
                      const char *key,
                      opal_pmix_cbfunc_t cbfunc,
                      void *cbdata);
static int s2_publish(const char service_name[],
                      opal_list_t *info,
                      const char port[]);
static int s2_lookup(const char service_name[],
                     opal_list_t *info,
                     char port[], int portLen);
static int s2_unpublish(const char service_name[], 
                        opal_list_t *info);
static int s2_local_info(int vpid, int **ranks_ret,
                         int *procs_ret, char **error);
static int s2_job_connect(const char jobId[]);
static int s2_job_disconnect(const char jobId[]);
static int s2_get_node_attr(const char name[],
                            char value[],
                            int valuelen,
                            int *found,
                            int waitfor);
static int s2_get_node_attr_array(const char name[],
                                  int array[],
                                  int arraylen,
                                  int *outlen,
                                  int *found);
static int s2_put_node_attr(const char name[], const char value[]);
static int s2_get_job_attr(const char name[],
                           char value[],
                           int valuelen,
                           int *found);
static int s2_get_job_attr_array(const char name[],
                                 int array[],
                                 int arraylen,
                                 int *outlen,
                                 int *found);

const opal_pmix_base_module_t opal_pmix_s2_module = {
    s2_init,
    s2_fini,
    s2_initialized,
    s2_abort,
    s2_get_jobid,
    s2_get_rank,
    s2_get_size,
    s2_get_appnum,
    s2_fence,
    s2_fence_nb,
    s2_put,
    s2_get,
    s2_get_nb,
    s2_publish,
    s2_lookup,
    s2_unpublish,
    s2_local_info,
    s2_spawn,
    s2_job_connect,
    s2_job_disconnect
};

// usage accounting
static int pmix_init_count = 0;

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

static int s2_init(void)
{
    int spawned, size, rank, appnum;
    int rc, ret = OPAL_ERROR;
    char buf[16];
    int found;

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

    pmix_size = size;
    pmix_rank = rank;
    pmix_appnum = appnum;

    pmix_vallen_max = PMI2_MAX_VALLEN;
    pmix_kvslen_max = PMI2_MAX_VALLEN; // FIX ME: What to put here for versatility?
    pmix_keylen_max = PMI2_MAX_KEYLEN;

    rc = PMI2_Info_GetJobAttr("universeSize", buf, 16, &found);
    if( PMI2_SUCCESS != rc ) {
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
    if( PMI2_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI2_Job_GetId");
        goto err_exit;
    }

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
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s2_get_jobid(char jobId[], int jobIdSize)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s2_get_rank(int *rank)
{
    *rank = pmix_rank;
    return OPAL_SUCCESS;
}

static int s2_get_size(opal_pmix_scope_t scope, int *size)
{
    *size = pmix_size;
    return OPAL_SUCCESS;
}

static int s2_job_connect(const char jobId[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s2_job_disconnect(const char jobId[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s2_get_appnum(int *appnum)
{
    *appnum = pmix_appnum;
    return OPAL_SUCCESS;
}

static int s2_put(opal_pmix_scope_t scope,
                  opal_value_t *kv)
{
    int rc;
/*
    if( PMI2_SUCCESS != PMI2_KVS_Put(key, value) ){
        OPAL_PMI_ERROR(rc, "PMI2_KVS_Put");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;*/
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s2_fence(void)
{
    int rc;
    if (PMI2_SUCCESS != (rc = PMI2_KVS_Fence())) {
        OPAL_PMI_ERROR(rc, "PMI2_KVS_Fence");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int s2_fence_nb(opal_pmix_cbfunc_t cbfunc, void *cbdata)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s2_get(opal_identifier_t *id,
        const char *key,
        opal_value_t *kv)
{
/*    int rc;
    int len;
    rc = PMI2_KVS_Get(kvs_name, PMI2_ID_NULL, key, value, valuelen, &len);
    if( PMI2_SUCCESS != rc ){
        // OPAL_PMI2_ERROR(rc, "PMI_KVS_Get");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;*/
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static void s2_get_nb(opal_identifier_t *id,
                      const char *key,
                      opal_pmix_cbfunc_t cbfunc,
                      void *cbdata)
{
    return;
}

static int s2_get_node_attr(const char name[],
                            char value[],
                            int valuelen,
                            int *found,
                            int waitfor)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s2_get_node_attr_array(const char name[],
                                  int array[],
                                  int arraylen,
                                  int *outlen,
                                  int *found)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s2_put_node_attr(const char name[], const char value[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s2_get_job_attr(const char name[],
                           char value[],
                           int valuelen,
                           int *found)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s2_get_job_attr_array(const char name[],
                                 int array[],
                                 int arraylen,
                                 int *outlen,
                                 int *found)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s2_publish(const char service_name[],
                      opal_list_t *info,
                      const char port[])
{
/*    int rc;

    if (PMI2_SUCCESS != (rc = PMI2_Nameserv_publish(service_name, info_ptr, port))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_publish");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;*/
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s2_lookup(const char service_name[],
                     opal_list_t *info,
                     char port[], int portLen)
{
    /*int rc;

    if (PMI_SUCCESS != (rc = PMI2_Nameserv_lookup(service_name, info_ptr, port, portlen))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_lookup");
        free(port);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;*/
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s2_unpublish(const char service_name[], 
                        opal_list_t *info)
{
    /*int rc;

    if (PMI2_SUCCESS != (rc = PMI2_Nameserv_unpublish(service_name, info_ptr))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_unpublish");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;;*/
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int s2_local_info(int vpid, int **ranks_ret,
                         int *procs_ret, char **error)
{
    int *ranks;
    int procs = -1;
    int rc;

    char *pmapping = (char*)malloc(PMI2_MAX_VALLEN);
    if( pmapping == NULL ){
        *error = "mca_common_pmix_local_info: could not get memory for PMIv2 process mapping";
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    int found;
    int my_node;

    rc = PMI2_Info_GetJobAttr("PMI_process_mapping", pmapping, PMI2_MAX_VALLEN, &found);
    if( !found || PMI2_SUCCESS != rc ) {
        /* can't check PMI2_SUCCESS as some folks (i.e., Cray) don't define it */
        OPAL_PMI_ERROR(rc,"PMI2_Info_GetJobAttr");
        *error = "mca_common_pmix_local_info: could not get PMI_process_mapping";
        return OPAL_ERROR;
    }

    ranks = mca_common_pmix2_parse_pmap(pmapping, vpid, &my_node, &procs);
    if (NULL == ranks) {
        *error = "mca_common_pmix_local_info: could not get memory for PMIv2 local ranks";
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    free(pmapping);
    *ranks_ret = ranks;
    *procs_ret = procs;
    return OPAL_SUCCESS;
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
