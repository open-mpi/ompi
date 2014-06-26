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

#include "pmi_s2.h"

static int s2_init(void);
static int s2_fini(void);
static bool s2_initialized(void);
static int s2_abort(int flag, const char msg[]);
static int s2_spawn(int count, const char * cmds[],
                    int argcs[], const char ** argvs[],
                    const int maxprocs[],
                    const int info_keyval_sizes[],
                    const struct MPID_Info *info_keyval_vectors[],
                    int preput_keyval_size,
                    const struct MPID_Info *preput_keyval_vector[],
                    char jobId[], int jobIdSize,
                    int errors[]);
static int s2_get_jobid(char jobId[], int jobIdSize);
static int s2_get_rank(int *rank);
static int s2_get_size(int *size);
static int s2_job_connect(const char jobId[],
                          PMI2_Connect_comm_t *conn);
static int s2_job_disconnect(const char jobId[]);
static int s2_put(const char key[], const char value[]);
static int s2_fence(void);
static int s2_get(const char *jobid,
                  int src_pmi_id,
                  const char key[],
                  char value [],
                  int maxvalue,
                  int *vallen);
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
static int s2_publish(const char service_name[],
                      const struct MPID_Info *info_ptr,
                      const char port[]);
static int s2_lookup(const char service_name[],
                     const struct MPID_Info *info_ptr,
                     char port[], int portLen);
static int s2_unpublish(const char service_name[], 
                        const struct MPID_Info *info_ptr);

opal_pmi_base_module_t opal_pmi_s2_module = {
    s2_init,
    s2_fini,
    s2_initialized,
    s2_abort,
    s2_spawn,
    s2_get_jobid,
    s2_get_rank,
    s2_get_size,
    s2_job_connect,
    s2_job_disconnect,
    s2_put,
    s2_fence,
    s2_get,
    s2_get_node_attr,
    s2_get_node_attr_array,
    s2_put_node_attr,
    s2_get_job_attr,
    s2_get_job_attr_array,
    s2_publish,
    s2_lookup,
    s2_unpublish
};

// usage accounting
static int pmi_init_count = 0;

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
        opal_show_help("help-pmi-base.txt", "pmi2-init-failed", true, rc);
        return OPAL_ERROR;
    }
    if( size < 0 || rank < 0 ){
        opal_show_help("help-pmi-base.txt", "pmi2-init-returned-bad-values", true);
        goto err_exit;
    }

    pmi_size = size;
    pmi_rank = rank;
    pmi_appnum = appnum;

    pmi_vallen_max = PMI2_MAX_VALLEN;
    pmi_kvslen_max = PMI2_MAX_VALLEN; // FIX ME: What to put here for versatility?
    pmi_keylen_max = PMI2_MAX_KEYLEN;

    rc = PMI2_Info_GetJobAttr("universeSize", buf, 16, &found);
    if( PMI2_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI_Get_universe_size");
        goto err_exit;
    }
    pmi_usize = atoi(buf);

    pmi_kvs_name = (char*)malloc(pmi_kvslen_max);
    if( pmi_kvs_name == NULL ){
         PMI2_Finalize();
         ret = OPAL_ERR_OUT_OF_RESOURCE;
         goto err_exit;
    }
    rc = PMI2_Job_GetId(pmi_kvs_name, pmi_kvslen_max);
    if( PMI2_SUCCESS != rc ) {
        OPAL_PMI_ERROR(rc, "PMI2_Job_GetId");
        goto err_exit;
    }

    return OPAL_SUCCESS;
err_exit:
    PMI2_Finalize();
    return ret;
}

static void s2_fini(void) {
    if (0 == pmi_init_count) {
        return;
    }

    if (0 == --pmi_init_count) {
        PMI2_Finalize();
    }
}

static bool s2_initialized(void)
{
    if (0 < pmi_init_count) {
        return true;
    }
    return false;
}

static void s2_abort(int status, char *msg)
{
    PMI2_Abort(status, msg);
}

static int s2_spawn(int count, const char * cmds[],
                    int argcs[], const char ** argvs[],
                    const int maxprocs[],
                    const int info_keyval_sizes[],
                    const struct MPID_Info *info_keyval_vectors[],
                    int preput_keyval_size,
                    const struct MPID_Info *preput_keyval_vector[],
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
    *rank = pmi_rank;
    return OPAL_SUCCESS;
}

static int s2_get_size(int *size)
{
    *size = pmi_size;
    return OPAL_SUCCESS;
}

static int s2_put(const char key[], const char value[])

    int rc;

    if( PMI2_SUCCESS != PMI2_KVS_Put(key, value) ){
        OPAL_PMI_ERROR(rc, "PMI2_KVS_Put");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int s2_fence(void)
{
    if (PMI2_SUCCESS != (rc = PMI2_KVS_Fence())) {
        OPAL_PMI_ERROR(rc, "PMI2_KVS_Fence");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int s2_get(const char *jobid,
                  int src_pmi_id,
                  const char key[],
                  char value [],
                  int maxvalue,
                  int *vallen)
{
    int rc;
    int len;
    rc = PMI2_KVS_Get(kvs_name, PMI2_ID_NULL, key, value, valuelen, &len);
    if( PMI2_SUCCESS != rc ){
        // OPAL_PMI2_ERROR(rc, "PMI_KVS_Put");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
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
                      const struct MPID_Info *info_ptr,
                      const char port[])
{
    int rc;

    if (PMI2_SUCCESS != (rc = PMI2_Nameserv_publish(service_name, info_ptr, port))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_publish");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int s2_lookup(const char service_name[],
                     const struct MPID_Info *info_ptr,
                     char port[], int portLen)
{
    int rc;

    if (PMI_SUCCESS != (rc = PMI2_Nameserv_lookup(service_name, info_ptr, port, portlen))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_lookup");
        free(port);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int s2_unpublish(const char service_name[], 
                        const struct MPID_Info *info_ptr)
{
    int rc;

    if (PMI2_SUCCESS != (rc = PMI2_Nameserv_unpublish(service_name, info_ptr))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_unpublish");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;;
}

static int s2_local_info(int vpid, int **ranks_ret,
                         int *procs_ret, char **error)
{
    int *ranks;
    int procs = -1;
    int rc;

    char *pmapping = (char*)malloc(PMI2_MAX_VALLEN);
    if( pmapping == NULL ){
        *error = "mca_common_pmi_local_info: could not get memory for PMIv2 process mapping";
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    int found;
    int my_node;

    rc = PMI2_Info_GetJobAttr("PMI_process_mapping", pmapping, PMI2_MAX_VALLEN, &found);
    if( !found || PMI2_SUCCESS != rc ) {
        /* can't check PMI2_SUCCESS as some folks (i.e., Cray) don't define it */
        OPAL_PMI_ERROR(rc,"PMI2_Info_GetJobAttr");
        *error = "mca_common_pmi_local_info: could not get PMI_process_mapping";
        return OPAL_ERROR;
    }

    ranks = mca_common_pmi2_parse_pmap(pmapping, vpid, &my_node, &procs);
    if (NULL == ranks) {
        *error = "mca_common_pmi_local_info: could not get memory for PMIv2 local ranks";
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    free(pmapping);
    *ranks_ret = ranks;
    *procs_ret = procs;
    return OPAL_SUCCESS;
}

