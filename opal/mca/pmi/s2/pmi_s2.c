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

#if !defined(PMI2_SUCCESS)
#define PMI2_SUCCESS PMI_SUCCESS
#endif

#include "pmi_s2.h"

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


static int mca_initialize_pmi_v2(void)
{
    int spawned, size, rank, appnum;
    int rc, ret = OPAL_ERROR;

    /* deal with a Slurm bug by first checking if we were
     * even launched by a PMI server before attempting
     * to use PMI */
    if (NULL == getenv("PMI_FD")) {
        return OPAL_ERROR;
    }

    /* if we can't startup PMI, we can't be used */
    if ( PMI2_Initialized () ) {
        return OPAL_SUCCESS;
    }
    size = -1;
    rank = -1;
    appnum = -1;
    if (PMI2_SUCCESS != (rc = PMI2_Init(&spawned, &size, &rank, &appnum))) {
        opal_show_help("help-common-pmi.txt", "pmi2-init-failed", true, rc);
        return OPAL_ERROR;
    }
    if( size < 0 || rank < 0 ){
        opal_output(0, "SIZE %d RANK %d", size, rank);
        opal_show_help("help-common-pmi.txt", "pmi2-init-returned-bad-values", true);
        goto err_exit;
    }


    pmi_size = size;
    pmi_rank = rank;
    pmi_appnum = appnum;

    pmi_vallen_max = PMI2_MAX_VALLEN;
    pmi_kvslen_max = PMI2_MAX_VALLEN; // FIX ME: What to put here for versatility?
    pmi_keylen_max = PMI2_MAX_KEYLEN;


    char buf[16];
    int found;
    
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
void mca_common_pmi_finalize (void) {
    if (0 == mca_common_pmi_init_count) {
        return;
    }

    if (0 == --mca_common_pmi_init_count) {
        PMI2_Finalize();
    }
}

int mca_common_pmi_rank()
{
    return pmi_rank;
}


int mca_common_pmi_size()
{
    return pmi_size;
}

int mca_common_pmi_appnum()
{
    return pmi_appnum;
}


int mca_common_pmi_universe()
{
    return pmi_usize;
}

int mca_common_pmi_kvslen() {
    return pmi_kvslen_max;
}

int mca_common_pmi_keylen()
{
    return pmi_keylen_max;
}

int mca_common_pmi_vallen()
{
    return pmi_vallen_max;
}

int mca_common_pmi_kvsname(char *buf, int len)
{
    int i;
    if( (unsigned)len < strnlen(pmi_kvs_name,pmi_kvslen_max) ){
        return OPAL_ERR_BAD_PARAM;
    }
    for(i = 0; pmi_kvs_name[i]; i++){
        buf[i] = pmi_kvs_name[i];
    }
    buf[i] = '\0';
    return OPAL_SUCCESS;
}

int mca_common_pmi_id(char **pmi_id_ret, char **error){
    char *pmi_id = NULL;
    int rc;

    // Default values
    *pmi_id_ret = pmi_id;
    *error = NULL;

    pmi_id = (char*)malloc(PMI2_MAX_VALLEN);
    if( pmi_id == NULL ){
        *error = "mca_common_pmi_id: could not get memory for PMIv2 ID";
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    strncpy(pmi_id, pmi_kvs_name, pmi_kvslen_max);

    *pmi_id_ret = pmi_id;
    return OPAL_SUCCESS;
}

int mca_common_pmi_local_info(int vpid, int **ranks_ret,
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

void mca_common_pmi_abort(int status, char *msg)
{
    PMI2_Abort(status, msg);
}

int rc;

int mca_common_pmi_publish(const char *service_name, const char *port_name)
{
    if (PMI2_SUCCESS != (rc = PMI2_Nameserv_publish(service_name, NULL, port_name))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_publish");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

int mca_common_pmi_lookup(const char *service_name, char **port_ret)
{
    // FIXME:
    // 1. Why don't we malloc memory for the port for PMI v1?
    // 2. Maybe error handling is needed in pbusub?
    // 3. Is it legal to call OPAL_PMI_ERROR for PMIv2 rc?

    char *port = NULL;
    *port_ret = port;
    int rc;

    port = (char*)malloc(1024*sizeof(char));  /* arbitrary size */
    if( port == NULL ){
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    if (PMI_SUCCESS != (rc = PMI2_Nameserv_lookup(service_name, NULL, port, 1024))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_lookup");
        free(port);
        return OPAL_ERROR;
    }

    *port_ret = port;
    return OPAL_SUCCESS;
}

int mca_common_pmi_unpublish ( const char *service_name )
{
    int rc;

    if (PMI2_SUCCESS != (rc = PMI2_Nameserv_unpublish(service_name, NULL))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_unpublish");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;;
}

int mca_common_pmi_barrier()
{
    /* PMI2 doesn't provide a barrier, so use the Fence function here */
    if (PMI2_SUCCESS != (rc = PMI2_KVS_Fence())) {
        // FIX ME: OPAL_PMI2_ERROR(rc, "PMI2_KVS_Fence");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

int mca_common_pmi_put(const char *kvs_name,
                       const char *key, const char *value)
{
    int rc;

    if( PMI2_SUCCESS != PMI2_KVS_Put(key, value) ){
        // FIXME: OPAL_PMI2_ERROR(rc, "PMI2_KVS_Put");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

int mca_common_pmi_get(const char *kvs_name, const char *key,
                       char *value, int valuelen)
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

int mca_common_pmi_commit(char *kvs_name)
{
    return mca_common_pmi_barrier();
}

