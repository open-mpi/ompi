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


static int mca_initialize_pmi_v1(void)
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

void mca_common_pmi_finalize (void) {
    if (0 == mca_common_pmi_init_count) {
        return;
    }

    if (0 == --mca_common_pmi_init_count) {
        PMI_Finalize ();
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
    int pmi_maxlen;

    // Default values
    *pmi_id_ret = pmi_id;
    *error = NULL;

    /* get our PMI id length */
    if (PMI_SUCCESS != (rc = PMI_Get_id_length_max(&pmi_maxlen))) {
        *error = "PMI_Get_id_length_max";
        return OPAL_ERROR;
    }
    // TODO: add proper error handling
    pmi_id = (char*)malloc(pmi_maxlen);
    if( pmi_id == NULL ){
        *error = "mca_common_pmi_id: could not get memory for PMIv1 ID";
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    /* Get domain id */
    if (PMI_SUCCESS != (rc = PMI_Get_kvs_domain_id(pmi_id, pmi_maxlen))) {
        free(pmi_id);
        *error = "PMI_Get_kvs_domain_id";
        return OPAL_ERROR;
    }

    *pmi_id_ret = pmi_id;
    return OPAL_SUCCESS;
}

int mca_common_pmi_local_info(int vpid, int **ranks_ret,
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

void mca_common_pmi_abort(int status, char *msg)
{
    PMI_Abort(status, msg);
}

int rc;

int mca_common_pmi_publish(const char *service_name, const char *port_name)
{
    if (PMI_SUCCESS != (rc = PMI_Publish_name(service_name, port_name))) {
        OPAL_PMI_ERROR(rc, "PMI_Publish_name");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

int mca_common_pmi_lookup(const char *service_name, char **port_ret)
{
    char *port = NULL;
    int rc;

    *port_ret = port;
    // Allocate mem for port here? Otherwise we won't get success!
    // SLURM PMIv1 doesn't implement this function

    if (PMI_SUCCESS != (rc = PMI_Lookup_name(service_name, port))) {
        OPAL_PMI_ERROR(rc, "PMI_Lookup_name");
        return OPAL_ERROR;
    }

    *port_ret = port;
    return OPAL_SUCCESS;
}

int mca_common_pmi_unpublish ( const char *service_name )
{
    int rc;

    if (PMI_SUCCESS != (rc = PMI_Unpublish_name(service_name))) {
        OPAL_PMI_ERROR(rc, "PMI2_Nameserv_unpublish");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;;
}

int mca_common_pmi_barrier()
{
    /* use the PMI barrier function */
    if (PMI_SUCCESS != (rc = PMI_Barrier())) {
        OPAL_PMI_ERROR(rc, "PMI_Barrier");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

int mca_common_pmi_put(const char *kvs_name,
                       const char *key, const char *value)
{
    int rc;

    rc = PMI_KVS_Put(kvs_name, key, value);
    if( PMI_SUCCESS != rc ){
        OPAL_PMI_ERROR(rc, "PMI_KVS_Put");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

int mca_common_pmi_get(const char *kvs_name, const char *key,
                       char *value, int valuelen)
{
    int rc;

    rc = PMI_KVS_Get(kvs_name, key, value, valuelen);
    if( PMI_SUCCESS != rc ){
        OPAL_PMI_ERROR(rc, "PMI_KVS_Put");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

int mca_common_pmi_commit(char *kvs_name)
{
    if (PMI_SUCCESS != (rc = PMI_KVS_Commit(kvs_name))) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Commit");
        return OPAL_ERROR;
    }

    return mca_common_pmi_barrier();
}

