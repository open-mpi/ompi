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
#if WANT_PMI2_SUPPORT
#include <pmi2.h>
#endif

#include "common_pmi.h"

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


#if WANT_PMI2_SUPPORT
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
#endif

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


int mca_common_pmi_init (int preferred_version) {
    int rc = OPAL_SUCCESS;
    if (0 < mca_common_pmi_init_count++) {
        return rc;
    }

    // Decide what version of PMI we want
#if WANT_PMI2_SUPPORT
    {
        bool auto_select = !(preferred_version >= 1 &&  preferred_version <= 2);
        if( auto_select ){
            // choose PMIv2
            mca_common_pmi_version = 2;
        }else{
            mca_common_pmi_version = preferred_version;
        }

        if( mca_common_pmi_version == 2 ){
            rc = mca_initialize_pmi_v2();
            if( !auto_select || rc == OPAL_SUCCESS ){
                // If we want exactly PMIv2 or we succeed
                if( rc != OPAL_SUCCESS ){
                    mca_common_pmi_init_count--;
                }
                return rc;
            }
        }
    }
#endif
    mca_common_pmi_version = 1;
    if( OPAL_SUCCESS != (rc = mca_initialize_pmi_v1()) ){
        mca_common_pmi_init_count--;
    }
    return rc;
}

void mca_common_pmi_finalize (void) {
    if (0 == mca_common_pmi_init_count) {
        return;
    }

    if (0 == --mca_common_pmi_init_count) {
#if WANT_PMI2_SUPPORT
        if( mca_common_pmi_version == 2){
            PMI2_Finalize ();
        }
        else
#endif
        {
            PMI_Finalize ();
        }
    }
}

/* useful util */
char* opal_errmgr_base_pmi_error(int pmi_err)
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

#if WANT_PMI2_SUPPORT
    if( mca_common_pmi_version == 2 ){
        // TODO: add proper error handling
        pmi_id = (char*)malloc(PMI2_MAX_VALLEN);
        if( pmi_id == NULL ){
            *error = "mca_common_pmi_id: could not get memory for PMIv2 ID";
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        strncpy(pmi_id, pmi_kvs_name, pmi_kvslen_max);
    }
    else
#endif
    {
        int pmi_maxlen;
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

#if WANT_PMI2_SUPPORT
    if(mca_common_pmi_version == 2){

        {
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
        }

    }
    else
#endif
    {
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
    }

    *ranks_ret = ranks;
    *procs_ret = procs;
    return OPAL_SUCCESS;
}

void mca_common_pmi_abort(int status, char *msg)
{
#if WANT_PMI2_SUPPORT
    if( mca_common_pmi_version == 2){
        PMI2_Abort(status, msg);
    }
    else
#endif
    {
        PMI_Abort(status, msg);
    }
}

int rc;

int mca_common_pmi_publish(const char *service_name, const char *port_name)
{
#if WANT_PMI2_SUPPORT
    if( mca_common_pmi_version == 2){
        if (PMI2_SUCCESS != (rc = PMI2_Nameserv_publish(service_name, NULL, port_name))) {
            OPAL_PMI_ERROR(rc, "PMI2_Nameserv_publish");
            return OPAL_ERROR;
        }
    }
    else
#endif
    {
        if (PMI_SUCCESS != (rc = PMI_Publish_name(service_name, port_name))) {
            OPAL_PMI_ERROR(rc, "PMI_Publish_name");
            return OPAL_ERROR;
        }
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

#if WANT_PMI2_SUPPORT
    if( mca_common_pmi_version == 2 ){
        port = (char*)malloc(1024*sizeof(char));  /* arbitrary size */
        if( port == NULL ){
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        if (PMI_SUCCESS != (rc = PMI2_Nameserv_lookup(service_name, NULL, port, 1024))) {
            OPAL_PMI_ERROR(rc, "PMI2_Nameserv_lookup");
            free(port);
            return OPAL_ERROR;
        }
    }
    else
#endif
    {
        // Allocate mem for port here? Otherwise we won't get success!
        // SLURM PMIv1 doesn't implement this function

        if (PMI_SUCCESS != (rc = PMI_Lookup_name(service_name, port))) {
            OPAL_PMI_ERROR(rc, "PMI_Lookup_name");
            return OPAL_ERROR;
        }
    }

    *port_ret = port;
    return OPAL_SUCCESS;
}

int mca_common_pmi_unpublish ( const char *service_name )
{
    int rc;

#if WANT_PMI2_SUPPORT
    if( mca_common_pmi_version == 2 ){
        if (PMI2_SUCCESS != (rc = PMI2_Nameserv_unpublish(service_name, NULL))) {
            OPAL_PMI_ERROR(rc, "PMI2_Nameserv_unpublish");
            return OPAL_ERROR;
        }
    }
    else
#endif
    {
        if (PMI_SUCCESS != (rc = PMI_Unpublish_name(service_name))) {
            OPAL_PMI_ERROR(rc, "PMI2_Nameserv_unpublish");
            return OPAL_ERROR;
        }
    }
    return OPAL_SUCCESS;;
}

int mca_common_pmi_barrier()
{
#if WANT_PMI2_SUPPORT
    if( mca_common_pmi_version == 2 ){
        /* PMI2 doesn't provide a barrier, so use the Fence function here */
        if (PMI2_SUCCESS != (rc = PMI2_KVS_Fence())) {
            // FIX ME: OPAL_PMI2_ERROR(rc, "PMI2_KVS_Fence");
            return OPAL_ERROR;
        }
    }
    else
#endif
    {
        /* use the PMI barrier function */
        if (PMI_SUCCESS != (rc = PMI_Barrier())) {
            OPAL_PMI_ERROR(rc, "PMI_Barrier");
            return OPAL_ERROR;
        }
    }
    return OPAL_SUCCESS;
}

int mca_common_pmi_put(const char *kvs_name,
                              const char *key, const char *value)
{
    int rc;
#if WANT_PMI2_SUPPORT
    if( mca_common_pmi_version == 2 ){
        if( PMI2_SUCCESS != PMI2_KVS_Put(key, value) ){
            // FIXME: OPAL_PMI2_ERROR(rc, "PMI2_KVS_Put");
            return OPAL_ERROR;
        }
    }
    else
#endif
    {
        rc = PMI_KVS_Put(kvs_name, key, value);
        if( PMI_SUCCESS != rc ){
            OPAL_PMI_ERROR(rc, "PMI_KVS_Put");
            return OPAL_ERROR;
        }
    }
    return OPAL_SUCCESS;
}

int mca_common_pmi_get(const char *kvs_name, const char *key,
                       char *value, int valuelen)
{
    int rc;
#if WANT_PMI2_SUPPORT
    if( mca_common_pmi_version == 2 ){
        int len;
        rc = PMI2_KVS_Get(kvs_name, PMI2_ID_NULL, key, value, valuelen, &len);
        if( PMI2_SUCCESS != rc ){
            // OPAL_PMI2_ERROR(rc, "PMI_KVS_Put");
            return OPAL_ERROR;
        }
    }
    else
#endif
    {
        rc = PMI_KVS_Get(kvs_name, key, value, valuelen);
        if( PMI_SUCCESS != rc ){
            OPAL_PMI_ERROR(rc, "PMI_KVS_Put");
            return OPAL_ERROR;
        }
    }
    return OPAL_SUCCESS;
}

int mca_common_pmi_commit(char *kvs_name)
{
    if( mca_common_pmi_version == 1 ){

        if (PMI_SUCCESS != (rc = PMI_KVS_Commit(kvs_name))) {
            OPAL_PMI_ERROR(rc, "PMI_KVS_Commit");
            return OPAL_ERROR;
        }
    }
    return mca_common_pmi_barrier();
}

