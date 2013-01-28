/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>
#include <pmi.h>
#if WANT_CRAY_PMI2_EXT
#include <pmi2.h>
#endif

#include "opal/dss/dss.h"
#include "opal/mca/hwloc/base/base.h"

#include "orte/mca/common/pmi/common_pmi.h"
#include "orte/mca/db/db.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_pmi.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag);
static int pmi_allgather(orte_grpcomm_collective_t *coll);
static int pmi_barrier(orte_grpcomm_collective_t *coll);
static int modex(orte_grpcomm_collective_t *coll);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_pmi_module = {
    init,
    finalize,
    xcast,
    pmi_allgather,
    pmi_barrier,
    modex
};

static char *pmi_kvs_name=NULL;

/**
 * Initialize the module
 */
static int init(void)
{
    int max_length, rc;

#if WANT_CRAY_PMI2_EXT
    /* TODO -- is this ok */
    max_length = 1024;
#else
    if (PMI_SUCCESS != (rc = PMI_KVS_Get_name_length_max(&max_length))) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Get_name_length_max");
        return ORTE_ERROR;
    }
#endif
    pmi_kvs_name = (char*)malloc(max_length);
    if (NULL == pmi_kvs_name) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

#if WANT_CRAY_PMI2_EXT
    rc = PMI2_Job_GetId(pmi_kvs_name, max_length);
#else
    rc = PMI_KVS_Get_my_name(pmi_kvs_name,max_length);
#endif
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Get_my_name");
        return ORTE_ERROR;
    }
    return ORTE_SUCCESS;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    if (NULL != pmi_kvs_name) {
        free(pmi_kvs_name);
    }
    return;
}

/**
 *  A "broadcast-like" function to a job's processes.
 *  @param  jobid   The job whose processes are to receive the message
 *  @param  buffer  The data to broadcast
 */

static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag)
{
    /* not used in this module */
    return ORTE_ERR_NOT_SUPPORTED;
}

static int pmi_barrier(orte_grpcomm_collective_t *coll)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if I am alone, just execute the callback */
    if (1 == orte_process_info.num_procs) {
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                             "%s grpcomm:pmi:barrier only one proc",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        coll->active = false;
        if (NULL != coll->cbfunc) {
            coll->cbfunc(NULL, coll->cbdata);
        }
        return ORTE_SUCCESS;
    }
    
#if WANT_CRAY_PMI2_EXT
    /* Cray doesn't provide a barrier, so use the Fence function here */
    if (PMI_SUCCESS != (rc = PMI2_KVS_Fence())) {
        ORTE_PMI_ERROR(rc, "PMI2_KVS_Fence");
        return ORTE_ERROR;
    }
#else
    /* use the PMI barrier function */
    if (PMI_SUCCESS != (rc = PMI_Barrier())) {
        ORTE_PMI_ERROR(rc, "PMI_Barrier");
        return ORTE_ERROR;
    }
#endif

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                         "%s grpcomm:pmi barrier complete",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    /* execute the callback */
    coll->active = false;
    if (NULL != coll->cbfunc) {
        coll->cbfunc(NULL, coll->cbdata);
    }

    return ORTE_SUCCESS;
}

static int pmi_allgather(orte_grpcomm_collective_t *coll)
{
    /* not used in this implementation */
    return ORTE_ERR_NOT_SUPPORTED;
}

/***   MODEX SECTION ***/
static int modex(orte_grpcomm_collective_t *coll)
{
    char *rml_uri;
    orte_vpid_t v;
    orte_process_name_t name;
    int rc;

     OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi: modex entered",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* commit our modex info */
#if WANT_CRAY_PMI2_EXT
    PMI2_KVS_Fence();
#else
    {
        int rc;
        
        if (PMI_SUCCESS != (rc = PMI_KVS_Commit(pmi_kvs_name))) {
            ORTE_PMI_ERROR(rc, "PMI_KVS_Commit");
            return ORTE_ERR_FATAL;
        }
        /* Barrier here to ensure all other procs have committed */
        PMI_Barrier();
    }
#endif

    /* cycle thru all my peers and collect their contact info in
     * case I need to send an RML message to them
     */
    name.jobid = ORTE_PROC_MY_NAME->jobid;
    for (v=0; v < orte_process_info.num_procs; v++) {
        if (v == ORTE_PROC_MY_NAME->vpid) {
            continue;
        }
        name.vpid = v;
	if (ORTE_SUCCESS != (rc = orte_db.fetch(&name, ORTE_DB_RMLURI, (void **)&rml_uri, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                             "%s grpcomm:pmi: proc %s oob endpoint %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name), rml_uri));
        /* set the contact info into the hash table */
        if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(rml_uri))) {
            free(rml_uri);
            return rc;
        }
        free(rml_uri);
    }

    /* execute the callback */
    coll->active = false;
    if (NULL != coll->cbfunc) {
        coll->cbfunc(NULL, coll->cbdata);
    }
    return ORTE_SUCCESS;
}
