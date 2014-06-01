/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
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

#include "opal/dss/dss.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/runtime/opal_params.h"
#include "opal/mca/common/pmi/common_pmi.h"
#include "opal/mca/dstore/dstore.h"

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

/**
 * Initialize the module
 */
static int init(void)
{
    return mca_common_pmi_init(opal_pmi_version);
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    mca_common_pmi_finalize();
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
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:pmi entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if I am alone, just execute the callback */
    if (1 == orte_process_info.num_procs) {
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:pmi:barrier only one proc",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        coll->active = false;
        if (NULL != coll->cbfunc) {
            coll->cbfunc(NULL, coll->cbdata);
        }
        return ORTE_SUCCESS;
    }
    
    if( OPAL_SUCCESS != (rc = mca_common_pmi_barrier()) ){
        return rc;
    }

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
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
    int *local_ranks, local_rank_count;
    opal_hwloc_locality_t locality;
    const char *cpuset;
    orte_process_name_t name;
    orte_vpid_t v;
    bool local;
    int rc, i;
    opal_list_t myvals;
    opal_value_t *kv, kvn;
    char *error;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:pmi: modex entered",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* discover the local ranks */
    rc = mca_common_pmi_local_info(ORTE_PROC_MY_NAME->vpid, &local_ranks,
                                  &local_rank_count, &error);
    if( OPAL_SUCCESS != rc){
        opal_output(0, "%s could not get PMI_process_mapping: %s",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), error);
        return ORTE_ERROR;
    }

    /* our RTE data was constructed and pushed in the ESS pmi component */

    /* cycle thru all my local peers and collect their RTE info - we need
     * that info to support some of the BTLs such as shared memory
     */
    name.jobid = ORTE_PROC_MY_NAME->jobid;
    for (v=0; v < orte_process_info.num_procs; v++) {
        if (v == ORTE_PROC_MY_NAME->vpid) {
            continue;
        }
        name.vpid = v;

	/* check if this is a local process */
	for (i = 0, local = false ; i < local_rank_count ; ++i) {
	    if ((orte_vpid_t) local_ranks[i] == v) {
		local = true;
		break;
	    }
	}

	/* compute and store the locality as it isn't something that gets pushed to PMI  - doing
         * this here will prevent the MPI layer from fetching data for all ranks
         */
        if (local) {
            OBJ_CONSTRUCT(&myvals, opal_list_t);
	    if (ORTE_SUCCESS != (rc = opal_dstore.fetch(opal_dstore_nonpeer,
                                                        (opal_identifier_t*)&name,
                                                        OPAL_DSTORE_CPUSET, &myvals))) {
		ORTE_ERROR_LOG(rc);
                OPAL_LIST_DESTRUCT(&myvals);
		return rc;
	    }
            kv = (opal_value_t*)opal_list_get_first(&myvals);
            cpuset = kv->data.string;
	    if (NULL == cpuset) {
		/* if we share a node, but we don't know anything more, then
		 * mark us as on the node as this is all we know
		 */
                locality = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
	    } else {
		/* determine relative location on our node */
		locality = opal_hwloc_base_get_relative_locality(opal_hwloc_topology,
								 orte_process_info.cpuset,
								 (char *) cpuset);
	    }
            OPAL_LIST_DESTRUCT(&myvals);
	} else {
            /* this is on a different node, then mark as non-local */
            locality = OPAL_PROC_NON_LOCAL;
	}

        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:pmi proc %s locality %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name), opal_hwloc_base_print_locality(locality)));

        OBJ_CONSTRUCT(&kvn, opal_value_t);
        kvn.key = strdup(OPAL_DSTORE_LOCALITY);
        kvn.type = OPAL_UINT16;
        kvn.data.uint16 = locality;
        if (ORTE_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal,
                                                    (opal_identifier_t*)&name, &kvn))) {
            OBJ_DESTRUCT(&kvn);
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&kvn);
    }

    /* execute the callback */
    coll->active = false;
    if (NULL != coll->cbfunc) {
        coll->cbfunc(NULL, coll->cbdata);
    }
    return rc;
}
