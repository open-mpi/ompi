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
#include <pmi.h>
#if WANT_PMI2_SUPPORT
#include <pmi2.h>
#endif

#include "opal/dss/dss.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/common/pmi/common_pmi.h"
#include "opal/mca/db/db.h"

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
     return ORTE_SUCCESS;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
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
    
#if WANT_PMI2_SUPPORT
    /* PMI2 doesn't provide a barrier, so use the Fence function here */
    if (PMI_SUCCESS != (rc = PMI2_KVS_Fence())) {
        OPAL_PMI_ERROR(rc, "PMI2_KVS_Fence");
        return ORTE_ERROR;
    }
#else
    /* use the PMI barrier function */
    if (PMI_SUCCESS != (rc = PMI_Barrier())) {
        OPAL_PMI_ERROR(rc, "PMI_Barrier");
        return ORTE_ERROR;
    }
#endif

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

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:pmi: modex entered",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* discover the local ranks */
#if WANT_PMI2_SUPPORT
    {
        char *pmapping = (char*)malloc(PMI2_MAX_VALLEN);
        int found;
        int my_node;

        rc = PMI2_Info_GetJobAttr("PMI_process_mapping", pmapping, PMI2_MAX_VALLEN, &found);
        if (!found || PMI_SUCCESS != rc) { /* can't check PMI2_SUCCESS as some folks (i.e., Cray) don't define it */
            opal_output(0, "%s could not get PMI_process_mapping (PMI2_Info_GetJobAttr() failed)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            return ORTE_ERROR;
        }

        local_ranks = orte_grpcomm_pmi2_parse_pmap(pmapping, ORTE_PROC_MY_NAME->vpid, &my_node, &local_rank_count);
        if (NULL == local_ranks) {
            opal_output(0, "%s could not get PMI_process_mapping",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            return ORTE_ERROR;
        }

        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                "%s: pmapping: %s my_node=%d lr_count=%d\n",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), pmapping, my_node, local_rank_count));
       
        free(pmapping);
    }
#else
    rc = PMI_Get_clique_size (&local_rank_count);
    if (PMI_SUCCESS != rc) {
	ORTE_ERROR_LOG(ORTE_ERROR);
	return ORTE_ERROR;
    }

    local_ranks = calloc (local_rank_count, sizeof (int));
    if (NULL == local_ranks) {
	ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
	return ORTE_ERR_OUT_OF_RESOURCE;
    }

    rc = PMI_Get_clique_ranks (local_ranks, local_rank_count);
    if (PMI_SUCCESS != rc) {
	ORTE_ERROR_LOG(ORTE_ERROR);
	return ORTE_ERROR;
    }
#endif


    /* our RTE data was constructed and pushed in the ESS pmi component */

    /* commit our modex info */
    opal_db.commit((opal_identifier_t *)ORTE_PROC_MY_NAME);

    /* cycle thru all my peers and collect their RTE info */
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
	    if (ORTE_SUCCESS != (rc = opal_db.fetch_pointer((opal_identifier_t*)&name, OPAL_DB_CPUSET,
                                                            (void **)&cpuset, OPAL_STRING))) {
		ORTE_ERROR_LOG(rc);
		return rc;
	    }

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
	} else {
            /* this is on a different node, then mark as non-local */
            locality = OPAL_PROC_NON_LOCAL;
	}

        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                            "%s grpcomm:pmi proc %s locality %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name), opal_hwloc_base_print_locality(locality)));

        if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&name, OPAL_SCOPE_INTERNAL,
                                                OPAL_DB_LOCALITY, &locality, OPAL_HWLOC_LOCALITY_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* execute the callback */
    coll->active = false;
    if (NULL != coll->cbfunc) {
        coll->cbfunc(NULL, coll->cbdata);
    }
    return rc;
}
