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

static char *pmi_kvs_name=NULL;

/**
 * Initialize the module
 */
static int init(void)
{
    int max_length, rc;

#if WANT_PMI2_SUPPORT
    /* TODO -- is this ok */
    max_length = 1024;
#else
    if (PMI_SUCCESS != (rc = PMI_KVS_Get_name_length_max(&max_length))) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_name_length_max");
        return ORTE_ERROR;
    }
#endif
    pmi_kvs_name = (char*)malloc(max_length);
    if (NULL == pmi_kvs_name) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

#if WANT_PMI2_SUPPORT
    rc = PMI2_Job_GetId(pmi_kvs_name, max_length);
#else
    rc = PMI_KVS_Get_my_name(pmi_kvs_name,max_length);
#endif
    if (PMI_SUCCESS != rc) {
        OPAL_PMI_ERROR(rc, "PMI_KVS_Get_my_name");
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
            int found, sid, nodes, k;
            orte_vpid_t n;
            char *p;
            rc = PMI2_Info_GetJobAttr("PMI_process_mapping", pmapping, PMI2_MAX_VALLEN, &found);
            if (!found || PMI_SUCCESS != rc) { /* can't check PMI2_SUCCESS as some folks (i.e., Cray) don't define it */
                opal_output(0, "%s could not get PMI_process_mapping (PMI2_Info_GetJobAttr() failed)",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                return ORTE_ERROR;
            }

            i = 0; n = 0; local_rank_count = 0;
            if (NULL != (p = strstr(pmapping, "(vector"))) {
                while (NULL != (p = strstr(p+1, ",("))) {
                    if (3 == sscanf(p, ",(%d,%d,%d)", &sid, &nodes, &local_rank_count)) {
                        for (k = 0; k < nodes; k++) {
                            if ((ORTE_PROC_MY_NAME->vpid >= n) &&
                                (ORTE_PROC_MY_NAME->vpid < (n + local_rank_count))) {
                                break;
                            }
                            n += local_rank_count;
                        }
                    }
                }
            }
            free(pmapping);

            if ((local_rank_count > 0) && (local_rank_count < (int)orte_process_info.num_procs)) {
                local_ranks = (int*)malloc(local_rank_count * sizeof(int));
                for (i=0; i < local_rank_count; i++) {
                    local_ranks[i] = n + i;
                }
            }

            if (NULL == local_ranks) {
                opal_output(0, "%s could not get PMI_process_mapping",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                return ORTE_ERROR;
            }
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

	/* compute and store the locality as it isn't something that gets pushed to PMI */
        if (local) {
#if OPAL_HAVE_HWLOC
        {
            opal_hwloc_level_t bind_level, *lvptr;
            unsigned int bind_idx, *idxptr;
            opal_hwloc_locality_t locality;

            /* get the proc's locality info */
            lvptr = &bind_level;
	    if (ORTE_SUCCESS != (rc = opal_db.fetch((opal_identifier_t*)&name, ORTE_DB_BIND_LEVEL, (void**)&lvptr, OPAL_HWLOC_LEVEL_T))) {
		ORTE_ERROR_LOG(rc);
		return rc;
	    }
            idxptr = &bind_idx;
	    if (ORTE_SUCCESS != (rc = opal_db.fetch((opal_identifier_t*)&name, ORTE_DB_BIND_INDEX, (void**)&idxptr, OPAL_UINT))) {
		ORTE_ERROR_LOG(rc);
		return rc;
	    }
            /* determine relative location on our node */
            locality = opal_hwloc_base_get_relative_locality(opal_hwloc_topology,
                                                             orte_process_info.bind_level,
                                                             orte_process_info.bind_idx,
                                                             bind_level, bind_idx);
	    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
				 "%s grpcomm:pmi setting proc %s locale %s",
				 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
				 ORTE_NAME_PRINT(&name),
				 opal_hwloc_base_print_locality(locality)));
	}
#else
        locality = OPAL_PROC_ON_NODE;
#endif
	} else {
            /* this is on a different node, then mark as non-local */
            locality = OPAL_PROC_NON_LOCAL;
	}

        if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&name, OPAL_DB_INTERNAL, ORTE_DB_LOCALITY, &locality, OPAL_HWLOC_LOCALITY_T))) {
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
