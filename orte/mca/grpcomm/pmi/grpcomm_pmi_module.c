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

#if WANT_CRAY_PMI2_EXT
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

#if WANT_CRAY_PMI2_EXT
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
    
#if WANT_CRAY_PMI2_EXT
    /* Cray doesn't provide a barrier, so use the Fence function here */
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
    char *cptr, **fields;
    orte_vpid_t v;
    orte_process_name_t name;
    int rc;
    opal_hwloc_locality_t locality;
    orte_local_rank_t local_rank;
    orte_node_rank_t node_rank;
    bool bound;

     OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:pmi: modex entered",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

     /* our RTE data was constructed and pushed in the ESS pmi component */

    /* commit our modex info */
#if WANT_CRAY_PMI2_EXT
    PMI2_KVS_Fence();
#else
    {
        int rc;
        
        if (PMI_SUCCESS != (rc = PMI_KVS_Commit(pmi_kvs_name))) {
            OPAL_PMI_ERROR(rc, "PMI_KVS_Commit");
            return ORTE_ERR_FATAL;
        }
        /* Barrier here to ensure all other procs have committed */
        PMI_Barrier();
    }
#endif

    /* cycle thru all my peers and collect their RTE info */
    name.jobid = ORTE_PROC_MY_NAME->jobid;
    fields = NULL;
    for (v=0; v < orte_process_info.num_procs; v++) {
        if (v == ORTE_PROC_MY_NAME->vpid) {
            continue;
        }
        name.vpid = v;
        /* fetch the RTE data for this proc */
	if (ORTE_SUCCESS != (rc = opal_db.fetch((opal_identifier_t*)&name, "RTE", (void **)&cptr, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* split on commas */
        fields = opal_argv_split(cptr, ',');
        free(cptr);
        /* sanity check */
        if (6 != opal_argv_count(fields)) {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            opal_argv_free(fields);
            return ORTE_ERR_BAD_PARAM;
        }
        
        /* store the composite parts */
        /* first field is the URI */
        if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&name, OPAL_DB_INTERNAL, ORTE_DB_RMLURI, fields[0], OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            opal_argv_free(fields);
            return rc;
        }
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:pmi: proc %s oob endpoint %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name), fields[0]));
        /* set the contact info into the hash table */
        if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(fields[0]))) {
            opal_argv_free(fields);
            return rc;
        }
        /* next is the hostname */
        if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&name, OPAL_DB_INTERNAL, ORTE_DB_HOSTNAME, fields[1], OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            opal_argv_free(fields);
            return rc;
        }
        /* local rank */
        local_rank = strtoul(fields[2], NULL, 10);
        if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&name, OPAL_DB_INTERNAL, ORTE_DB_LOCALRANK, &local_rank, ORTE_LOCAL_RANK))) {
            ORTE_ERROR_LOG(rc);
            opal_argv_free(fields);
            return rc;
        }
        /* node rank */
        node_rank = strtoul(fields[3], NULL, 10);
        if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&name, OPAL_DB_INTERNAL, ORTE_DB_NODERANK, &node_rank, ORTE_NODE_RANK))) {
            ORTE_ERROR_LOG(rc);
            opal_argv_free(fields);
            return rc;
        }
        /* if the process was bound, then there will be another field
         * that contains its cpuset
         */
        if (5 == opal_argv_count(fields)) {
            if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&name, OPAL_DB_INTERNAL, ORTE_DB_CPUSET, fields[4], OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                opal_argv_free(fields);
                return rc;
            }
            bound = true;
        } else {
            /* store a placeholder so we know that this value was retrieved,
             * but the proc wasn't bound
             */
            if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&name, OPAL_DB_INTERNAL, ORTE_DB_CPUSET, NULL, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                opal_argv_free(fields);
                return rc;
            }
            bound = false;
        }

        /* compute and store the locality as it isn't something that gets pushed to PMI */
        if (0 != strcmp(fields[1], orte_process_info.nodename)) {
            /* this is on a different node, then mark as non-local */
            locality = OPAL_PROC_NON_LOCAL;
        } else if (!bound) {
            /* if we share a node, but we don't know anything more, then
             * mark us as on the node as this is all we know
             */
            locality = OPAL_PROC_ON_NODE;
        } else {
            /* determine relative location on our node */
            locality = opal_hwloc_base_get_relative_locality(opal_hwloc_topology,
                                                             orte_process_info.cpuset,
                                                             fields[4]);
        }
        if (ORTE_SUCCESS != (rc = opal_db.store((opal_identifier_t*)&name, OPAL_DB_INTERNAL, ORTE_DB_LOCALITY, &locality, OPAL_HWLOC_LOCALITY_T))) {
            ORTE_ERROR_LOG(rc);
            opal_argv_free(fields);
            return rc;
        }

        /* cleanup */
        opal_argv_free(fields);
        fields = NULL;
    }

    /* execute the callback */
    coll->active = false;
    if (NULL != coll->cbfunc) {
        coll->cbfunc(NULL, coll->cbdata);
    }
    return rc;
}
