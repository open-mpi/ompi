/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include <errno.h>
#include <unistd.h>
#include <string.h>

#include "opal/util/argv.h"
#include "opal/util/output.h"

#include "orte/dss/dss.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/rmgr/base/rmgr_private.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ras/base/ras_private.h"

#include "ras_xgrid.h"


/*
 * Local functions
 */
static int allocate(orte_jobid_t jobid, opal_list_t *attributes);
static int deallocate(orte_jobid_t jobid);
static int finalize(void);

static int discover(orte_jobid_t jobid, opal_list_t* nodelist);


/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_xgrid_module = {
    allocate,
    orte_ras_base_node_insert,
    orte_ras_base_node_query,
    orte_ras_base_node_query_alloc,
    orte_ras_base_node_lookup,
    deallocate,
    finalize
};


/**
 * Discover available (pre-allocated) nodes.  Allocate the
 * requested number of nodes/process slots to the job.
 *  
 */
static int allocate(orte_jobid_t jobid, opal_list_t *attributes)
{
    int ret;
    opal_list_t nodes;
    opal_list_item_t* item;

    OBJ_CONSTRUCT(&nodes, opal_list_t);

    if (ORTE_SUCCESS != (ret = discover(jobid, &nodes))) {
        opal_output(orte_ras_base.ras_output,
                    "ras:xgrid:allocate: discover failed!");
        return ret;
    }
    ret = orte_ras_base_allocate_nodes(jobid, &nodes);

    while (NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);

    /* All done */

    if (ORTE_SUCCESS == ret) {
        opal_output(orte_ras_base.ras_output, 
                    "ras:xgrid:allocate: success");
    } else {
        opal_output(orte_ras_base.ras_output, 
                    "ras:xgrid:allocate: failure (base_allocate_nodes=%d)", ret);
    }

    return ret;
}

/*
 * There's really nothing to do here
 */
static int deallocate(orte_jobid_t jobid)
{
    opal_output(orte_ras_base.ras_output, 
                "ras:xgrid:deallocate: success (nothing to do)");
    return ORTE_SUCCESS;
}


/*
 * There's really nothing to do here
 */
static int finalize(void)
{
    opal_output(orte_ras_base.ras_output, 
                "ras:xgrid:finalize: success (nothing to do)");
    return ORTE_SUCCESS;
}


/* discover number of available resouces.  Always exactly what asked for (surprise...) */
static int discover(orte_jobid_t jobid, opal_list_t* nodelist)
{
    int ret;
    orte_ras_node_t *node;
    orte_std_cntr_t num_requested = 0;
    orte_std_cntr_t i;
    char *hostname;

    /* how many slots do we need? */
    if(ORTE_SUCCESS != (ret = orte_rmgr_base_get_job_slots(jobid, &num_requested))) {
        return ret;
    }

    /* create a "node" for each slot */
    for (i = 0 ; i < num_requested ; ++i) {
        asprintf(&hostname, "xgrid-node-%d", (int) i);
        node = OBJ_NEW(orte_ras_node_t);
        node->node_name = hostname;
        node->node_arch = NULL;
        node->node_state = ORTE_NODE_STATE_UP;
        node->node_cellid = 0;
        node->node_slots_inuse = 0;
        node->node_slots_max = 0;
        node->node_slots = 1;
        opal_list_append(nodelist, &node->super);
    }

    /* Add these nodes to the registry, and return all the values */
    opal_output(orte_ras_base.ras_output, 
                "ras:xgrid:allocate:discover: done -- adding to registry");
    ret = orte_ras_base_node_insert(nodelist);

    /* All done */
    if (ORTE_SUCCESS == ret) {
        opal_output(orte_ras_base.ras_output, 
                    "ras:xgrid:allocate:discover: success");
    } else {
        opal_output(orte_ras_base.ras_output, 
                    "ras:xgrid:allocate:discover: failed (rc=%d)", ret);
    }

    return ret;
}
