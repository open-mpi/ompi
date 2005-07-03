/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include <errno.h>
#include <unistd.h>
#include <string.h>

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/ras/base/base.h"
#include "mca/ras/base/ras_base_node.h"
#include "mca/rmgr/base/base.h"
#include "ras_xgrid.h"


/*
 * Local functions
 */
static int allocate(orte_jobid_t jobid);
static int deallocate(orte_jobid_t jobid);
static int finalize(void);

static int discover(orte_jobid_t jobid, opal_list_t* nodelist);


/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_xgrid_module = {
    allocate,
    deallocate,
    finalize
};


/**
 * Discover available (pre-allocated) nodes.  Allocate the
 * requested number of nodes/process slots to the job.
 *  
 */
#include "mca/gpr/gpr.h"
static int allocate(orte_jobid_t jobid)
{
    int ret;
    opal_list_t nodes;
    opal_list_item_t* item;

    OBJ_CONSTRUCT(&nodes, opal_list_t);
    if (ORTE_SUCCESS != (ret = discover(jobid, &nodes))) {
        ompi_output(orte_ras_base.ras_output,
                    "ras:xgrid:allocate: discover failed!");
        return ret;
    }
    ret = orte_ras_base_allocate_nodes_by_node(jobid, &nodes);

    while (NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);

    /* All done */

    if (ORTE_SUCCESS == ret) {
        ompi_output(orte_ras_base.ras_output, 
                    "ras:xgrid:allocate: success");
    } else {
        ompi_output(orte_ras_base.ras_output, 
                    "ras:xgrid:allocate: failure (base_allocate_nodes=%d)", ret);
    }

    return ret;
}


/*
 * There's really nothing to do here
 */
static int deallocate(orte_jobid_t jobid)
{
    ompi_output(orte_ras_base.ras_output, 
                "ras:xgrid:deallocate: success (nothing to do)");
    return ORTE_SUCCESS;
}


/*
 * There's really nothing to do here
 */
static int finalize(void)
{
    ompi_output(orte_ras_base.ras_output, 
                "ras:xgrid:finalize: success (nothing to do)");
    return ORTE_SUCCESS;
}


/* discover number of available resouces.  Always exactly what asked for (surprise...) */
static int discover(orte_jobid_t jobid, opal_list_t* nodelist)
{
    int ret;
    orte_ras_base_node_t *node;
    opal_list_item_t* item;
    opal_list_t new_nodes;
    size_t num_requested = 0;
    size_t i;
    char *hostname;

    /* how many slots do we need? */
    if(ORTE_SUCCESS != (ret = orte_rmgr_base_get_job_slots(jobid, &num_requested))) {
        return ret;
    }

    /* create a "node" for each slot */
    OBJ_CONSTRUCT(&new_nodes, opal_list_t);
    for (i = 0 ; i < num_requested ; ++i) {
        asprintf(&hostname, "xgrid-node-%d", (int) i);
        node = OBJ_NEW(orte_ras_base_node_t);
        node->node_name = hostname;
        node->node_arch = strdup("unknown");
        node->node_state = ORTE_NODE_STATE_UP;
        node->node_cellid = 0;
        node->node_slots_inuse = 0;
        node->node_slots_max = 1;
        node->node_slots = 1;
        opal_list_append(&new_nodes, &node->super);
    }

    /* Add these nodes to the registry, and return all the values */
    ompi_output(orte_ras_base.ras_output, 
                "ras:xgrid:allocate:discover: done -- adding to registry");
    ret = orte_ras_base_node_insert(&new_nodes);
    for (item = opal_list_remove_first(&new_nodes);
         NULL != item; item = opal_list_remove_first(&new_nodes)) {
        if (ORTE_SUCCESS == ret) {
            opal_list_append(nodelist, item);
        } else {
            OBJ_RELEASE(item);
        }
    }

    /* All done */
    if (ORTE_SUCCESS == ret) {
        ompi_output(orte_ras_base.ras_output, 
                    "ras:xgrid:allocate:discover: success");
    } else {
        ompi_output(orte_ras_base.ras_output, 
                    "ras:xgrid:allocate:discover: failed (rc=%d)", ret);
    }
    OBJ_DESTRUCT(&new_nodes);
    return ret;
}
