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

#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "orte/include/orte_constants.h"
#include "orte/include/orte_types.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/ras/base/ras_base_node.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/ras/base/ras_base_node.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ras/hostfile/ras_hostfile.h"


/*
 * Local functions
 */
static int orte_ras_hostfile_allocate(orte_jobid_t jobid);
static int orte_ras_hostfile_deallocate(orte_jobid_t jobid);
static int orte_ras_hostfile_finalize(void);


/*
 * Local variables
 */
orte_ras_base_module_t orte_ras_hostfile_module = {
    orte_ras_hostfile_allocate,
    orte_ras_base_node_insert,
    orte_ras_base_node_query,
    orte_ras_hostfile_deallocate,
    orte_ras_hostfile_finalize
};


orte_ras_base_module_t *orte_ras_hostfile_init(int* priority)
{
    *priority = mca_ras_hostfile_component.priority;
    return &orte_ras_hostfile_module;
}


/*
 * THIS FUNCTION NEEDS TO CHANGE POST-1.0.
 *
 * After 1.0, this function, and the rds/hostfile need to change to
 * clean up properly.  They're not "broken" right now, so we're not
 * fixing them.  But they're implemented wrong, so they should be
 * adapted to the model that they're supposed to implement, not the
 * workarounds that they currently have.  The end result will be much,
 * much cleaner.
 *
 * Specifically, the rds/hostfile currently puts all of its nodes on
 * the resource segment *and* the node segment.  It should not.  It
 * should only put its nodes on the resource segment, appropriately
 * tagged that they came from a hostfile.  The ras/hostfile should
 * then examine the resources segment and pull out all nodes that came
 * from a hostfile and put them on the nodes segment.
 */
static int orte_ras_hostfile_allocate(orte_jobid_t jobid)
{
    opal_list_t nodes;
    opal_list_item_t* item;
    int rc;

    OBJ_CONSTRUCT(&nodes, opal_list_t);

    /* Query for all nodes in the node segment that have been
       allocated to this job */
    if (ORTE_SUCCESS != (rc = orte_ras_base_node_query_alloc(&nodes, jobid))) {
        goto cleanup;
    }

    /* If there are nodes allocated, then query for *all* nodes */
    if (opal_list_is_empty(&nodes)) {
        if (ORTE_SUCCESS != (rc = orte_ras_base_node_query(&nodes))) {
            goto cleanup;
        }
        
        /* If there are any nodes at all, allocate them all to this job */
        if (!opal_list_is_empty(&nodes)) {
            rc = orte_ras_base_allocate_nodes(jobid, &nodes);
            goto cleanup;
        }
    }

cleanup:
    while (NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);

    return rc;
}


static int orte_ras_hostfile_deallocate(orte_jobid_t jobid)
{
    /* Nothing to do */

    opal_output(orte_ras_base.ras_output, 
                "ras:hostfile:deallocate: success (nothing to do)");
    return ORTE_SUCCESS;
}


static int orte_ras_hostfile_finalize(void)
{
    /* Nothing to do */

    opal_output(orte_ras_base.ras_output, 
                "ras:hostfile:finalize: success (nothing to do)");
    return ORTE_SUCCESS;
}
