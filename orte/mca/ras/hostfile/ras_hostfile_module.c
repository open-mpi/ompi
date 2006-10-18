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

#include "opal/util/output.h"
#include "opal/util/argv.h"

#include "orte/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ras/base/ras_private.h"
#include "orte/mca/ras/hostfile/ras_hostfile.h"


/*
 * Local functions
 */
static int orte_ras_hostfile_allocate(orte_jobid_t jobid, opal_list_t *attributes);
static int orte_ras_hostfile_deallocate(orte_jobid_t jobid);
static int orte_ras_hostfile_finalize(void);


/*
 * Local variables
 */
orte_ras_base_module_t orte_ras_hostfile_module = {
    orte_ras_hostfile_allocate,
    orte_ras_base_node_insert,
    orte_ras_base_node_query,
    orte_ras_base_node_query_alloc,
    orte_ras_base_node_lookup,
    orte_ras_hostfile_deallocate,
    orte_ras_hostfile_finalize
};


orte_ras_base_module_t *orte_ras_hostfile_init(int* priority)
{
    /* if we are not an HNP, then we must not be selected */
    if (!orte_process_info.seed) {
        return NULL;
    }
    
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
static int orte_ras_hostfile_allocate(orte_jobid_t jobid, opal_list_t *attributes)
{
    opal_list_t nodes;
    opal_list_item_t* item;
    int rc;
    orte_jobid_t *jptr;
    orte_attribute_t *attr;

    OBJ_CONSTRUCT(&nodes, opal_list_t);

    /* check the attributes to see if we are supposed to use the parent
     * jobid's allocation. This can occur if we are doing a dynamic
     * process spawn and don't want to go through the allocator again
     */
    if (NULL != (attr = orte_rmgr.find_attribute(attributes, ORTE_RAS_USE_PARENT_ALLOCATION))) {
        /* attribute was given - just reallocate to the new jobid */
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&jptr, attr->value, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = orte_ras_base_reallocate(*jptr, jobid))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        return ORTE_SUCCESS;
    }
    
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
