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
#include "include/orte_constants.h"

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/ras/base/base.h"
#include "mca/ras/base/ras_base_node.h"
#include "mca/rmgr/base/base.h"
#include "mca/errmgr/errmgr.h"


/*
 * Allocate one process per node on a round-robin basis, looping back
 * around to the beginning as necessary
 */
int orte_ras_base_allocate_nodes_by_node(orte_jobid_t jobid, 
                                         opal_list_t* nodes)
{
    opal_list_t allocated;
    opal_list_item_t* item;
    size_t num_requested = 0;
    size_t num_allocated = 0;
    size_t num_constrained = 0;
    size_t slots;
    bool oversubscribe = false;
    int rc;

    /* query for the number of process slots required */
    if (ORTE_SUCCESS != 
       (rc = orte_rmgr_base_get_job_slots(jobid, &num_requested))) {
        return rc;
    }

    OBJ_CONSTRUCT(&allocated, opal_list_t);
    num_allocated = 0;

    /* This loop continues until all procs have been allocated or we run
       out of resources.  There are two definitions of "run out of
       resources":

       1. All nodes have node_slots processes allocated to them
       2. All nodes have node_slots_max processes allocated to them

       We first map until condition #1 is met.  If there are still
       processes that haven't been allocated yet, then we continue
       until condition #2 is met.  If we still have processes that
       haven't been allocated yet, then it's an "out of resources"
       error. */
    while (num_allocated < num_requested) {
        num_constrained = 0;

        /* loop over all nodes until either all processes are
           allocated or they all become constrained */
        for (item = opal_list_get_first(nodes);
             item != opal_list_get_end(nodes) && num_allocated < num_requested;
             item = opal_list_get_next(item)) {
            orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
            
            /* are any slots available? */
            slots = (oversubscribe ? node->node_slots_max : node->node_slots);
            if (node->node_slots_inuse < slots ||
                (oversubscribe && 0 == slots)) {
                ++num_allocated;
                ++node->node_slots_inuse; /* running total */
                ++node->node_slots_alloc; /* this job */
            } else {
                ++num_constrained;
            }
        }

        /* if all nodes are constrained:
           - if this is the first time through the loop, then set
             "oversubscribe" to true, and we'll now start obeying
             node_slots_max instead of node_slots
           - if this is the second time through the loop, then all
             nodes are full to the max, and therefore we can't do
             anything more -- we're out of resources */
        if (opal_list_get_size(nodes) == num_constrained) {
            if (oversubscribe) {
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                goto cleanup;
            } else {
                oversubscribe = true;
            }
        }
    }

    /* move all nodes w/ allocations to the allocated list */
    item = opal_list_get_first(nodes);
    while(item != opal_list_get_end(nodes)) {
        orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
        opal_list_item_t* next = opal_list_get_next(item);
        if(node->node_slots_alloc) {
            opal_list_remove_item(nodes, item);
            opal_list_append(&allocated, item);
        }
        item = next;
    }

    rc = orte_ras_base_node_assign(&allocated, jobid);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
cleanup:

    while(NULL != (item = opal_list_remove_first(&allocated))) 
        opal_list_append(nodes, item);
    OBJ_DESTRUCT(&allocated);
    return rc;
}


/*
 * Allocate processes to nodes, using all available slots on a node.
 */
int orte_ras_base_allocate_nodes_by_slot(orte_jobid_t jobid, 
                                         opal_list_t* nodes)
{
    opal_list_t allocated;
    opal_list_item_t* item;
    size_t num_requested = 0;
    size_t num_allocated = 0;
    size_t num_constrained = 0;
    size_t available;
    int rc;

    /* query for the number of process slots required */
    if (ORTE_SUCCESS != 
       (rc = orte_rmgr_base_get_job_slots(jobid, &num_requested))) {
        return rc;
    }

    OBJ_CONSTRUCT(&allocated, opal_list_t);
    num_allocated = 0;

    /* In the first pass, just grab all available slots (i.e., stay <=
       node_slots) greedily off each node */
    for (item = opal_list_get_first(nodes);
         item != opal_list_get_end(nodes) && num_allocated < num_requested;
         item = opal_list_get_next(item)) {
        orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
        
        /* are any slots available? */
        if (node->node_slots_inuse < node->node_slots) {
            available = node->node_slots - node->node_slots_inuse;
            if (num_requested - num_allocated < available) {
                node->node_slots_inuse +=
                    (num_requested - num_allocated); /* running total */
                node->node_slots_alloc +=
                    (num_requested - num_allocated); /* this job */
                num_allocated = num_requested;
            } else {
                num_allocated += available;
                node->node_slots_inuse += available; /* running total */
                node->node_slots_alloc += available; /* this job */
            }
        }
    }
    
    /* If we're not done, then we're in an oversubscribing situation.
       Switch to a round-robin-by-node policy -- take one slot from
       each node until we hit node_slots_max or we have no more
       resources; whichever occurs first. */
    while (num_allocated < num_requested) {
        num_constrained = 0;

        /* loop over all nodes until either all processes are
           allocated or they all become constrained */
        for (item = opal_list_get_first(nodes);
             item != opal_list_get_end(nodes) && num_allocated < num_requested;
             item = opal_list_get_next(item)) {
            orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
            
            /* are any slots available? */
            if (node->node_slots_inuse < node->node_slots_max ||
                0 == node->node_slots_max) {
                ++num_allocated;
                ++node->node_slots_inuse; /* running total */
                ++node->node_slots_alloc; /* this job */
            } else {
                ++num_constrained;
            }
        }

        /* if all nodes are constrained, then we're out of resources
           -- thanks for playing */
        if (opal_list_get_size(nodes) == num_constrained) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
    }

    /* move all nodes w/ allocations to the allocated list */
    item = opal_list_get_first(nodes);
    while(item != opal_list_get_end(nodes)) {
        orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
        opal_list_item_t* next = opal_list_get_next(item);
        if(node->node_slots_alloc) {
            opal_list_remove_item(nodes, item);
            opal_list_append(&allocated, item);
        }
        item = next;
    }

    rc = orte_ras_base_node_assign(&allocated, jobid);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }

cleanup:

    while(NULL != (item = opal_list_remove_first(&allocated))) 
        opal_list_append(nodes, item);
    OBJ_DESTRUCT(&allocated);
    return rc;
}
