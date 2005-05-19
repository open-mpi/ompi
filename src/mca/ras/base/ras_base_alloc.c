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


/**
 * 
 */

static int orte_ras_base_node_compare(orte_ras_base_node_t** n1,
                                      orte_ras_base_node_t** n2)
{
    if((*n1)->node_slots_inuse < (*n2)->node_slots_inuse) {
        return -1;
    } else if((*n1)->node_slots_inuse > (*n2)->node_slots_inuse) {
        return 1;
    }
    return 0;
}

/**
 *
 */

int orte_ras_base_allocate_nodes(orte_jobid_t jobid, ompi_list_t* nodes)
{
    ompi_list_t allocated;
    ompi_list_item_t* item;
    size_t num_requested = 0;
    size_t num_allocated = 0;
    size_t num_constrained = 0;
    int rc;

    /* query for the number of process slots required */
    if(ORTE_SUCCESS != (rc = orte_rmgr_base_get_job_slots(jobid, &num_requested))) {
        return rc;
    }
    /* sort the node list by proc slots inuse - lowest to highest */
    ompi_list_sort(nodes, (ompi_list_item_compare_fn_t)orte_ras_base_node_compare);

    OBJ_CONSTRUCT(&allocated, ompi_list_t);
    num_allocated = 0;

    /* iterate through nodes until request is satisfied or all are oversubscribed */
    while(num_allocated < num_requested) {
        num_constrained = 0;
        for(item = ompi_list_get_first(nodes);
            item != ompi_list_get_end(nodes) && num_allocated < num_requested;
            item = ompi_list_get_next(item)) {
            orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
                                                                                                                     
            /* are any slots available */
            if (node->node_slots_inuse >= node->node_slots) {
                                                                                                                     
                /* if there is a constraint on the max number of slots - skip this node */
                if(node->node_slots_max && node->node_slots_inuse >= node->node_slots_max) {
                    num_constrained++;
                    continue;
                }
            }
                                                                                                                     
            /* otherwise take one slot on this node */
            num_allocated++;
            node->node_slots_inuse++;  /* running total */
            node->node_slots_alloc++; /* this job */
        }
        if(num_constrained == ompi_list_get_size(nodes)) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
    }
                                                                                                                     
    /* move all nodes w/ allocations to the allocated list */
    item = ompi_list_get_first(nodes);
    while(item != ompi_list_get_end(nodes)) {
        orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
        ompi_list_item_t* next = ompi_list_get_next(item);
        if(node->node_slots_alloc) {
            ompi_list_remove_item(nodes, item);
            ompi_list_append(&allocated, item);
        }
        item = next;
    }

    rc = orte_ras_base_node_assign(&allocated, jobid);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }

cleanup:

    while(NULL != (item = ompi_list_remove_first(&allocated))) 
        ompi_list_append(nodes, item);
    OBJ_DESTRUCT(&allocated);
    return rc;
}

