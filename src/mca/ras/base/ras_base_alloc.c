/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

    /* if a specified number of nodes was requested - attempt to locate 
     * the number requested - selecting unused nodes first - then anything
     * that can be oversubscribed
    */
    OBJ_CONSTRUCT(&allocated, ompi_list_t);
    num_allocated = 0;
    if(orte_ras_base.ras_num_nodes != 0) {
        size_t num_per_node = num_requested / orte_ras_base.ras_num_nodes;
        if(num_per_node == 0)
           num_per_node = 1;

        /* are enough nodes available to satisfy the request */
        if(ompi_list_get_size(nodes) < orte_ras_base.ras_num_nodes) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* allocate nodes to the job */
        while(item != ompi_list_get_last(nodes) && 
            ompi_list_get_size(&allocated) < orte_ras_base.ras_num_nodes) {
            orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
            ompi_list_item_t* next = ompi_list_get_next(item);
            
            if (num_requested - num_allocated < num_per_node)
                num_per_node = num_requested - num_allocated;

            if (node->node_slots_max && 
                node->node_slots_max > num_per_node + node->node_slots_inuse) {
                size_t num_avail = node->node_slots_max - node->node_slots_inuse;
                num_allocated += num_avail;
                node->node_slots_inuse += num_avail;
                node->node_slots_alloc += num_avail;
                num_constrained++;
            } else {
                num_allocated += num_per_node;
                node->node_slots_inuse += num_per_node;
                node->node_slots_alloc += num_per_node;
            }
            ompi_list_remove_item(nodes, item);
            ompi_list_append(&allocated, item);
            item = next;
        }
        goto validate;
    }

    /* the number of nodes was not specified - so try to allocate the
     * most available nodes to the request before oversubscribing
    */
    for(item = ompi_list_get_first(nodes);
        item != ompi_list_get_end(nodes) && num_allocated < num_requested;
        ) {
        orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
        ompi_list_item_t* next = ompi_list_get_next(item);

        /* are any slots available */
        if (node->node_slots_inuse >= node->node_slots) {

            /* if there is a constraint on the max number of slots - skip this node */
            if(node->node_slots_max && node->node_slots_max >= node->node_slots_inuse) {
                item = next;
                continue;
                num_constrained++;
            } 

            /* otherwise only take one slot on this node */
            num_allocated++;
            node->node_slots_inuse++;  /* running total */
            node->node_slots_alloc++; /* this job */
        }

        /* take available slots on this node */
        else {
            size_t num_to_alloc = node->node_slots - node->node_slots_inuse;
            if(num_to_alloc + num_allocated > num_requested)
                 num_to_alloc = num_requested - num_allocated;
            num_allocated += num_to_alloc;
            node->node_slots_inuse += num_to_alloc;
            node->node_slots_alloc += num_to_alloc;
        }

        ompi_list_remove_item(nodes, item);
        ompi_list_append(&allocated, item);
        item = next;
    }

    /* if the requested number of process slots hasn't been satisfied 
     * must oversubscribe each of the nodes (where allowed)
    */

validate:
    if(num_allocated < num_requested) {
        size_t num_nodes = ompi_list_get_size(&allocated) - num_constrained;
        size_t num_needed = num_requested - num_allocated;
        size_t num_per_node;

        /* are there any nodes that can be oversubscribed? */
        if(num_nodes == 0) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        } 

        num_per_node = num_needed / num_nodes;
        if(num_per_node == 0)
           num_per_node = 1;

        /* oversubscribe each of the nodes that allow it */
        for(item = ompi_list_get_first(&allocated);
            item != ompi_list_get_end(&allocated);
            item = ompi_list_get_next(item)) {
            orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
            if(node->node_slots_max && node->node_slots_inuse >= node->node_slots_max)
                continue;
            if(num_allocated + num_per_node > num_requested) {
                num_per_node = num_requested - num_allocated;
            }
            num_allocated += num_per_node;
            node->node_slots_inuse += num_per_node; /* running total */
            node->node_slots_alloc += num_per_node; /* this job */
        }
    }

    rc = orte_ras_base_node_assign(&allocated, jobid);
cleanup:

    while(NULL != (item = ompi_list_remove_first(&allocated))) 
        ompi_list_append(nodes, item);
    OBJ_DESTRUCT(&allocated);
    return rc;
}

