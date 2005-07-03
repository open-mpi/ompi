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
#include "util/output.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"
#include "mca/rmaps/base/base.h"
#include "mca/rmgr/base/base.h"
#include "mca/rmaps/base/rmaps_base_map.h"
#include "mca/ras/base/ras_base_node.h"
#include "rmaps_rr.h"


/*
 * Local variable
 */
static opal_list_item_t *cur_node_item = NULL;


static int claim_slot(orte_rmaps_base_map_t *map, 
                      orte_ras_base_node_t *current_node,
                      orte_jobid_t jobid, orte_vpid_t vpid, int proc_index)
{
    orte_rmaps_base_proc_t *proc;
    orte_process_name_t *proc_name;
    orte_rmaps_base_node_t *rmaps_node;
    int rc;
    
    /* create objects */
    rmaps_node = OBJ_NEW(orte_rmaps_base_node_t);
    if (NULL == rmaps_node) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    rmaps_node->node_name = strdup(current_node->node_name);
    
    proc = OBJ_NEW(orte_rmaps_base_proc_t);
    if (NULL == proc) {
        OBJ_RELEASE(rmaps_node);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* create the process name as an offset from the vpid-start */
    rc = orte_ns.create_process_name(&proc_name, current_node->node_cellid,
                                     jobid, vpid);
    if (rc != ORTE_SUCCESS) {
        OBJ_RELEASE(proc);
        OBJ_RELEASE(rmaps_node);
        return rc;
    }
    proc->proc_node = rmaps_node;
    proc->proc_name = *proc_name;
    proc->proc_rank = vpid;
    orte_ns.free_name(&proc_name);
    OBJ_RETAIN(proc); /* bump reference count for the node */
    opal_list_append(&rmaps_node->node_procs, &proc->super);
    map->procs[proc_index] = proc;
    
    /* Save this node on the map */
    opal_list_append(&map->nodes, &rmaps_node->super);
    
    /* Decrease the number of slots available for allocation
       on this node */
    --current_node->node_slots_alloc;
    return ORTE_SUCCESS;
}


/*
 * Create a default mapping for the application, scheduling round
 * robin by node.
 *
 * NOTE: This function assumes that the allocator has already setup
 * the list of nodes such that the sum of the node_slots_alloc fields
 * from all entries will be the total number of processes in all the
 * apps.
 */
static int map_app_by_node(
    orte_app_context_t* app, 
    orte_rmaps_base_map_t* map,
    orte_jobid_t jobid,
    orte_vpid_t vpid_start,
    int rank,
    opal_list_t* nodes)
{
    int rc;
    size_t num_alloc = 0;
    size_t proc_index = 0;
    opal_list_item_t *start, *next;
    orte_ras_base_node_t *node;
    bool did_alloc;

    /* Note that cur_node_item already points to the Right place in
       the node list to start looking (i.e., if this is the first time
       through, it'll point to the first item.  If this is not the
       first time through -- i.e., we have multiple app contexts --
       it'll point to where we left off last time.).  If we're at the
       end, bounce back to the front (as would happen in the loop
       below)

       But do a bozo check to ensure that we don't have a empty node
       list. */
    if (0 == opal_list_get_size(nodes)) {
        return ORTE_ERR_TEMP_OUT_OF_RESOURCE;
    } else if (opal_list_get_end(nodes) == cur_node_item) {
        cur_node_item = opal_list_get_first(nodes);
    }
    start = cur_node_item;

    /* This loop continues until all procs have been mapped or we run
       out of resources.  There are two definitions of "run out of
       resources":

       1. All nodes have node_slots processes mapped to them
       2. All nodes have node_slots_max processes mapped to them

       We first map until condition #1 is met.  If there are still
       processes that haven't been mapped yet, then we continue until
       condition #2 is met.  If we still have processes that haven't
       been mapped yet, then it's an "out of resources" error. */
    did_alloc = false;
    while (num_alloc < app->num_procs) {
        node = (orte_ras_base_node_t*) cur_node_item;
        next = opal_list_get_next(cur_node_item);

        /* If we have an available slot on this node, claim it */
        if (node->node_slots_alloc > 0) {
            fflush(stdout);
            rc = claim_slot(map, node, jobid, vpid_start + rank, proc_index);
            if (ORTE_SUCCESS != rc) {
                return rc;
            }
            if (node->node_slots_alloc == 0) {
                opal_list_remove_item(nodes, (opal_list_item_t*)node);
                OBJ_RELEASE(node);
            }

            ++rank;
            ++proc_index;

            /* Save the fact that we successfully allocated a process
               to a node in this round */
            did_alloc = true;

            /* Increase the number of procs allocated and see if we're
               done */
            ++num_alloc;
        }

        /* Move on to the next node */

        cur_node_item = next;
        if (opal_list_get_end(nodes) == cur_node_item) {
            cur_node_item = opal_list_get_first(nodes);
        }

        /* Are we done? */
        if (num_alloc == app->num_procs) {
            break;
        }

        /* Double check that the list is not empty */
        if (opal_list_get_end(nodes) == cur_node_item) {
            return ORTE_ERR_TEMP_OUT_OF_RESOURCE;
        }

        /* If we looped around without allocating any new processes,
           then we're full */
        if (start == cur_node_item) {
            if (!did_alloc) {
                return ORTE_ERR_TEMP_OUT_OF_RESOURCE;
            }
        }
    } 

    map->num_procs = num_alloc;
    return ORTE_SUCCESS;
}
   

/*
 * Create a default mapping for the application, scheduling one round
 * robin by slot.
 *
 * NOTE: This function assumes that the allocator has already setup
 * the list of nodes such that the sum of the node_slots_alloc fields
 * from all entries will be the total number of processes in all the
 * apps.
 */
static int map_app_by_slot(
    orte_app_context_t* app, 
    orte_rmaps_base_map_t* map,
    orte_jobid_t jobid,
    orte_vpid_t vpid_start,
    int rank,
    opal_list_t* nodes)
{
    int rc;
    size_t num_alloc = 0;
    size_t proc_index = 0;
    opal_list_item_t *next;
    orte_ras_base_node_t *node;

    /* Note that cur_node_item already points to the Right place in
       the node list to start looking (i.e., if this is the first time
       through, it'll point to the first item.  If this is not the
       first time through -- i.e., we have multiple app contexts --
       it'll point to where we left off last time.).   If we're at the
       end, bounce back to the front (as would happen in the loop
       below)

       But do a bozo check to ensure that we don't have a empty node
       list. */
    if (0 == opal_list_get_size(nodes)) {
        return ORTE_ERR_TEMP_OUT_OF_RESOURCE;
    } else if (opal_list_get_end(nodes) == cur_node_item) {
        cur_node_item = opal_list_get_first(nodes);
    }

    /* Go through all nodes and take up to node_slots_alloc slots and
       map it to this job */

    while (opal_list_get_end(nodes) != cur_node_item &&
           num_alloc < app->num_procs) {
        node = (orte_ras_base_node_t*) cur_node_item;
        next = opal_list_get_next(cur_node_item);

        /* If we have available slots on this node, claim it */
        while (node->node_slots_alloc > 0 &&
               num_alloc < app->num_procs) {
            fflush(stdout);
            rc = claim_slot(map, node, jobid, vpid_start + rank, proc_index);
            if (ORTE_SUCCESS != rc) {
                return rc;
            }
            ++rank;
            ++proc_index;

            /* Increase the number of procs allocated and see if we're
               done */
            ++num_alloc;
        }
        if (node->node_slots_alloc == 0) {
            opal_list_remove_item(nodes, (opal_list_item_t*)node);
            OBJ_RELEASE(node);
        }

        /* Move on to the next node */

        cur_node_item = next;
    } 

    /* Did we allocate everything? */

    if (num_alloc < app->num_procs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    map->num_procs = num_alloc;
    return ORTE_SUCCESS;
}
   

/*
 * Create a default mapping for the job.
 */

static int orte_rmaps_rr_map(orte_jobid_t jobid)
{
    orte_app_context_t** context;
    size_t i, num_context;
    opal_list_t nodes;
    opal_list_t mapping;
    opal_list_item_t* item;
    orte_vpid_t vpid_start;
    size_t num_procs = 0;
    int rank = 0;
    int rc = ORTE_SUCCESS;
    bool bynode = true;

    /* query for the application context and allocated nodes */
    if(ORTE_SUCCESS != (rc = orte_rmgr_base_get_app_context(jobid, &context, &num_context))) {
        return rc;
    }

    /* which policy should we use? */
    if (0 == strcmp(mca_rmaps_round_robin_component.schedule_policy, "node")) {
        bynode = true;
    } else {
        bynode = false;
    }

    /* total number of procs required */
    for(i=0; i<num_context; i++) {
        orte_app_context_t* app = context[i];
        num_procs += app->num_procs;
    }

    /* allocate a vpid range for the job */
    if(ORTE_SUCCESS != (rc = orte_ns.reserve_range(jobid, num_procs, &vpid_start))) {
        return rc;
    }

    /* query for all nodes allocated to this job */
    OBJ_CONSTRUCT(&nodes, opal_list_t);
    if(ORTE_SUCCESS != (rc = orte_ras_base_node_query_alloc(&nodes,jobid))) {
        OBJ_DESTRUCT(&nodes);
        return rc;
    }

    /* construct a default mapping */
    OBJ_CONSTRUCT(&mapping, opal_list_t);
    cur_node_item = opal_list_get_first(&nodes);
    for(i=0; i<num_context; i++) {
        orte_app_context_t* app = context[i];
        orte_rmaps_base_map_t* map = OBJ_NEW(orte_rmaps_base_map_t);
        if(NULL == map) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
        opal_list_append(&mapping, &map->super);

        map->app = app;
        map->procs = malloc(sizeof(orte_rmaps_base_proc_t*) * app->num_procs);
        if(NULL == map->procs) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
        if (bynode) {
            rc = map_app_by_node(app, map, jobid, vpid_start, rank, &nodes);
        } else {
            rc = map_app_by_slot(app, map, jobid, vpid_start, rank, &nodes);
        }
        if (ORTE_SUCCESS != rc) {
            goto cleanup;
        }
        rank += app->num_procs;
    }

    /* save mapping to the registry */
    if(ORTE_SUCCESS != (rc = orte_rmaps_base_set_map(jobid, &mapping))) {
        goto cleanup;
    }
    
    /* save vpid start/range on the job segment */
    rc = orte_rmaps_base_set_vpid_range(jobid,vpid_start,num_procs);

cleanup:
    while(NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);
    while(NULL != (item = opal_list_remove_first(&mapping))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&mapping);
    return rc;
}


static int orte_rmaps_rr_finalize(void)
{
    return ORTE_SUCCESS;
}


orte_rmaps_base_module_t orte_rmaps_round_robin_module = {
    orte_rmaps_rr_map,
    orte_rmaps_rr_finalize
};

