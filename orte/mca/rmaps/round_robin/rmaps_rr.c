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
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/argv.h"
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

/*
 * A sanity check to ensure that all of the requested nodes are actually
 * allocated to this application.
 */
static bool are_all_mapped_valid(char **mapping,
                                 int num_mapped,
                                 opal_list_t* nodes)
{
    opal_list_item_t *item;
    int i;
    bool matched;

    for (i = 0; i < num_mapped; ++i) {
        matched = false;

        for(item  = opal_list_get_first(nodes);
            item != opal_list_get_end(nodes);
            item  = opal_list_get_next(item) ) {
            if( 0 == strcmp( ((orte_ras_node_t*) item)->node_name, mapping[i]) ) {
                matched = true;
                break;
            }
        }

        /* If we find one requested resource that is not allocated, 
         * then return an error */
        if(!matched) {
            return false;
        }
    }

    return true;
}

/*
 * If the node in question is in the current mapping.
 */
static bool is_mapped(opal_list_item_t *item, 
                      char **mapping, 
                      int num_mapped, 
                      opal_list_t* nodes) 
{
    int i;

    for ( i = 0; i < num_mapped; ++i) {
        if ( 0 == strcmp( ((orte_ras_node_t*) item)->node_name, mapping[i])){
            return true;
        }
    }

    return false;
}

/*
 * Return a point to the next node allocated, included in the mapping.
 */
static opal_list_item_t* get_next_mapped(opal_list_item_t *node_item, 
                                         char **mapping, 
                                         int num_mapped, 
                                         opal_list_t* nodes) 
{
    opal_list_item_t *item;

    /* Wrap around to beginning if we are at the end of the list */
    if (opal_list_get_end(nodes) == opal_list_get_next(node_item)) {
        item = opal_list_get_first(nodes);
    }
    else {
        item = opal_list_get_next(node_item);
    }
    
    do {
        /* See if current node is in the mapping and contains slots */
        if( is_mapped(item, mapping, num_mapped, nodes) ) {
            return item;
        }

        /*
         * We just rechecked the current item and concluded that 
         * it wasn't in the list, thus the list contains no matches
         * in this mapping. Return an error.
         */
        if(node_item == item){
            return NULL;
        }
        
        /* Access next item in Round Robin Manner */
        if (opal_list_get_end(nodes) ==  opal_list_get_next(item)) {
            item = opal_list_get_first(nodes);
        }
        else {
            item = opal_list_get_next(item);
        }

    } while( true );
}

static int claim_slot(orte_rmaps_base_map_t *map, 
                      orte_ras_node_t *current_node,
                      orte_jobid_t jobid, 
                      orte_vpid_t vpid, 
                      int proc_index)
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

    OBJ_RETAIN(current_node);
    rmaps_node->node = current_node;
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
    
    /* Be sure to demarcate this slot claim for the node */
    current_node->node_slots_inuse++;

    return ORTE_SUCCESS;
}


/*
 * Create a default mapping for the application, scheduling round
 * robin by node.
 */
static int map_app_by_node(
    orte_app_context_t* app,
    orte_rmaps_base_map_t* map,
    orte_jobid_t jobid,
    orte_vpid_t vpid_start,
    int rank,
    opal_list_t* nodes,
    char **mapped_nodes,
    int num_mapped_nodes)
{
    int rc = ORTE_SUCCESS;
    size_t num_alloc = 0;
    size_t proc_index = 0;
    opal_list_item_t *next;
    orte_ras_node_t *node;

    /* This loop continues until all procs have been mapped or we run
       out of resources. We determine that we have "run out of
       resources" when all nodes have node_slots_max processes mapped to them,
       thus there are no free slots for a process to be mapped.
       If we still have processes that haven't  been mapped yet, then it's an 
       "out of resources" error. */
    while (num_alloc < app->num_procs) {
        node = (orte_ras_node_t*) cur_node_item;
        
        /* Find the next node we can use before claiming slots, since
         * we may need to prune the nodes list removing overused nodes */
        if ( 0 < app->num_map ) {
            next = get_next_mapped(cur_node_item, mapped_nodes, num_mapped_nodes, nodes);
            if (NULL == next ) {
                /* Not allocated anything */
                opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:no-mapped-node",
                               true, app->app, opal_argv_join(mapped_nodes, ','));
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                goto cleanup;
            }
        }
        else {
            if (opal_list_get_end(nodes) ==  opal_list_get_next(cur_node_item)) {
                next = opal_list_get_first(nodes);
            }
            else {
                next = opal_list_get_next(cur_node_item);
            }
        }

        /* Remove this node if it has reached its max number of allocatable slots */
        if( 0 != node->node_slots_max  &&
            node->node_slots_inuse > node->node_slots_max) {
            opal_list_remove_item(nodes, (opal_list_item_t*)node);
            if(0 >= opal_list_get_size(nodes) ) {
                /* No more nodes to allocate :( */
                opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:alloc-error",
                               true, app->num_procs, app->app);
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                goto cleanup;
            }
        }
        /* Allocate this node */
        else {
            rc = claim_slot(map, node, jobid, vpid_start + rank, proc_index);
            if (ORTE_SUCCESS != rc) {
                goto cleanup;
            }

            ++rank;
            ++proc_index;
            ++num_alloc;
        }

        cur_node_item = next;
    }

    map->num_procs = num_alloc;

 cleanup:

    return rc;
}
   

/*
 * Create a default mapping for the application, scheduling one round
 * robin by slot.
 */
static int map_app_by_slot(
    orte_app_context_t* app,
    orte_rmaps_base_map_t* map,
    orte_jobid_t jobid,
    orte_vpid_t vpid_start,
    int rank,
    opal_list_t* nodes,
    char **mapped_nodes,
    int num_mapped_nodes)
{
    int rc = ORTE_SUCCESS;
    size_t i;
    size_t num_alloc = 0;
    size_t proc_index = 0;
    orte_ras_node_t *node;
    opal_list_item_t *start, *next;
    bool oversubscribe;


    /* This loop continues until all procs have been mapped or we run
       out of resources. We determine that we have "run out of
       resources" when all nodes have node_slots_max processes mapped to them,
       thus there are no free slots for a process to be mapped.
       If we still have processes that haven't  been mapped yet, then it's an
       "out of resources" error. */
    num_alloc = 0;
    start = cur_node_item;
    oversubscribe = false;
    while ( num_alloc < app->num_procs) {
        node = (orte_ras_node_t*) cur_node_item;
        
        /* Find the next node we can use before claiming slots, since
         * we may need to prune the nodes list removing over used nodes */
        if ( 0 < app->num_map ) {
            next = get_next_mapped(cur_node_item, mapped_nodes, num_mapped_nodes, nodes);
            if (NULL == next ) {
                /* Not allocated anything */
                opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:no-mapped-node",
                               true, app->app, opal_argv_join(mapped_nodes, ','));
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                goto cleanup;
            }
        }
        else {
            if (opal_list_get_end(nodes) ==  opal_list_get_next(cur_node_item)) {
                next = opal_list_get_first(nodes);
            }
            else {
                next = opal_list_get_next(cur_node_item);
            }
        }

        /* If we have available slots on this node, claim all of them 
         * If node_slots == 0, assume 1 slot for that node. 
         * JJH - is this assumption fully justified? */
        for( i = 0; i < ((node->node_slots == 0) ? 1 : node->node_slots); ++i) {
            /* If we are not oversubscribing, and this node is full, skip it. */
            if( !oversubscribe && 
                0 != node->node_slots &&
                node->node_slots_inuse > node->node_slots) {
                break;
            }
            /* If this node has reached its max number of slots, 
             * take it out of the list, and skip it */
            else if( 0 != node->node_slots_max  && 
                     node->node_slots_inuse > node->node_slots_max){
                opal_list_remove_item(nodes, (opal_list_item_t*)node);
                if( 0 >= opal_list_get_size(nodes) ) {
                    /* No more nodes to allocate */
                    opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:alloc-error",
                                   true, app->num_procs, app->app);
                    rc = ORTE_ERR_OUT_OF_RESOURCE;
                    goto cleanup;
                }
                break;
            }
            
            rc = claim_slot(map, node, jobid, vpid_start + rank, proc_index);
            if (ORTE_SUCCESS != rc) {
                goto cleanup;
            }
                        
            /* Increase the number of procs allocated */
            ++num_alloc;
            ++rank;
            ++proc_index;
            if(num_alloc >= app->num_procs) {
                break;
            }
        }

        cur_node_item = next;

        /* Since we have now looped back around, go ahead and oversubscribe nodes */
        if(start == cur_node_item) {
            oversubscribe = true;
        }
    }

    map->num_procs = num_alloc;

 cleanup:

    return rc;
}
   

/*
 * Create a default mapping for the job.
 */

static int orte_rmaps_rr_map(orte_jobid_t jobid)
{
    orte_app_context_t** context, *app;
    orte_rmaps_base_map_t* map;
    size_t i, j, k, num_context;
    opal_list_t nodes;
    opal_list_t mapping;
    opal_list_item_t* item;
    orte_vpid_t vpid_start;
    size_t num_procs = 0;
    int rank = 0;
    int rc = ORTE_SUCCESS;
    bool bynode = true;
    char **mapped_nodes = NULL;
    int  num_mapped_nodes = 0;

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

    /* query for all nodes allocated to this job */
    OBJ_CONSTRUCT(&nodes, opal_list_t);
    if(ORTE_SUCCESS != (rc = orte_ras_base_node_query_alloc(&nodes, jobid))) {
        OBJ_DESTRUCT(&nodes);
        return rc;
    }

    /* Sanity check to make sure we have been allocated nodes */
    if (0 == opal_list_get_size(&nodes)) {
        OBJ_DESTRUCT(&nodes);
        return ORTE_ERR_TEMP_OUT_OF_RESOURCE;
    }

    /* Total number of procs required 
     * DEVEL NOTE: Need to extend this when implementing C/N notation
     *   Will need to set the app->num_procs approprately before this,
     *   Since we will have the allocated node information at this point.
     */
    for(i=0; i<num_context; i++) {
        app = context[i];
        num_procs += app->num_procs;
    }

    /* allocate a vpid range for the job */
    if(ORTE_SUCCESS != (rc = orte_ns.reserve_range(jobid, num_procs, &vpid_start))) {
        return rc;
    }


    /* construct a default mapping by application */
    OBJ_CONSTRUCT(&mapping, opal_list_t);
    cur_node_item = opal_list_get_first(&nodes);
    for(i=0; i<num_context; i++) {
        app = context[i];
        
        map = OBJ_NEW(orte_rmaps_base_map_t);
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

        /* Extract the requested mapping for this application */
        
        /* Note that cur_node_item already points to the Right place in
           the node list to start looking (i.e., if this is the first time
           through, it'll point to the first item.  If this is not the
           first time through -- i.e., we have multiple app contexts --
           it'll point to where we left off last time.).  If we're at the
           end, bounce back to the front (as would happen in the loop
           below)
           
           But do a bozo check to ensure that we don't have a empty node list.*/
        if (0 == opal_list_get_size(&nodes)) {
            rc = ORTE_ERR_TEMP_OUT_OF_RESOURCE;
            goto cleanup;
        } else if (opal_list_get_end(&nodes) == cur_node_item) {
            cur_node_item = opal_list_get_first(&nodes);
        }

        /* If this application has a mapping then 
         * - if the current node is in the mapping, use it
         * - ow get the next node in that mapping.
         */
        if ( 0 < app->num_map ) {
            orte_app_context_map_t** loc_map = app->map_data;
            
            /* Accumulate all of the host name mappings */
            for(k = 0; k < app->num_map; ++k) {
                if ( ORTE_APP_CONTEXT_MAP_HOSTNAME == loc_map[k]->map_type ) {
                    if(mapped_nodes == NULL) {
                        mapped_nodes     = opal_argv_split(loc_map[k]->map_data, ',');
                        num_mapped_nodes = opal_argv_count(mapped_nodes);
                    }
                    else { /* Append to the existing mapping */
                        char ** mini_map    = opal_argv_split(loc_map[k]->map_data, ',');
                        size_t mini_num_map = opal_argv_count(mini_map);
                        for (j = 0; j < mini_num_map; ++j) {
                            rc = opal_argv_append(&num_mapped_nodes, &mapped_nodes, mini_map[j]);
                            if (OPAL_SUCCESS != rc) {
                                goto cleanup;
                            }
                        }
                        opal_argv_free(mini_map);
                    }
                }
            }
            
            if( !are_all_mapped_valid(mapped_nodes, num_mapped_nodes, &nodes) ) {
                opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:not-all-mapped-alloc",
                               true, app->app, opal_argv_join(mapped_nodes, ','));
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                goto cleanup;
            }
            
            /* If the current node is not in the current mapping
             * Then get the next node that is in the mapping */
            if( !is_mapped(cur_node_item, mapped_nodes, num_mapped_nodes, &nodes) ) {
                cur_node_item = get_next_mapped(cur_node_item, mapped_nodes, num_mapped_nodes, &nodes);
                if( NULL == cur_node_item) {
                    opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:no-mapped-node",
                                   true, app->app, opal_argv_join(mapped_nodes, ','));
                    rc = ORTE_ERR_OUT_OF_RESOURCE;
                    goto cleanup;
                }
            }
        }
        
        /* Make assignments */
        if (bynode) {
            rc = map_app_by_node(app, map, jobid, vpid_start, rank, &nodes, mapped_nodes, num_mapped_nodes);
        } else {
            rc = map_app_by_slot(app, map, jobid, vpid_start, rank, &nodes, mapped_nodes, num_mapped_nodes);
        }
        if (ORTE_SUCCESS != rc) {
            goto cleanup;
        }
        
        rank += app->num_procs;
        opal_argv_free(mapped_nodes);
        mapped_nodes = NULL;
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

    if( NULL != mapped_nodes ) {
        opal_argv_free(mapped_nodes);
    }

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

