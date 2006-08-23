/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/argv.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/rmaps/base/rmaps_base_map.h"
#include "orte/mca/rmaps/base/rmaps_base_node.h"
#include "rmaps_rr.h"


/*
 * Local variable
 */
static opal_list_item_t *cur_node_item = NULL;
static opal_list_t fully_used_nodes;


/*
 * Create a default mapping for the application, scheduling round
 * robin by node.
 */
static int map_app_by_node(
    orte_app_context_t* app,
    orte_rmaps_base_map_t* map,
    orte_jobid_t jobid,
    orte_vpid_t vpid_start,
    opal_list_t* nodes,
    opal_list_t* max_used_nodes)
{
    int rc = ORTE_SUCCESS;
    orte_std_cntr_t num_alloc = 0;
    opal_list_item_t *next;
    orte_ras_node_t *node;
    

    /* This loop continues until all procs have been mapped or we run
       out of resources. We determine that we have "run out of
       resources" when all nodes have node_slots_max processes mapped to them,
       thus there are no free slots for a process to be mapped, or we have
       hit the soft limit on all nodes and are in a "no oversubscribe" state.
       If we still have processes that haven't been mapped yet, then it's an 
       "out of resources" error.

       In this scenario, we rely on the claim_slot function to handle the
       oversubscribed case. The claim_slot function will leave a node on the
       list until it either reachs node_slots_max OR reaches node_slots (the
       soft limit) and the "no_oversubscribe" flag has been set - at which point,
       the node will be removed to prevent any more processes from being mapped to
       it. Since we are taking one slot from each node as we cycle through, the
       list, oversubscription is automatically taken care of via this logic.
        */
    
    while (num_alloc < app->num_procs) {
        
        /** see if any nodes remain unused and available. We need to do this check
         * each time since we may remove nodes from the list (as they become fully
         * used) as we cycle through the loop */
        if(0 >= opal_list_get_size(nodes) ) {
            /* No more nodes to allocate :( */
            opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:alloc-error",
                           true, app->num_procs, app->app);
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        
        /* Save the next node we can use before claiming slots, since
         * we may need to prune the nodes list removing overused nodes.
         * Wrap around to beginning if we are at the end of the list */
        if (opal_list_get_end(nodes) == opal_list_get_next(cur_node_item)) {
            next = opal_list_get_first(nodes);
        }
        else {
            next = opal_list_get_next(cur_node_item);
        }
        
        /* Allocate a slot on this node */
        node = (orte_ras_node_t*) cur_node_item;
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(map, node, jobid, vpid_start + num_alloc, num_alloc,
                                             nodes, max_used_nodes))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        ++num_alloc;

        cur_node_item = next;
    }

    map->num_procs = num_alloc;

    return ORTE_SUCCESS;
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
    opal_list_t* nodes,
    opal_list_t* max_used_nodes)
{
    int rc = ORTE_SUCCESS;
    orte_std_cntr_t i, num_slots_to_take;
    orte_std_cntr_t num_alloc = 0;
    orte_ras_node_t *node;
    opal_list_item_t *next;


    /* This loop continues until all procs have been mapped or we run
       out of resources. We determine that we have "run out of
       resources" when either all nodes have node_slots_max processes mapped to them,
       (thus there are no free slots for a process to be mapped), OR all nodes
       have reached their soft limit and the user directed us to "no oversubscribe".
       If we still have processes that haven't been mapped yet, then it's an
       "out of resources" error. */
    num_alloc = 0;
    
    while ( num_alloc < app->num_procs) {

        /** see if any nodes remain unused and available. We need to do this check
        * each time since we may remove nodes from the list (as they become fully
        * used) as we cycle through the loop */
        if(0 >= opal_list_get_size(nodes) ) {
            /* No more nodes to allocate :( */
            opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:alloc-error",
                           true, app->num_procs, app->app);
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        
        /* Save the next node we can use before claiming slots, since
        * we may need to prune the nodes list removing overused nodes.
        * Wrap around to beginning if we are at the end of the list */
        if (opal_list_get_end(nodes) == opal_list_get_next(cur_node_item)) {
            next = opal_list_get_first(nodes);
        }
        else {
            next = opal_list_get_next(cur_node_item);
        }
        
        /** declare a shorter name for convenience in the code below */
        node = (orte_ras_node_t*) cur_node_item;
        
        /* If we have available slots on this node, claim all of them 
         * If node_slots == 0, assume 1 slot for that node. 
         * JJH - is this assumption fully justified?
         *
         * If we are now oversubscribing the nodes, then we still take
         * a full node_slots from each node until either everything is done,
         * or all nodes have hit their hard limit. This preserves the ratio
         * of processes between the nodes (e.g., if one node has twice as
         * many processes as another before oversubscribing, it will continue
         * to do so after oversubscribing).
         */
        num_slots_to_take = (node->node_slots == 0) ? 1 : node->node_slots;
        
        for( i = 0; i < num_slots_to_take; ++i) {
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(map, node, jobid, vpid_start + num_alloc, num_alloc,
                                                 nodes, max_used_nodes))) {
                /** if the code is ORTE_ERR_NODE_FULLY_USED, then we know this
                 * really isn't an error - we just need to break from the loop
                 * since the node is fully used up. For now, just don't report
                 * an error
                 */
                if (ORTE_ERR_NODE_FULLY_USED != rc) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
            
            /* Update the number of procs allocated */
            ++num_alloc;

            /** if all the procs have been mapped OR we have fully used up this node, then
             * break from the loop
             */
            if(num_alloc >= app->num_procs || ORTE_ERR_NODE_FULLY_USED == rc) {
                break;
            }
        }

        cur_node_item = next;

    }

    map->num_procs = num_alloc;

    return ORTE_SUCCESS;
}
   

/*
 * Create a round-robin mapping for the job.
 */

static int orte_rmaps_rr_map(orte_jobid_t jobid)
{
    orte_app_context_t** context, *app;
    orte_rmaps_base_map_t* map;
    orte_std_cntr_t i, num_context;
    opal_list_t master_node_list, mapped_node_list, max_used_nodes, *working_node_list;
    opal_list_t mapping;
    opal_list_item_t *item, *item2;
    orte_ras_node_t *node, *node2;
    orte_vpid_t vpid_start, job_vpid_start=0;
    orte_std_cntr_t num_procs = 0, total_num_slots, mapped_num_slots;
    int rc;
    bool bynode = true, modify_app_context = false;

    /* query for the application context and allocated nodes */
    if(ORTE_SUCCESS != (rc = orte_rmgr_base_get_app_context(jobid, &context, &num_context))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* which policy should we use? */
    if (0 == strcmp(mca_rmaps_round_robin_component.schedule_policy, "node")) {
        bynode = true;
    } else {
        bynode = false;
    }
    
    /* query for all nodes allocated to this job - this will become our master list of
     * nodes. From this, we will construct a working list of nodes based on any specified
     * mappings from the user
     */
    OBJ_CONSTRUCT(&master_node_list, opal_list_t);
    if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&master_node_list, jobid, &total_num_slots))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&master_node_list);
        return rc;
    }

    /* construct a mapping for the job - the list will hold mappings for each
     * application context
     */
    OBJ_CONSTRUCT(&mapping, opal_list_t);
    
    /** initialize the cur_node_item to point to the first node in the list */
    cur_node_item = opal_list_get_first(&master_node_list);
    
    /** construct the list to hold any nodes that get fully used during this
     * mapping. We need to keep a record of these so we can update their
     * information on the registry when we are done, but we want to remove
     * them from our master_node_list as we go so we don't keep checking to
     * see if we can still map something onto them.
     */
    OBJ_CONSTRUCT(&fully_used_nodes, opal_list_t);

    /** construct an intermediate list that will hold the nodes that are fully
     * used during any one pass through the mapper (i.e., for each app_context).
     * we will join the results together to form the fully_used_nodes list. This
     * allows us to more efficiently handle the cases where users specify
     * the proc-to-node mapping themselves.
     */
    OBJ_CONSTRUCT(&max_used_nodes, opal_list_t);

    /** construct a list to hold any nodes involved in a user-specified mapping */
    OBJ_CONSTRUCT(&mapped_node_list, opal_list_t);
    
    for(i=0; i<num_context; i++) {
        app = context[i];

        /** if the number of processes wasn't specified, then we know there can be only
        * one app_context allowed in the launch, and that we are to launch it across
        * all available slots. We'll double-check the single app_context rule first
        */
        if (0 == app->num_procs && 1 < num_context) {
            opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:multi-apps-and-zero-np",
                           true, num_context, NULL);
            ORTE_ERROR_LOG(ORTE_ERR_INVALID_NUM_PROCS);
            return ORTE_ERR_INVALID_NUM_PROCS;
        }

        /** create a map for this app_context */
        map = OBJ_NEW(orte_rmaps_base_map_t);
        if(NULL == map) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
        /** add it to the list of mappings for the job */
        opal_list_append(&mapping, &map->super);

        if ( 0 < app->num_map ) {
            /** If the user has specified a mapping for this app_context, then we
            * create a working node list that contains only those nodes.
            */
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_get_mapped_targets(&mapped_node_list, app, &master_node_list, &mapped_num_slots))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            working_node_list = &mapped_node_list;
           /* Set cur_node_item to point to the first node in the specified list to be used */
            cur_node_item = opal_list_get_first(working_node_list);
            
            if (0 == app->num_procs) {
               /** set the num_procs to equal the number of slots on these mapped nodes */
                app->num_procs = mapped_num_slots;
                modify_app_context = true;
            }
        }
        else {
            /** no mapping was specified, so we are going to just use everything that was
             * allocated to us. We don't need to update cur_node_item in this case since it
             * is always pointing to something in the master_node_list - we'll just pick up
             * from wherever we last stopped.
             */
            working_node_list = &master_node_list;
            
            if (0 == app->num_procs) {
                /** set the num_procs to equal the number of slots on all available nodes */
                app->num_procs = total_num_slots;
                modify_app_context = true;
            }
        }


        map->app = app;
        map->procs = (orte_rmaps_base_proc_t**)malloc(sizeof(orte_rmaps_base_proc_t*) * app->num_procs);
        if(NULL == map->procs) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }

        /* allocate a vpid range for this app within the job */
        if(ORTE_SUCCESS != (rc = orte_ns.reserve_range(jobid, app->num_procs, &vpid_start))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&master_node_list);
            return rc;
        }
        
        /** save the initial starting vpid for later */
        if (0 == i) {
            job_vpid_start = vpid_start;
        }
        
        /** track the total number of processes we mapped */
        num_procs += app->num_procs;

        /* Make assignments */
        if (bynode) {
            rc = map_app_by_node(app, map, jobid, vpid_start, working_node_list, &max_used_nodes);
        } else {
            rc = map_app_by_slot(app, map, jobid, vpid_start, working_node_list, &max_used_nodes);
        }

        
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /** cleanup the mapped_node_list, if necessary */
        if (0 < app->num_map) {
            /** before we get rid of the mapped_node_list, we first need to update
            * corresponding entries in the master_node_list so we accurately
            * track the usage of slots. Also, any node that was "used up" will have
            * been removed from the mapped_node_list - we now also must ensure that
            * such a node is removed from the master_node_list.
            *
            * Clearly, there will be a performance penalty in doing all these
            * operations to maintain data integrity. However, the case where
            * someone maps processes this specifically is considered the
            * atypical one, so penalizing it may not be a major issue.
            *
            * Still, some effort to improve the efficiency of this process
            * may be in order for the future.
            */
            while (NULL != (item = opal_list_remove_first(&mapped_node_list))) {
                node = (orte_ras_node_t*)item;

                /** if the node was still on the mapped_node_list, then it hasn't
                 * been moved to the fully_used_node list - find it on the
                 * master_node_list and update the slots_inuse count there
                 */
                for (item2  = opal_list_get_first(&master_node_list);
                     item2 != opal_list_get_end(&master_node_list);
                     item2  = opal_list_get_next(item2) ) {
                    node2 = (orte_ras_node_t*)item2;

                    if (0 == strcmp(node2->node_name, node->node_name)) {
                        node2->node_slots_inuse = node->node_slots_inuse;
                        break;
                    }
                }
                OBJ_RELEASE(item);
            }

            /** that updated everything that wasn't fully used up while
             * processing the specific map. Now we have to ensure that
             * any nodes that were used up (and hence, transferred to the
             * max_used_node list) are removed from the master_node_list
             * No really nice way to do this - we just have to run through
             * the two lists and remove any duplicates.
             */
            while (NULL != (item = opal_list_remove_first(&max_used_nodes))) {
                node = (orte_ras_node_t*)item;
                
                for (item2  = opal_list_get_first(&master_node_list);
                     item2 != opal_list_get_end(&master_node_list);
                     item2  = opal_list_get_next(item2) ) {
                    node2 = (orte_ras_node_t*)item2;

                    /** if we have a match, then remove the entry from the
                     * master_node_list
                     */
                    if (0 == strcmp(node2->node_name, node->node_name)) {
                        opal_list_remove_item(&master_node_list, item2);
                        break;
                    }
                }
                
                /** now put that node on the fully_used_nodes list */
                opal_list_append(&fully_used_nodes, &node->super);
            }

        } else {
            /** this mapping wasn't specified, so all we have to do is add any nodes
             * that were used up in the mapping to the fully_used_nodes list - they
             * were already removed from the master_node_list when we did the mapping.
             */
            opal_list_join(&fully_used_nodes, opal_list_get_end(&fully_used_nodes), &max_used_nodes);
        }
        
    }

    /* save mapping to the registry */
    if(ORTE_SUCCESS != (rc = orte_rmaps_base_set_map(jobid, &mapping))) {
        goto cleanup;
    }
    
    /* save vpid start/range on the job segment */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_set_vpid_range(jobid, job_vpid_start, num_procs))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /** join the master_node_list and fully_used_list so that all info gets updated */
    opal_list_join(&master_node_list, opal_list_get_end(&master_node_list), &fully_used_nodes);

    /** save the modified node information so we can start from the right
     * place next time through
    */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_update_node_usage(&master_node_list))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /** if the app_context was modified, update that information too. This can only happen
        for the case where num_context=1 and the user didn't specify the number of
        processes
     */
    if (modify_app_context) {
        if (ORTE_SUCCESS != (rc = orte_rmgr_base_put_app_context(jobid, context, 1))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    

cleanup:
    while(NULL != (item = opal_list_remove_first(&master_node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&master_node_list);

    while(NULL != (item = opal_list_remove_first(&mapping))) {
        OBJ_RELEASE(item);
    }

    OBJ_DESTRUCT(&mapping);
    OBJ_DESTRUCT(&max_used_nodes);
    OBJ_DESTRUCT(&fully_used_nodes);
    OBJ_DESTRUCT(&mapped_node_list);

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

