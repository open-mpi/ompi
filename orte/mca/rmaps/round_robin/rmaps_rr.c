/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
#include "opal/util/trace.h"
#include "opal/util/show_help.h"
#include "opal/util/argv.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rmgr/rmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "rmaps_rr.h"


/*
 * Local variable
 */
static opal_list_item_t *cur_node_item = NULL;
static opal_list_t fully_used_nodes;
static orte_std_cntr_t num_per_node;


/*
 * Create a default mapping for the application, scheduling round
 * robin by node.
 */
static int map_app_by_node(
    orte_app_context_t* app,
    orte_job_map_t* map,
    orte_jobid_t jobid,
    orte_vpid_t vpid_start,
    opal_list_t* nodes,
    opal_list_t* max_used_nodes)
{
    int rc = ORTE_SUCCESS;
    orte_std_cntr_t num_alloc = 0;
    opal_list_item_t *next;
    orte_ras_node_t *node;

    OPAL_TRACE(2);
    
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
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(map, node, jobid, vpid_start + num_alloc, app->idx,
                                             nodes, max_used_nodes,
                                             mca_rmaps_round_robin_component.oversubscribe))) {
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

        ++num_alloc;

        cur_node_item = next;
    }

    return ORTE_SUCCESS;
}
   

/*
 * Create a default mapping for the application, scheduling one round
 * robin by slot.
 */
static int map_app_by_slot(
    orte_app_context_t* app,
    orte_job_map_t* map,
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

    OPAL_TRACE(2);
    
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
            /* Everything is at max usage! :( */
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
         * If we are now oversubscribing the nodes, then we still take:
         * (a) if the node has not been used yet, we take a full node_slots
         * (b) if some of the slots are in-use, then we take the number of
         *     remaining slots before hitting the soft limit (node_slots)
         * (c) if we are at or above the soft limit, we take a full node_slots
         *
         * Note: if node_slots is zero, then we always just take 1 slot
         *
         * We continue this process until either everything is done,
         * or all nodes have hit their hard limit. This algorithm ensures we
         * fully utilize each node before oversubscribing, and preserves the ratio
         * of processes between the nodes thereafter (e.g., if one node has twice as
         * many processes as another before oversubscribing, it will continue
         * to do so after oversubscribing).
         */
        if (0 == node->node_slots_inuse ||
            node->node_slots_inuse >= node->node_slots) {
            num_slots_to_take = (node->node_slots == 0) ? 1 : node->node_slots;
        } else {
            num_slots_to_take = node->node_slots - node->node_slots_inuse;
        }
        
        /* check if we are in npernode mode - if so, then set the num_slots_to_take
         * to the num_per_node
         */
        if (mca_rmaps_round_robin_component.n_per_node) {
            num_slots_to_take = num_per_node;
        }
        
        for( i = 0; i < num_slots_to_take; ++i) {
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(map, node, jobid, vpid_start + num_alloc, app->idx,
                                                 nodes, max_used_nodes,
                                                 mca_rmaps_round_robin_component.oversubscribe))) {
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
            if(num_alloc == app->num_procs || ORTE_ERR_NODE_FULLY_USED == rc) {
                break;
            }
        }

        /* we move on to the next node in all cases EXCEPT if we came
         * out of the loop without having taken a full bite AND the
         * node is NOT max'd out
         *
         */
        if (i < (num_slots_to_take-1) && ORTE_ERR_NODE_FULLY_USED != rc) {
            continue;
        }
        cur_node_item = next;
    }

    return ORTE_SUCCESS;
}
   

/*
 * Process the attributes and push them into our local "global"
 */
static int orte_rmaps_rr_process_attrs(opal_list_t *attributes)
{
    int rc;
    char *policy;
    orte_attribute_t *attr;
    orte_std_cntr_t *scptr;
    bool policy_override;
    
    mca_rmaps_round_robin_component.bynode = false;  /* set default mapping policy to byslot*/
    policy_override = false;

    mca_rmaps_round_robin_component.per_node = false;
    if (NULL != (attr = orte_rmgr.find_attribute(attributes, ORTE_RMAPS_PERNODE))) {
        /* was provided - set boolean accordingly */
         mca_rmaps_round_robin_component.per_node = true;
        /* indicate that we are going to map this job bynode */
        mca_rmaps_round_robin_component.bynode = true;
        /* indicate that this is to *be* the policy no matter what */
        policy_override = true;
    }
    
    mca_rmaps_round_robin_component.n_per_node = false;
    if (NULL != (attr = orte_rmgr.find_attribute(attributes, ORTE_RMAPS_N_PERNODE))) {
        /* was provided - set boolean accordingly */
        mca_rmaps_round_robin_component.n_per_node = true;
        /* get the number of procs per node to launch */
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&scptr, attr->value, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        num_per_node = *scptr;
        /* default to byslot mapping */
        mca_rmaps_round_robin_component.bynode = false;
    }
    
    /* define the mapping policy. This *must* come after we process the pernode
     * options since those set a default mapping policy - we want to be able
     * to override that setting if requested
     *
     * NOTE: we don't do this step if the policy_override has been set!
     */
    if (!policy_override &&
        NULL != (attr = orte_rmgr.find_attribute(attributes, ORTE_RMAPS_MAP_POLICY))) {
        /* they specified a mapping policy - extract its name */
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&policy, attr->value, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (0 == strcmp(policy, "bynode")) {
            mca_rmaps_round_robin_component.bynode = true;
        } else {
            mca_rmaps_round_robin_component.bynode = false;
        }
    }
    
    mca_rmaps_round_robin_component.no_use_local = false;
    if (NULL != (attr = orte_rmgr.find_attribute(attributes, ORTE_RMAPS_NO_USE_LOCAL))) {
        /* was provided - set boolean accordingly */
        mca_rmaps_round_robin_component.no_use_local = true;
    }
    
    mca_rmaps_round_robin_component.oversubscribe = true;
    if (NULL != (attr = orte_rmgr.find_attribute(attributes, ORTE_RMAPS_NO_OVERSUB))) {
        /* was provided - set boolean accordingly */
        mca_rmaps_round_robin_component.oversubscribe = false;
    }
    
    mca_rmaps_round_robin_component.no_allocate_range = false;
    if (NULL != (attr = orte_rmgr.find_attribute(attributes, ORTE_RMAPS_NO_ALLOC_RANGE))) {
        /* was provided - set boolean accordingly */
        mca_rmaps_round_robin_component.no_allocate_range = true;
    }
    
    return ORTE_SUCCESS;
}
/*
 * Create a round-robin mapping for the job.
 */

static int orte_rmaps_rr_map(orte_jobid_t jobid, opal_list_t *attributes)
{
    orte_app_context_t *app;
    orte_job_map_t* map;
    orte_std_cntr_t i;
    opal_list_t master_node_list, mapped_node_list, max_used_nodes, *working_node_list;
    opal_list_item_t *item, *item2;
    orte_ras_node_t *node, *node2;
    orte_mapped_node_t *mnode;
    char *save_bookmark;
    orte_vpid_t vpid_start;
    orte_std_cntr_t num_procs = 0, total_num_slots, mapped_num_slots, num_nodes, num_slots;
    int rc;
    bool modify_app_context = false;
    char *sptr;
    orte_attribute_t *attr;
    orte_std_cntr_t slots_per_node;

    OPAL_TRACE(1);
    
    /* setup the local environment from the attributes */
    if (ORTE_SUCCESS != (rc = orte_rmaps_rr_process_attrs(attributes))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* create the map object */
    map = OBJ_NEW(orte_job_map_t);
    if (NULL == map) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* set the jobid */
    map->job = jobid;
    
    /* query for the application context and allocated nodes */
    if(ORTE_SUCCESS != (rc = orte_rmgr.get_app_context(jobid, &(map->apps), &(map->num_apps)))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* query for all nodes allocated to this job - this will become our master list of
     * nodes. From this, we will construct a working list of nodes based on any specified
     * mappings from the user
     */
    OBJ_CONSTRUCT(&master_node_list, opal_list_t);
    if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&master_node_list, jobid,
                                                              &total_num_slots,
                                                              mca_rmaps_round_robin_component.no_use_local))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&master_node_list);
        return rc;
    }

    /* if a bookmark exists from some prior mapping, set us to start there */
    if (NULL != (attr = orte_rmgr.find_attribute(attributes, ORTE_RMAPS_BOOKMARK))) {
        cur_node_item = NULL;
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, attr->value, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* find this node on the master list */
        for (item = opal_list_get_first(&master_node_list);
             item != opal_list_get_end(&master_node_list);
             item = opal_list_get_next(item)) {
            node = (orte_ras_node_t*)item;
            
            if (0 == strcmp(sptr, node->node_name)) {
                cur_node_item = item;
                break;
            }
        }
        /* see if we found it - if not, just start at the beginning */
        if (NULL == cur_node_item) {
            cur_node_item = opal_list_get_first(&master_node_list); 
        }
    } else {
        /* if no bookmark, then just start at the beginning of the list */
        cur_node_item = opal_list_get_first(&master_node_list);
    }
    
    /* save the node name for the bookmark just in case we don't do anything
     * useful down below
     */
    save_bookmark = strdup(((orte_ras_node_t*)cur_node_item)->node_name);
    
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
    
    for(i=0; i < map->num_apps; i++) {
        app = map->apps[i];

        /** if the number of processes wasn't specified, then we know there can be only
        * one app_context allowed in the launch, and that we are to launch it across
        * all available slots. We'll double-check the single app_context rule first
        */
        if (0 == app->num_procs && 1 < map->num_apps) {
            opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:multi-apps-and-zero-np",
                           true, map->num_apps, NULL);
            ORTE_ERROR_LOG(ORTE_ERR_INVALID_NUM_PROCS);
            return ORTE_ERR_INVALID_NUM_PROCS;
        }

        if ( 0 < app->num_map ) {
            /** If the user has specified a mapping for this app_context, then we
            * create a working node list that contains only those nodes.
            */
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_get_mapped_targets(&mapped_node_list, app,
                                                                         &master_node_list, &mapped_num_slots))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            working_node_list = &mapped_node_list;
           /* Set cur_node_item to point to the first node in the specified list to be used */
            cur_node_item = opal_list_get_first(working_node_list);
            
            num_nodes = (orte_std_cntr_t)opal_list_get_size(&mapped_node_list);
            num_slots = (orte_std_cntr_t)mapped_num_slots;
        }
        else {
            /** no mapping was specified, so we are going to just use everything that was
             * allocated to us. We don't need to update cur_node_item in this case since it
             * is always pointing to something in the master_node_list - we'll just pick up
             * from wherever we last stopped.
             */
            working_node_list = &master_node_list;
            
            num_nodes = (orte_std_cntr_t)opal_list_get_size(&master_node_list);
            num_slots = total_num_slots;
        }

        if (mca_rmaps_round_robin_component.per_node) {
            /* there are three use-cases that we need to deal with:
            * (a) if -np was not provided, then we just use the number of nodes
            * (b) if -np was provided AND #procs > #nodes, then error out
            * (c) if -np was provided AND #procs <= #nodes, then launch
            *     the specified #procs one/node. In this case, we just
            *     leave app->num_procs alone
            */
            if (0 == app->num_procs) {
                app->num_procs = num_nodes;
                modify_app_context = true;
            } else if (app->num_procs > num_nodes) {
                opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:per-node-and-too-many-procs",
                               true, app->num_procs, num_nodes, NULL);
                return ORTE_ERR_SILENT;
            }
        } else if (mca_rmaps_round_robin_component.n_per_node) {
            /* first, let's check to see if there are enough slots/node to
             * meet the request - error out if not
             */
            slots_per_node = num_slots / num_nodes;
            if (num_per_node > slots_per_node) {
                opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:n-per-node-and-not-enough-slots",
                               true, num_per_node, slots_per_node, NULL);
                return ORTE_ERR_SILENT;
            }
            /* there are three use-cases that we need to deal with:
            * (a) if -np was not provided, then we just use the n/node * #nodes
            * (b) if -np was provided AND #procs > (n/node * #nodes), then error out
            * (c) if -np was provided AND #procs <= (n/node * #nodes), then launch
            *     the specified #procs n/node. In this case, we just
            *     leave app->num_procs alone
            */
            if (0 == app->num_procs) {
                /* set the num_procs to equal the specified num/node * the number of nodes */
                app->num_procs = num_per_node * num_nodes;
                modify_app_context = true;
            } else if (app->num_procs > (num_per_node * num_nodes)) {
                opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:n-per-node-and-too-many-procs",
                               true, app->num_procs, num_per_node, num_nodes, num_slots, NULL);
                return ORTE_ERR_SILENT;
            }
        } else if (0 == app->num_procs) {
            /** set the num_procs to equal the number of slots on these mapped nodes - if
            user has specified "-bynode", then set it to the number of nodes
            */
            if (mca_rmaps_round_robin_component.bynode) {
                app->num_procs = num_nodes;
            } else {
                app->num_procs = num_slots;
            }
            modify_app_context = true;
        }

        /* allocate a vpid range for this app within the job, unless told not to do so */
        if (mca_rmaps_round_robin_component.no_allocate_range) {
            vpid_start = 0;
        } else {
            if(ORTE_SUCCESS != (rc = orte_ns.reserve_range(jobid, app->num_procs, &vpid_start))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&master_node_list);
                return rc;
            }
        }
        
        /** save the initial starting vpid for later */
        if (0 == i) {
            map->vpid_start = vpid_start;
        }
        
        /** track the total number of processes we mapped */
        num_procs += app->num_procs;

        /* Make assignments */
        if (mca_rmaps_round_robin_component.bynode) {
            map->mapping_mode = strdup("bynode");
            rc = map_app_by_node(app, map, jobid, vpid_start, working_node_list, &max_used_nodes);
        } else {
            map->mapping_mode = strdup("byslot");
            rc = map_app_by_slot(app, map, jobid, vpid_start, working_node_list, &max_used_nodes);
        }

        
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* save the next node name bookmark as we will - in the case of mapped nodes -
         * release the node information being pointed to by cur_node_item
         */
        if(NULL != cur_node_item) {
            free(save_bookmark);
            save_bookmark = strdup(((orte_ras_node_t*)cur_node_item)->node_name);
        }

        /** cleanup the mapped_node_list, if necessary */
        if (0 < app->num_map) {
            /* we need to adjust our bookmark so it points to the node in the
             * master node list - this allows the cur_node_item to "survive"
             * the disassembly of the mapped_node_list
             */
            if (NULL != cur_node_item) {
                node = (orte_ras_node_t*)cur_node_item;
                /* This can be a little tricky due to all the corner
                 * cases. If the mapped_node_list only has ONE entry on it, then the
                 * cur_node_item will always point at it, even if we used everything
                 * on that node. What we will do, therefore, is check the usage of the
                 * cur_node_item to see if it has reached the soft limit. If so, we find
                 * the node after that one on the master node list
                 */
                for (item = opal_list_get_first(&master_node_list);
                     item != opal_list_get_end(&master_node_list);
                     item = opal_list_get_next(item)) {
                    node2 = (orte_ras_node_t*)item;
                    if (0 == strcmp(node->node_name, node2->node_name)) {
                        if (node->node_slots <= node->node_slots_inuse) {
                            /* we are at or beyond the soft limit */
                            cur_node_item = opal_list_get_next(item);
                        } else {
                            cur_node_item = item;
                        }
                        break;
                    }
                }
            }
            
            /* as we get rid of the mapped_node_list, we need to update
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
            *
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
                     * master_node_list. if that entry was our bookmark,
                     * shift the bookmark to the next entry on the list
                     */
                    if (0 == strcmp(node2->node_name, node->node_name)) {
                        if (0 == strcmp(node->node_name,
                                        ((orte_ras_node_t*)cur_node_item)->node_name)) {
                            cur_node_item = opal_list_get_next(item2);
                        }
                        opal_list_remove_item(&master_node_list, item2);
                        OBJ_RELEASE(item2);
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

    /* compute and save convenience values */
    map->vpid_range = num_procs;
    map->num_nodes = opal_list_get_size(&map->nodes);
    for (item = opal_list_get_first(&map->nodes);
         item != opal_list_get_end(&map->nodes);
         item = opal_list_get_next(item)) {
        mnode = (orte_mapped_node_t*)item;
        mnode->num_procs = opal_list_get_size(&mnode->procs);
    }
    
    /* save mapping to the registry */
    if(ORTE_SUCCESS != (rc = orte_rmaps_base_put_job_map(map))) {
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
        if (ORTE_SUCCESS != (rc = orte_rmgr.store_app_context(jobid, map->apps, 1))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    
    /* save a bookmark indicating what node we finished with so that subsequent children (if any)
     * can start at the right place
     */
    if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(attributes, ORTE_RMAPS_BOOKMARK,
                                                      ORTE_STRING, save_bookmark,
                                                      ORTE_RMGR_ATTR_OVERRIDE))) {
        ORTE_ERROR_LOG(rc);
    }

cleanup:
    while(NULL != (item = opal_list_remove_first(&master_node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&master_node_list);

    OBJ_DESTRUCT(&max_used_nodes);
    OBJ_DESTRUCT(&fully_used_nodes);
    OBJ_DESTRUCT(&mapped_node_list);
    OBJ_RELEASE(map);


    free(save_bookmark);
    
    return rc;
}


static int orte_rmaps_rr_finalize(void)
{
    return ORTE_SUCCESS;
}


orte_rmaps_base_module_t orte_rmaps_round_robin_module = {
    orte_rmaps_rr_map,
    orte_rmaps_base_get_job_map,
    orte_rmaps_base_get_node_map,
    orte_rmaps_rr_finalize
};

