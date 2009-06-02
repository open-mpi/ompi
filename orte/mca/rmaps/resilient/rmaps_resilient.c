/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include <stdio.h>

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "rmaps_resilient.h"


/*
 * Local variable
 */
static opal_list_item_t *cur_node_item = NULL;
static orte_vpid_t vpid_start = 0;

static char *orte_getline(FILE *fp);

/* default round-robin mapper */
static int rr_map_byslot(orte_job_t *jdata, orte_app_context_t *app,
                         orte_job_map_t *map, opal_list_t *node_list,
                         orte_vpid_t num_procs)
{
    int rc;
    int overload;
    int i;
    orte_node_t *node, *ndmin, *nd1;
    opal_list_item_t *item, *next;
    orte_vpid_t num_alloc = 0;
    int num_slots_to_take;
    
    /* if a bookmark exists from some prior mapping, set us to start there */
    if (NULL != jdata->bookmark) {
        cur_node_item = NULL;
        /* find this node on the list */
        for (item = opal_list_get_first(node_list);
             item != opal_list_get_end(node_list);
             item = opal_list_get_next(item)) {
            node = (orte_node_t*)item;
            
            if (node->index == jdata->bookmark->index) {
                cur_node_item = item;
                break;
            }
        }
        /* see if we found it - if not, just start at the beginning */
        if (NULL == cur_node_item) {
            cur_node_item = opal_list_get_first(node_list); 
        }
    } else {
        /* if no bookmark, then just start at the beginning of the list */
        cur_node_item = opal_list_get_first(node_list);
    }
    
    /* is this node fully subscribed? If so, then the first
     * proc we assign will oversubscribe it, so let's look
     * for another candidate
     */
    node = (orte_node_t*)cur_node_item;
    ndmin = node;
    overload = ndmin->slots_inuse - ndmin->slots_alloc;
    if (node->slots_inuse >= node->slots_alloc) {
        /* work down the list - is there another node that
         * would not be oversubscribed?
         */
        if (cur_node_item != opal_list_get_last(node_list)) {
            item = opal_list_get_next(cur_node_item);
        } else {
            item = opal_list_get_first(node_list);
        }
        while (item != cur_node_item) {
            nd1 = (orte_node_t*)item;
            if (nd1->slots_inuse < nd1->slots_alloc) {
                /* this node is not oversubscribed! use it! */
                cur_node_item = item;
                goto proceed;
            }
            /* this one was also oversubscribed, keep track of the
             * node that has the least usage - if we can't
             * find anyone who isn't fully utilized, we will
             * start with the least used node
             */
            if (overload >= (nd1->slots_inuse - nd1->slots_alloc)) {
                ndmin = nd1;
                overload = ndmin->slots_inuse - ndmin->slots_alloc;
            }
            if (item == opal_list_get_last(node_list)) {
                item = opal_list_get_first(node_list);
            } else {
                item= opal_list_get_next(item);
            }
        }
        /* if we get here, then we cycled all the way around the
         * list without finding a better answer - just use the node
         * that is minimally overloaded
         */
        cur_node_item = (opal_list_item_t*)ndmin;
    }
    
proceed:        
    /* This loop continues until all procs have been mapped or we run
     out of resources. We determine that we have "run out of
     resources" when either all nodes have slots_max processes mapped to them,
     (thus there are no free slots for a process to be mapped), OR all nodes
     have reached their soft limit and the user directed us to "no oversubscribe".
     If we still have processes that haven't been mapped yet, then it's an
     "out of resources" error. */
    
    while ( num_alloc < num_procs) {
        /** see if any nodes remain unused and available. We need to do this check
         * each time since we may remove nodes from the list (as they become fully
         * used) as we cycle through the loop */
        if(0 >= opal_list_get_size(node_list) ) {
            /* Everything is at max usage! :( */
            orte_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:alloc-error",
                           true, num_procs, app->app);
            return ORTE_ERR_SILENT;
        }
        
        /* Save the next node we can use before claiming slots, since
         * we may need to prune the nodes list removing overused nodes.
         * Wrap around to beginning if we are at the end of the list */
        if (opal_list_get_end(node_list) == opal_list_get_next(cur_node_item)) {
            next = opal_list_get_first(node_list);
        }
        else {
            next = opal_list_get_next(cur_node_item);
        }
        
        /** declare a shorter name for convenience in the code below */
        node = (orte_node_t*) cur_node_item;
        /* If we have available slots on this node, claim all of them 
         * If node_slots == 0, assume 1 slot for that node. 
         * JJH - is this assumption fully justified?
         *
         * If we are now oversubscribing the nodes, then we still take:
         * (a) if the node has not been used yet, we take a full node_slots
         * (b) if some of the slots are in-use, then we take the number of
         *     remaining slots before hitting the soft limit (node_slots)
         * (c) if we are at or above the soft limit, we take a full node_slots
         *     unless we are loadbalancing, in which case we only take one
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
        if (node->slots_inuse >= node->slots_alloc || 0 == node->slots_inuse) {
            num_slots_to_take = (node->slots_alloc == 0) ? 1 : node->slots_alloc;
        } else {
            num_slots_to_take = node->slots_alloc - node->slots_inuse;
        }
        
        /* check if we are in npernode mode - if so, then set the num_slots_to_take
         * to the num_per_node
         */
        if (jdata->map->pernode) {
            num_slots_to_take = jdata->map->npernode;
        }
        
        for( i = 0; i < num_slots_to_take; ++i) {
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node, vpid_start + num_alloc, NULL, app->idx,
                                                                 node_list, jdata->map->oversubscribe, true))) {
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
            
            /** if all the procs have been mapped, we return */
            if (num_alloc == num_procs) {
                goto complete;
            }
            
            /* if we have fully used up this node, then break from the loop */
            if (ORTE_ERR_NODE_FULLY_USED == rc) {
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

complete:
    /* update the starting vpid */
    vpid_start += num_procs;
    
    /* save the bookmark */
    jdata->bookmark = (orte_node_t*)cur_node_item;
    
    return ORTE_SUCCESS;
}

static void flag_nodes(opal_list_t *node_list)
{
    opal_list_item_t *item, *nitem;
    orte_node_t *node, *nd;
    orte_rmaps_res_ftgrp_t *ftgrp;
    int k;
    
    for (item = opal_list_get_first(&mca_rmaps_resilient_component.fault_grps);
         item != opal_list_get_end(&mca_rmaps_resilient_component.fault_grps);
         item = opal_list_get_next(item)) {
        ftgrp = (orte_rmaps_res_ftgrp_t*)item;
        /* reset the flags */
        ftgrp->used = false;
        ftgrp->included = false;
        /* if at least one node in our list is included in this
         * ftgrp, then flag it as included
         */
        for (nitem = opal_list_get_first(node_list);
             !ftgrp->included && nitem != opal_list_get_end(node_list);
             nitem = opal_list_get_next(nitem)) {
            node = (orte_node_t*)nitem;
            for (k=0; k < ftgrp->nodes.size; k++) {
                if (NULL == (nd = (orte_node_t*)opal_pointer_array_get_item(&ftgrp->nodes, k))) {
                    continue;
                }
                if (0 == strcmp(nd->name, node->name)) {
                    ftgrp->included = true;
                    break;
                }
            }
        }
    }
}


/*
 * Loadbalance the cluster
 */
static int orte_rmaps_resilient_map(orte_job_t *jdata)
{
    orte_job_map_t *map;
    orte_app_context_t *app;
    int i, j, k, totnodes;
    opal_list_t node_list;
    opal_list_item_t *item;
    orte_std_cntr_t num_slots;
    int rc;
    float avgload, minload;
    orte_node_t *node, *nd;
    orte_rmaps_res_ftgrp_t *ftgrp, *target;
    orte_vpid_t totprocs;
    FILE *fp;
    char *ftinput;
    int grp;
    char **nodes;
    bool found;
    
    /* have we already constructed the fault group list? */
    if (0 == opal_list_get_size(&mca_rmaps_resilient_component.fault_grps) &&
        NULL != mca_rmaps_resilient_component.fault_group_file) {
        /* construct it */
        fp = fopen(mca_rmaps_resilient_component.fault_group_file, "r");
        if (NULL == fp) { /* not found */
            orte_show_help("help-orte-rmaps-resilient.txt", "orte-rmaps-resilient:file-not-found",
                           true, mca_rmaps_resilient_component.fault_group_file);
            return ORTE_ERROR;
        }
        /* build list of fault groups */
        grp = 0;
        while (NULL != (ftinput = orte_getline(fp))) {
            ftgrp = OBJ_NEW(orte_rmaps_res_ftgrp_t);
            ftgrp->ftgrp = grp++;
            nodes = opal_argv_split(ftinput, ',');
            /* find the referenced nodes */
            for (k=0; k < opal_argv_count(nodes); k++) {
                found = false;
                for (i=0; i < orte_node_pool->size && !found; i++) {
                    if (NULL == (node = opal_pointer_array_get_item(orte_node_pool, i))) {
                        continue;
                    }
                    if (0 == strcmp(node->name, nodes[k])) {
                        OBJ_RETAIN(node);
                        opal_pointer_array_add(&ftgrp->nodes, node);
                        found = true;
                        break;
                    }
                }
            }
            opal_list_append(&mca_rmaps_resilient_component.fault_grps, &ftgrp->super);
            opal_argv_free(nodes);
            free(ftinput);
        }
        fclose(fp);
    }
    
    /* the map will never be NULL as we initialize it before getting here,
     * so check to see if any nodes are in the map - this will be our
     * indicator that this is the prior map for a failed job that
     * needs to be re-mapped
     */
    if (0 < jdata->map->num_nodes) {
        /* this map tells us how a job that failed was mapped - we need
         * to save this map and create a new one where we can put the
         * new mapping
         */
        return ORTE_ERR_NOT_IMPLEMENTED;
    }
    
    
    /* CREATE INITIAL MAP FOR A JOB */
    /* we map each app_context separately when creating an initial job map. For
     * each app_context, we get the list of available nodes as this can be
     * app_context specific based on hostfile and -host options. We then organize
     * that list into fault groups based on the fault group definitions, if
     * provided, and then divide the specified number of copies across them in
     * a load-balanced way
     */
    
    /* start at the beginning... */
    vpid_start = 0;
    jdata->num_procs = 0;
    map = jdata->map;
    
    for (i=0; i < jdata->apps->size; i++) {
        /* get the app_context */
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        /* for each app_context, we have to get the list of nodes that it can
         * use since that can now be modified with a hostfile and/or -host
         * option
         */
        OBJ_CONSTRUCT(&node_list, opal_list_t);
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app,
                                                                  map->policy))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
        /* were we given a fault group definition? */
        if (0 < opal_list_get_size(&mca_rmaps_resilient_component.fault_grps)) {
            /* flag the fault groups included by these nodes */
            flag_nodes(&node_list);
            /* map each copy to a different fault group - if more copies are
             * specified than fault groups, then overlap in a round-robin fashion
             */
            for (j=0; j < app->num_procs; j++) {
                /* find unused included fault group with lowest average load - if none
                 * found, then break
                 */
                target = NULL;
                minload = 1000000000.0;
                for (item = opal_list_get_first(&mca_rmaps_resilient_component.fault_grps);
                     item != opal_list_get_end(&mca_rmaps_resilient_component.fault_grps);
                     item = opal_list_get_next(item)) {
                    ftgrp = (orte_rmaps_res_ftgrp_t*)item;
                    /* if this ftgrp has already been used or is not included, then
                     * skip it
                     */
                    if (ftgrp->used || !ftgrp->included) {
                        continue;
                    }
                    /* compute the load average on this fault group */
                    totprocs = 0;
                    totnodes = 0;
                    for (k=0; k < ftgrp->nodes.size; k++) {
                        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(&ftgrp->nodes, k))) {
                            continue;
                        }
                        totnodes++;
                        totprocs += node->num_procs;
                    }
                    avgload = (float)totprocs / (float)totnodes;
                    if (avgload < minload) {
                        minload = avgload;
                        target = ftgrp;
                    }
                }
                /* if we have more procs than fault groups, then we simply
                 * map the remaining procs on available nodes in a round-robin
                 * fashion - it doesn't matter where they go as they will not
                 * be contributing to fault tolerance by definition
                 */
                if (NULL == target) {
                    if (ORTE_SUCCESS != (rc = rr_map_byslot(jdata, app, map, &node_list, app->num_procs-vpid_start))) {
                        goto error;
                    }
                    goto cleanup;
                }
                /* pick node with lowest load from within that group */
                totprocs = 1000000;
                for (k=0; k < target->nodes.size; k++) {
                    if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(&target->nodes, k))) {
                        continue;
                    }
                    if (node->num_procs < totprocs) {
                        totprocs = node->num_procs;
                        nd = node;
                    }
                }
                /* put proc on that node */
                if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, nd, vpid_start, NULL, app->idx,
                                                                     &node_list, jdata->map->oversubscribe, false))) {
                    /** if the code is ORTE_ERR_NODE_FULLY_USED, then we know this
                     * really isn't an error
                     */
                    if (ORTE_ERR_NODE_FULLY_USED != rc) {
                        ORTE_ERROR_LOG(rc);
                        goto error;
                    }
                }
                /* track number of procs mapped */
                vpid_start++;
                
                /* flag this fault group as used */
                target->used = true;
            }
        } else {
            /* if we don't have a fault group definition, then just map the
             * procs in a round-robin manner
             */
            if (ORTE_SUCCESS != (rc = rr_map_byslot(jdata, app, map, &node_list, app->num_procs))) {
                goto error;
            }
        }
        
    cleanup:
        /* cleanup the node list - it can differ from one app_context
         * to another, so we have to get it every time
         */
        while (NULL != (item = opal_list_remove_first(&node_list))) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&node_list);
        /* update the number of procs in the job */
        jdata->num_procs += vpid_start;
    }
    
    /* compute and save convenience values */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_usage(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;

error:
    while (NULL != (item = opal_list_remove_first(&node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node_list);

    return rc;
}

orte_rmaps_base_module_t orte_rmaps_resilient_module = {
    orte_rmaps_resilient_map
};

static char *orte_getline(FILE *fp)
{
    char *ret, *buff;
    char input[1024];
    
    ret = fgets(input, 1024, fp);
    if (NULL != ret) {
        input[strlen(input)-1] = '\0';  /* remove newline */
        buff = strdup(input);
        return buff;
    }
    
    return NULL;
}

