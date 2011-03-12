/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
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
#include "orte/constants.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <string.h>

#include "opal/util/if.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/hostfile/hostfile.h"
#include "orte/util/dash_host/dash_host.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"

/*
 * Query the registry for all nodes allocated to a specified app_context
 */
int orte_rmaps_base_get_target_nodes(opal_list_t *allocated_nodes, orte_std_cntr_t *total_num_slots,
                                     orte_app_context_t *app, orte_mapping_policy_t policy)
{
    opal_list_item_t *item, *next;
    orte_node_t *node;
    orte_std_cntr_t num_slots;
    orte_std_cntr_t i;
    int rc;

    /** set default answer */
    *total_num_slots = 0;
    
    /* if the hnp was allocated, include it unless flagged not to */
    if (orte_hnp_is_allocated) {
        if (NULL != (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 0))) {
            if (ORTE_NODE_STATE_DO_NOT_USE == node->state) {
                /* clear this for future use, but don't include it */
                node->state = ORTE_NODE_STATE_UP;
            } else if (ORTE_NODE_STATE_NOT_INCLUDED != node->state) {
                OBJ_RETAIN(node);
                opal_list_append(allocated_nodes, &node->super);
            }
        }
    }
    
    /* add everything in the node pool that can be used */
    for (i=1; i < orte_node_pool->size; i++) {
        if (NULL != (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
            /* ignore nodes that are marked as do-not-use for this mapping */
            if (ORTE_NODE_STATE_DO_NOT_USE == node->state) {
                /* reset the state so it can be used another time */
                node->state = ORTE_NODE_STATE_UP;
                continue;
            }
            if (ORTE_NODE_STATE_DOWN == node->state) {
                continue;
            }
            if (ORTE_NODE_STATE_NOT_INCLUDED == node->state) {
                /* not to be used */
                continue;
            }
            /* retain a copy for our use in case the item gets
             * destructed along the way
             */
            OBJ_RETAIN(node);
            opal_list_append(allocated_nodes, &node->super);
        } 
    }

    /** check that anything is here */
    if (0 == opal_list_get_size(allocated_nodes)) {
        orte_show_help("help-orte-rmaps-base.txt",
                       "orte-rmaps-base:no-available-resources",
                       true);
        return ORTE_ERR_SILENT;
    }
    
    /* is there a default hostfile? */
    if (NULL != orte_default_hostfile) {
        /* yes - filter the node list through the file, removing
         * any nodes not in the file -or- excluded via ^
         */
        if (ORTE_SUCCESS != (rc = orte_util_filter_hostfile_nodes(allocated_nodes,
                                                                  orte_default_hostfile))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /** check that anything is here */
        if (0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt",
                           "orte-rmaps-base:no-available-resources",
                           true);
            return ORTE_ERR_SILENT;
        }
    }
    
    
    /* did the app_context contain a hostfile? */
    if (NULL != app && NULL != app->hostfile) {
        /* yes - filter the node list through the file, removing
         * any nodes not found in the file
         */
        if (ORTE_SUCCESS != (rc = orte_util_filter_hostfile_nodes(allocated_nodes,
                                                                  app->hostfile))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /** check that anything is here */
        if (0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:no-mapped-node",
                           true, app->app, app->hostfile);
            return ORTE_ERR_SILENT;
        }
    }
    
    
    /* did the app_context contain an add-hostfile? */
    if (NULL != app && NULL != app->add_hostfile) {
        /* yes - filter the node list through the file, removing
         * any nodes not found in the file
         */
        if (ORTE_SUCCESS != (rc = orte_util_filter_hostfile_nodes(allocated_nodes,
                                                                  app->add_hostfile))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /** check that anything is here */
        if (0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:no-mapped-node",
                           true, app->app, app->hostfile);
            return ORTE_ERR_SILENT;
        }
    }
    
    
    /* now filter the list through any -host specification */
    if (NULL != app && NULL != app->dash_host) {
        if (ORTE_SUCCESS != (rc = orte_util_filter_dash_host_nodes(allocated_nodes,
                                                                   app->dash_host))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /** check that anything is left! */
        if (0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:no-mapped-node",
                           true, app->app, "");
            return ORTE_ERR_SILENT;
        }
    }
    
    /* now filter the list through any add-host specification */
    if (NULL != app && NULL != app->add_host) {
        if (ORTE_SUCCESS != (rc = orte_util_filter_dash_host_nodes(allocated_nodes,
                                                                   app->add_host))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /** check that anything is left! */
        if (0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:no-mapped-node",
                           true, app->app, "");
            return ORTE_ERR_SILENT;
        }
    }
    
    /* If the "no local" option was set, then remove the local node
     * from the list
     */
    if (policy & ORTE_MAPPING_NO_USE_LOCAL) {
        /* we don't need to check through the entire list as
         * the head node - if it is on the list at all - will
         * always be in the first position
         */
        item = opal_list_get_first(allocated_nodes);
        node = (orte_node_t*)item;
        /* need to check ifislocal because the name in the
         * hostfile may not have been FQDN, while name returned
         * by gethostname may have been (or vice versa)
         */
        if (opal_ifislocal(node->name)) {
            opal_list_remove_item(allocated_nodes, item);
            OBJ_RELEASE(item);  /* "un-retain" it */
        }
        /** if we aren't mapping daemons, check that anything is left! */
        if (NULL != app && 0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt",
                           "orte-rmaps-base:nolocal-no-available-resources", true);
            return ORTE_ERR_SILENT;
        }
    }

    /* if the app is NULL, then we are mapping daemons - so remove
     * all nodes that already have a daemon on them
     *
     * NOTE: it is okay if the final list is empty. It just means
     * that there are no new daemons to be launched for the
     * virtual machine
     */
    if (NULL == app) {
        item  = opal_list_get_first(allocated_nodes);
        while (item != opal_list_get_end(allocated_nodes)) {
            
            /** save the next pointer in case we remove this node */
            next  = opal_list_get_next(item);
            
            /** already have a daemon? */
            node = (orte_node_t*)item;
            if (NULL != node->daemon) {
                /* if this is the local node, keep it if requested */
                if (node->daemon->name.vpid == ORTE_PROC_MY_HNP->vpid &&
                    !(policy & ORTE_MAPPING_NO_USE_LOCAL)) {
                    item = next;
                    continue;
                }
                opal_list_remove_item(allocated_nodes, item);
                OBJ_RELEASE(item);  /* "un-retain" it */
            }
            
            /** go on to next item */
            item = next;
        }
        *total_num_slots = 0;
        return ORTE_SUCCESS;
    }
    
    /* remove all nodes that are already at max usage, and
     * compute the total number of allocated slots while
     * we do so
     */
    num_slots = 0;
    item  = opal_list_get_first(allocated_nodes);
    while (item != opal_list_get_end(allocated_nodes)) {
        /** save the next pointer in case we remove this node */
        next  = opal_list_get_next(item);

        /** check to see if this node is fully used - remove if so */
        node = (orte_node_t*)item;
        if (0 != node->slots_max && node->slots_inuse > node->slots_max) {
            opal_list_remove_item(allocated_nodes, item);
            OBJ_RELEASE(item);  /* "un-retain" it */
        } else { /** otherwise, add the slots for our job to the total */
            num_slots += node->slots_alloc;
        }

        /** go on to next item */
        item = next;
    }

    /* Sanity check to make sure we have resources available */
    if (0 == num_slots) {
        orte_show_help("help-orte-rmaps-base.txt", 
                       "orte-rmaps-base:all-available-resources-used", true);
        return ORTE_ERR_SILENT;
    }
    
    *total_num_slots = num_slots;
    
    return ORTE_SUCCESS;
}


int orte_rmaps_base_add_proc_to_map(orte_job_map_t *map, orte_node_t *node,
                                    bool oversubscribed, orte_proc_t *proc)
{
    orte_std_cntr_t i;
    orte_node_t *node_from_map;
    int rc;
    
    /* see if this node has already been assigned to the map - if
     * not, then add the pointer to the pointer array
     */
    for (i=0; i < map->nodes->size; i++) {
        if (NULL == (node_from_map = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }
        if (node_from_map->index == node->index) {
            /* we have this node in the array */
            goto PROCESS;
        }
    }
    /* if we get here, then this node isn't already in the map - add it */
    OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                         "%s rmaps:base: adding node %s to map",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == node->name) ? "NULL" : node->name));
    
    if (ORTE_SUCCESS > (rc = opal_pointer_array_add(map->nodes, (void*)node))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    OBJ_RETAIN(node);  /* maintain accounting on object */
    ++map->num_nodes;
    
PROCESS:
    /* add the proc to this node's local processes - it is assumed
     * that the proc isn't already there as this would be an error
     * in the mapper
     */
    OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                         "%s rmaps:base: mapping proc for job %s to node %s whose daemon is %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(proc->name.jobid),
                         (NULL == node->name) ? "NULL" : node->name,
                         (NULL == node->daemon) ? "NULL" : ORTE_NAME_PRINT(&(node->daemon->name))));
    
    if (0 > (rc = opal_pointer_array_add(node->procs, (void*)proc))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* retain the proc struct so that we correctly track its release */
    OBJ_RETAIN(proc);
    ++node->num_procs;

    /* update the oversubscribed state of the node */
    node->oversubscribed = oversubscribed;
    
    return ORTE_SUCCESS;
}


/*
 * Claim a slot for a specified job on a node
 */
int orte_rmaps_base_claim_slot(orte_job_t *jdata,
                               orte_node_t *current_node,
                               int32_t cpus_per_rank,
                               orte_std_cntr_t app_idx,
                               opal_list_t *nodes,
                               bool oversubscribe,
                               bool remove_from_list,
                               orte_proc_t **returnproc)
{
    orte_proc_t *proc;
    bool oversub;
    int rc;

    /* if we were given a proc, just use it */
    if (NULL != returnproc && NULL != *returnproc) {
        proc = *returnproc;
    } else {
        /* create mapped_proc object */
        proc = OBJ_NEW(orte_proc_t);
        if (NULL == proc) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* set the jobid */
        proc->name.jobid = jdata->jobid;
        /* flag the proc as ready for launch */
        proc->state = ORTE_PROC_STATE_INIT;
        /* we do not set the vpid here - this will be done
         * during a second phase
         */
        proc->app_idx = app_idx;
        OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                             "%s rmaps:base:claim_slot: created new proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name)));
        
        /* provide returned proc, if requested */
        if (NULL != returnproc) {
            *returnproc = proc;
        }
    }

    OBJ_RETAIN(current_node);  /* maintain accounting on object */
    
    proc->node = current_node;
    proc->nodename = current_node->name;
    
    OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                         "%s rmaps:base:claim_slot mapping proc in job %s to node %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid), current_node->name));
    
    /* Be sure to demarcate the slots for this proc as claimed from the node */
    current_node->slots_inuse += 1;
    
    /* see if this node is oversubscribed now */
    if (current_node->slots_inuse > current_node->slots) {
        oversub = true;
    } else {
        oversub = false;
    }
    
    /* assign the proc to the node and ensure the node is on the map */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_add_proc_to_map(jdata->map, current_node,
                                                              oversub, proc))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(proc);
        return rc;
    }
    
    /* If this node has reached its max number of allocatable slots OR it has
     * reached the soft limit AND we are in a "no oversubscribe" state, then
     * we need to return a flag telling the mapper this is the case so it
     * can move on to the next node
     */
    if ((0 != current_node->slots_max  &&
        current_node->slots_inuse >= current_node->slots_max) ||
        (!oversubscribe && current_node->slots_inuse >= current_node->slots)) {
        /* see if we are supposed to remove the node from the list - some
         * mappers want us to do so to avoid any chance of continuing to
         * add procs to it
         */
        if (NULL != nodes && remove_from_list) {
            opal_list_remove_item(nodes, (opal_list_item_t*)current_node);
            /* release it - it was retained when we started, so this
             * just ensures the instance counter is correctly updated
             */
            OBJ_RELEASE(current_node);
        }
        /* now return the proper code so the caller knows this node
         * is fully used
         */
        return ORTE_ERR_NODE_FULLY_USED;
    }

    return ORTE_SUCCESS;
}

int orte_rmaps_base_compute_vpids(orte_job_t *jdata)
{
    orte_job_map_t *map;
    orte_vpid_t vpid;
    int i, j;
    orte_node_t *node;
    orte_proc_t *proc;
    int rc;
    
    map = jdata->map;
    
    if (ORTE_MAPPING_BYSLOT & map->policy ||
        ORTE_MAPPING_BYSOCKET & map->policy ||
        ORTE_MAPPING_BYBOARD & map->policy) {
        /* assign the ranks sequentially */
        for (i=0; i < map->nodes->size; i++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
                continue;
            }
            for (j=0; j < node->procs->size; j++) {
                if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, j))) {
                    continue;
                }
                /* ignore procs from other jobs */
                if (proc->name.jobid != jdata->jobid) {
                    continue;
                }
                if (ORTE_VPID_INVALID == proc->name.vpid) {
                    /* find the next available vpid */
                    for (vpid=0; vpid < jdata->num_procs; vpid++) {
                        if (NULL == opal_pointer_array_get_item(jdata->procs, vpid)) {
                            break;
                        }
                    }
                    proc->name.vpid = vpid;
                }
                if (NULL == opal_pointer_array_get_item(jdata->procs, proc->name.vpid)) {
                    if (ORTE_SUCCESS != (rc = opal_pointer_array_set_item(jdata->procs, proc->name.vpid, proc))) {
                        ORTE_ERROR_LOG(rc);
                        return rc;
                    }                    
                }
            }
        }
        return ORTE_SUCCESS;
    }
    
    if (ORTE_MAPPING_BYNODE & map->policy) {
        /* assign the ranks round-robin across nodes */
        for (i=0; i < map->nodes->size; i++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
                continue;
            }
            for (j=0; j < node->procs->size; j++) {
                if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, j))) {
                    continue;
                }
                /* ignore procs from other jobs */
                if (proc->name.jobid != jdata->jobid) {
                    continue;
                }
                if (ORTE_VPID_INVALID == proc->name.vpid) {
                    /* find the next available vpid */
                    vpid = i;
                    while (NULL != opal_pointer_array_get_item(jdata->procs, vpid)) {
                        vpid += map->num_nodes;
                        if (jdata->num_procs <= vpid) {
                            vpid = vpid - jdata->num_procs;
                        }
                    }
                    proc->name.vpid = vpid;
                }
                if (NULL == opal_pointer_array_get_item(jdata->procs, proc->name.vpid)) {
                    if (ORTE_SUCCESS != (rc = opal_pointer_array_set_item(jdata->procs, proc->name.vpid, proc))) {
                        ORTE_ERROR_LOG(rc);
                        return rc;
                    }                    
                }
            }
        }
        return ORTE_SUCCESS;
    }

    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_rmaps_base_compute_local_ranks(orte_job_t *jdata)
{
    orte_std_cntr_t i;
    int j, k;
    orte_node_t *node;
    orte_proc_t *proc, *psave, *psave2;
    orte_vpid_t minv, minv2;
    orte_local_rank_t local_rank;
    orte_job_map_t *map;
    
    OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                         "%s rmaps:base:compute_usage",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* point to map */
    map = jdata->map;
    
    /* for each node in the map... */
    for (i=0; i < map->nodes->size; i++) {
        /* cycle through the array of procs on this node, setting
         * local and node ranks, until we
         * have done so for all procs on nodes in this map
         */
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }
        
        /* init search values */
        local_rank = 0;
        
        /* the proc map may have holes in it, so cycle
         * all the way through and avoid the holes
         */
        for (k=0; k < node->procs->size; k++) {
            /* if this proc is NULL, skip it */
            if (NULL == opal_pointer_array_get_item(node->procs, k)) {
                continue;
            }
            minv = ORTE_VPID_MAX;
            minv2 = ORTE_VPID_MAX;
            psave = NULL;
            psave2 = NULL;
            /* find the minimum vpid proc */
            for (j=0; j < node->procs->size; j++) {
                /* if this proc is NULL, skip it */
                if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, j))) {
                    continue;
                }
                /* only look at procs for this job when
                 * determining local rank
                 */
                if (proc->name.jobid == jdata->jobid &&
                    ORTE_LOCAL_RANK_INVALID == proc->local_rank &&
                    proc->name.vpid < minv) {
                    minv = proc->name.vpid;
                    psave = proc;
                }
                /* no matter what job...still have to handle node_rank */
                if (ORTE_NODE_RANK_INVALID == proc->node_rank &&
                    proc->name.vpid < minv2) {
                    minv2 = proc->name.vpid;
                    psave2 = proc;
                }
            }
            if (NULL == psave && NULL == psave2) {
                /* we must have processed them all for this node! */
                break;
            }
            if (NULL != psave) {
                psave->local_rank = local_rank;
                ++local_rank;
            }
            if (NULL != psave2) {
                psave2->node_rank = node->next_node_rank;
                node->next_node_rank++;
            }
        }
    }

    return ORTE_SUCCESS;
}

/* when we restart a process on a different node, we have to
 * ensure that the node and local ranks assigned to the proc
 * don't overlap with any pre-existing proc on that node. If
 * we don't, then it would be possible for procs to conflict
 * when opening static ports, should that be enabled.
 */
void orte_rmaps_base_update_local_ranks(orte_job_t *jdata, orte_node_t *oldnode,
                                        orte_node_t *newnode, orte_proc_t *newproc)
{
    int k;
    orte_node_rank_t node_rank;
    orte_local_rank_t local_rank;
    orte_proc_t *proc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                         "%s rmaps:base:update_usage",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* if the node hasn't changed, then we can just use the
     * pre-defined values
     */
    if (oldnode == newnode) {
        return;
    }
    
    /* if the node has changed, then search the new node for the
     * lowest unused local and node rank
     */
    node_rank = 0;
retry_nr:
    for (k=0; k < newnode->procs->size; k++) {
        /* if this proc is NULL, skip it */
        if (NULL == (proc = (orte_proc_t *) opal_pointer_array_get_item(newnode->procs, k))) {
            continue;
        }
        if (node_rank == proc->node_rank) {
            node_rank++;
            goto retry_nr;
        }
    }
    newproc->node_rank = node_rank;
    
    local_rank = 0;
retry_lr:
    for (k=0; k < newnode->procs->size; k++) {
        /* if this proc is NULL, skip it */
        if (NULL == (proc = (orte_proc_t *) opal_pointer_array_get_item(newnode->procs, k))) {
            continue;
        }
        /* ignore procs from other jobs */
        if (proc->name.jobid != jdata->jobid) {
            continue;
        }
        if (local_rank == proc->local_rank) {
            local_rank++;
            goto retry_lr;
        }
    }
    newproc->local_rank = local_rank;
}


int orte_rmaps_base_define_daemons(orte_job_t *jdata)
{
    orte_job_map_t *map;
    orte_node_t *node;
    orte_proc_t *proc;
    orte_job_t *daemons;
    int i;
    int rc;

    OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                         "%s rmaps:base:define_daemons",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    if (ORTE_MAPPING_USE_VM & jdata->map->policy) {
        /* nothing for us to do - all daemons are
         * defined by definition!
         */
        return ORTE_SUCCESS;
    }

    /* get the daemon job data struct */
    if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_HNP->jobid))) {
        /* bad news */
        ORTE_ERROR_LOG(ORTE_ERR_FATAL);
        return ORTE_ERR_FATAL;
    }
    
    /* initialize the #new daemons */
    map = jdata->map;
    map->num_new_daemons = 0;
    
    /* go through the nodes in the map, checking each one's daemon name
     */
    for (i=0; i < map->nodes->size; i++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }
        if (NULL == node->daemon) {
            /* we haven't defined one for it
             * yet, so do so now and indicate it is to be launched
             */
            proc = OBJ_NEW(orte_proc_t);
            if (NULL == proc) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            proc->name.jobid = ORTE_PROC_MY_HNP->jobid;
            if (ORTE_VPID_MAX-1 <= daemons->num_procs) {
                /* no more daemons available */
                orte_show_help("help-orte-rmaps-base.txt", "out-of-vpids", true);
                OBJ_RELEASE(proc);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            proc->name.vpid = daemons->num_procs;  /* take the next available vpid */
            proc->node = node;
            proc->nodename = node->name;
            OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                 "%s rmaps:base:define_daemons add new daemon %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name)));
            /* add the daemon to the daemon job object */
            if (0 > (rc = opal_pointer_array_add(daemons->procs, (void*)proc))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            ++daemons->num_procs;
            /* point the node to the daemon */
            node->daemon = proc;
            OBJ_RETAIN(proc);  /* maintain accounting */
            /* track number of daemons to be launched */
            ++map->num_new_daemons;
            /* and their starting vpid */
            if (ORTE_VPID_INVALID == map->daemon_vpid_start) {
                map->daemon_vpid_start = proc->name.vpid;
            }
        }
        /*
         * If we are launching on a node where there used to be a daemon, but
         * it had previously failed, try to relaunch it. (Daemon Recovery) Do
         * this ONLY if there are procs mapped to that daemon!
         */
        else if (node->daemon->state > ORTE_PROC_STATE_UNTERMINATED) {
            /* If no processes are to be launched on this node, then exclude it */
            if( 0 >= node->num_procs ) {
                OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                     "%s rmaps:base:define_daemons Skipping the Recovery of daemon %s [0x%x] Launched: %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&node->daemon->name),
                                     node->daemon->state,
                                     (node->daemon_launched ? "T" : "F")
                                     ));
                /* since this daemon exists but is not needed, then flag it
                 * as "launched" to avoid relaunching it for no reason
                 */
                node->daemon_launched = true;
                continue;
            }

            OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                 "%s rmaps:base:define_daemons RECOVERING daemon %s [0x%x] Launched: %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&node->daemon->name),
                                 node->daemon->state,
                                 (node->daemon_launched ? "T" : "F")
                                 ));

            /* flag that the daemon is no longer launched */
            node->daemon_launched = false;

            /* set the state to indicate launch is in progress */
            node->daemon->state = ORTE_PROC_STATE_RESTART;

            free(node->daemon->rml_uri);
            node->daemon->rml_uri = NULL;
            
            OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                 "%s rmaps:base:define_daemons add new daemon %s (Recovering old daemon)",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&node->daemon->name)));

            /* track number of daemons to be launched */
            ++map->num_new_daemons;
        }
        else {
            /* this daemon was previously defined - flag it */
            node->daemon_launched = true;
            OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                 "%s rmaps:base:define_daemons existing daemon %s already launched",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&node->daemon->name)));
        }
    }

    return ORTE_SUCCESS;
}

int orte_rmaps_base_setup_virtual_machine(orte_job_t *jdata)
{
    orte_job_t *jdat;
    orte_node_t *node;
    orte_proc_t *proc;
    orte_job_map_t *map;
    opal_list_t node_list;
    opal_list_item_t *item;
    orte_app_context_t *app;
    orte_std_cntr_t num_slots;
    int rc, i, n;
    bool ignored;

    /* get the daemon app if provided - may include -host or hostfile
     * info about available nodes
     */
    app = (orte_app_context_t *) opal_pointer_array_get_item(jdata->apps, 0);
    
    map = jdata->map;
    
    /* get the list of all available nodes that do not already
     * have a daemon on them
     */
    OBJ_CONSTRUCT(&node_list, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots,
                                                               app, map->policy))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&node_list);
        return rc;
    }
    /* check all other known jobs to see if they have something to
     * add to the allocation - we won't have seen these and the
     * daemon job won't have any in its app
     */
    for (i=0; i < orte_job_data->size; i++) {
        if (NULL == (jdat = (orte_job_t*)opal_pointer_array_get_item(orte_job_data, i))) {
            continue;
        }
        for (n=0; n < jdat->apps->size; n++) {
            if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdat->apps, n))) {
                continue;
            }
            if (NULL != app->hostfile) {
                /* hostfile was specified - parse it and add it to the list. The
                 * function automatically ignores duplicates
                 */
                if (ORTE_SUCCESS != (rc = orte_util_add_hostfile_nodes(&node_list,
                                                                       &ignored,
                                                                       app->hostfile))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_DESTRUCT(&node_list);
                    return rc;
                }
            }
            if (NULL != app->dash_host) {
                /* parse and add to list, ignoring duplicates */
                if (ORTE_SUCCESS != (rc = orte_util_add_dash_host_nodes(&node_list,
                                                                        &ignored,
                                                                        app->dash_host))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_DESTRUCT(&node_list);
                    return rc;
                }
            }
        }
    }

    /* add all these nodes to the map */
    while (NULL != (item = opal_list_remove_first(&node_list))) {
        node = (orte_node_t*)item;
        /* if this is my node, ignore it - we are already here */
        if (0 == strcmp(node->name, orte_process_info.nodename)) {
            continue;
        }
        opal_pointer_array_add(map->nodes, (void*)node);
        ++(map->num_nodes);
        /* if this node already has a daemon, release that object
         * to maintain bookkeeping
         */
        if (NULL != node->daemon) {
            OBJ_RELEASE(node->daemon);
        }
        /* create a new daemon object for this node */
        proc = OBJ_NEW(orte_proc_t);
        if (NULL == proc) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        proc->name.jobid = ORTE_PROC_MY_HNP->jobid;
        if (ORTE_VPID_MAX-1 <= jdata->num_procs) {
            /* no more daemons available */
            orte_show_help("help-orte-rmaps-base.txt", "out-of-vpids", true);
            OBJ_RELEASE(proc);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        proc->name.vpid = jdata->num_procs;  /* take the next available vpid */
        proc->node = node;
        proc->nodename = node->name;
        OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                             "%s rmaps:base:setup_vm add new daemon %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name)));
        /* add the daemon to the daemon job object */
        if (0 > (rc = opal_pointer_array_add(jdata->procs, (void*)proc))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        ++jdata->num_procs;
        /* point the node to the daemon */
        node->daemon = proc;
        OBJ_RETAIN(proc);  /* maintain accounting */
        /* track number of daemons to be launched */
        ++map->num_new_daemons;
        /* and their starting vpid */
        if (ORTE_VPID_INVALID == map->daemon_vpid_start) {
            map->daemon_vpid_start = proc->name.vpid;
        }
    }
    OBJ_DESTRUCT(&node_list);
    
    return ORTE_SUCCESS;
}
