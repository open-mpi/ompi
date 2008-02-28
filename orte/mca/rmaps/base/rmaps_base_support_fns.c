/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#include <string.h>

#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/if.h"
#include "opal/util/show_help.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/sys_info.h"
#include "orte/util/hostfile/hostfile.h"
#include "orte/util/dash_host/dash_host.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/plm/plm_types.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"

/*
 * Query the registry for all nodes allocated to a specified app_context
 */
int orte_rmaps_base_get_target_nodes(opal_list_t *allocated_nodes, orte_std_cntr_t *total_num_slots,
                                     orte_app_context_t *app, bool nolocal)
{
    opal_list_item_t *item, *next;
    orte_node_t *node, **nodes;
    orte_std_cntr_t num_slots;
    orte_std_cntr_t i;
    int rc;

    /** set default answer */
    *total_num_slots = 0;
    
    /* create a working list of nodes */
    nodes = (orte_node_t**)orte_node_pool->addr;
    for (i=0; i < orte_node_pool->size; i++) {
        if (NULL != nodes[i] && nodes[i]->allocate) {
            /* retain a copy for our use in case the item gets
             * destructed along the way
             */
            OBJ_RETAIN(nodes[i]);
            opal_list_append(allocated_nodes, &nodes[i]->super);
        }
    }

    /** check that anything is here */
    if (0 == opal_list_get_size(allocated_nodes)) {
        opal_show_help("help-orte-rmaps-base.txt",
                       "orte-rmaps-base:no-available-resources",
                       true);
        return ORTE_ERR_SILENT;
    }
    
    /* did the app_context contain a hostfile? */
    if (NULL != app->hostfile) {
        /* yes - filter the node list through the file, removing
         * any nodes not found in the file
         */
        if (ORTE_SUCCESS != (rc = orte_util_filter_hostfile_nodes(allocated_nodes,
                                                                  app->hostfile))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /** check that anything is here */
    if (0 == opal_list_get_size(allocated_nodes)) {
        opal_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:no-mapped-node",
                       true, app->app, app->hostfile);
        return ORTE_ERR_SILENT;
    }
    
    /* now filter the list through any -host specification */
    if (ORTE_SUCCESS != (rc = orte_util_filter_dash_host_nodes(allocated_nodes,
                                                               app->num_map, app->map_data))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /** check that anything is left! */
    if (0 == opal_list_get_size(allocated_nodes)) {
        opal_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:no-mapped-node",
                       true, app->app, app->map_data);
        return ORTE_ERR_SILENT;
    }
    
    /* If the "no local" option was set, then remove the local node
        from the list */
    if (nolocal) {
        for (item  = opal_list_get_first(allocated_nodes);
             item != opal_list_get_end(allocated_nodes);
             item  = opal_list_get_next(item) ) {
            node = (orte_node_t*)item;
            /* by this time, we have adjusted all local node
             * names to be our node name, so we don't need
             * to keep checking for that condition
             */
            if (0 == strcmp(node->name, orte_system_info.nodename)) {
                opal_list_remove_item(allocated_nodes, item);
                OBJ_RELEASE(item);  /* "un-retain" it */
                break;
            }
        }
    }

    /** check that anything is left! */
    if (0 == opal_list_get_size(allocated_nodes)) {
        opal_show_help("help-orte-rmaps-base.txt",
                       "orte-rmaps-base:nolocal-no-available-resources", true);
        return ORTE_ERR_SILENT;
    }
        
    /* remove all nodes that are already at max usage, and
     * compute the total number of available slots while
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
            num_slots += node->slots;
        }

        /** go on to next item */
        item = next;
    }

    /* Sanity check to make sure we have resources available */
    if (0 == num_slots) {
        opal_show_help("help-orte-rmaps-base.txt", 
                       "orte-rmaps-base:all-available-resources-used", true);
        return ORTE_ERR_SILENT;
    }
    
    *total_num_slots = num_slots;
    
    return ORTE_SUCCESS;
}


/*
 * Query the registry for all nodes allocated to a specified job
 */
#if 0
static int compare(opal_list_item_t **a, opal_list_item_t **b)
{
    orte_ras_proc_t *aa = *((orte_ras_proc_t **) a);
    orte_ras_proc_t *bb = *((orte_ras_proc_t **) b);

    return (aa->rank - bb->rank);
}
#endif

int orte_rmaps_base_get_target_procs(opal_list_t *procs)
{
#if 0
    int rc;
    /* get the allocation for this job */
    if(ORTE_SUCCESS != (rc = orte_ras.proc_query(procs))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    opal_list_sort(procs, compare);
#endif
    return ORTE_SUCCESS;
}



int orte_rmaps_base_add_proc_to_map(orte_job_map_t *map, orte_node_t *node,
                                    bool oversubscribed, orte_proc_t *proc)
{
    orte_std_cntr_t i;
    orte_node_t **nodes;
    int rc;
    
    /* see if this node has already been assigned to the map - if
     * not, then add the pointer to the pointer array
     */
    nodes = (orte_node_t**)map->nodes->addr;
    for (i=0; i < map->num_nodes; i++) {
        if (nodes[i]->index == node->index) {
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
                         "%s rmaps:base: mapping proc %s to node %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&proc->name),
                         (NULL == node->name) ? "NULL" : node->name));
    
    if (0 > (rc = opal_pointer_array_add(node->procs, (void*)proc))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* retain the proc struct so that we correctly track its release */
    OBJ_RETAIN(proc);
    ++node->num_procs;
    
    return ORTE_SUCCESS;
}


/*
 * Claim a slot for a specified job on a node
 */
int orte_rmaps_base_claim_slot(orte_job_t *jdata,
                               orte_node_t *current_node,
                               orte_vpid_t vpid,
                               orte_std_cntr_t app_idx,
                               opal_list_t *nodes,
                               bool oversubscribe)
{
    orte_proc_t *proc;
    bool oversub;
    int rc;
    
    /* create mapped_proc object */
    proc = OBJ_NEW(orte_proc_t);
    if (NULL == proc) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* create the process name */
    proc->name.jobid = jdata->jobid;
    proc->name.vpid = vpid;
    proc->app_idx = app_idx;
    OBJ_RETAIN(current_node);  /* maintain accounting on object */
    proc->node = current_node;
    if (NULL != current_node->name) {
        proc->nodename = strdup(current_node->name);
    }
    
    /* add this proc to the job's data - we don't have to worry here
     * about keeping the array left-justified as all vpids
     * from 0 to num_procs will be filled
     */
    OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                         "%s rmaps:base:claim_slot mapping rank %d to job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         vpid, ORTE_JOBID_PRINT(jdata->jobid)));
    if (ORTE_SUCCESS != (rc = opal_pointer_array_set_item(jdata->procs,
                                                          (int)vpid,
                                                          (void*)proc))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(proc);
        return rc;
    }
    
    /* Be sure to demarcate this slot as claimed for the node */
    current_node->slots_inuse++;
    
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
    
    /* Remove this node if it has reached its max number of allocatable slots OR it has
     * reached the soft limit AND we are in a "no oversubscribe" state
     */
    if ((0 != current_node->slots_max  &&
        current_node->slots_inuse >= current_node->slots_max) ||
        (!oversubscribe && current_node->slots_inuse >= current_node->slots)) {
        opal_list_remove_item(nodes, (opal_list_item_t*)current_node);
        /* release it - it was retained when we started, so this
         * just ensures the instance counter is correctly updated
         */
        OBJ_RELEASE(current_node);
        /** now return the proper code so the caller knows we removed the node! */
        return ORTE_ERR_NODE_FULLY_USED;
    }

    return ORTE_SUCCESS;
}


#if 0
static int orte_find_unallocated_proc_in_map(orte_ras_proc_t *proc, orte_job_map_t *map, orte_proc_t **mproc)
{
    orte_mapped_node_t *mnode;
    opal_list_item_t *item, *item2;
    int i;

    for (item = opal_list_get_first(&map->nodes);
         item != opal_list_get_end(&map->nodes);
         item = opal_list_get_next(item)) {
        mnode = (orte_mapped_node_t*)item;
        if (strcmp(proc->node_name, mnode->nodename)) {
            continue;
        }
        for (item2 = opal_list_get_first(&mnode->procs),i=1;
             item2 != opal_list_get_end(&mnode->procs);
             item2 = opal_list_get_next(item2),i++) {
            *mproc = (orte_mapped_proc_t*)item2;
            if (NULL == (*mproc)->slot_list) {
                return ORTE_SUCCESS;
            }
        }
    }
    return ORTE_ERROR;
}
#endif

int orte_rmaps_base_rearrange_map(orte_app_context_t *app, orte_job_map_t *map, opal_list_t *procs)
{
#if 0
    opal_list_item_t *proc_item, *map_node_item, *map_proc_item;
    orte_mapped_node_t *mnode;
    bool *used_ranks; /* an array for string used ranks */
    orte_std_cntr_t used_rank_index;
    orte_std_cntr_t assigned_procs = 0;
    orte_ras_proc_t *proc;
    orte_mapped_proc_t *mproc;
    int rc;

    used_ranks = (bool *)calloc(map->vpid_range, sizeof(bool));

    for (proc_item = opal_list_get_first(procs);
         proc_item != opal_list_get_end(procs) && assigned_procs < app->num_procs;
         proc_item = opal_list_get_next(proc_item)) {
        proc = (orte_ras_proc_t *)proc_item;
        if (proc->rank != ORTE_VPID_MAX) {
            /* Check if this proc belong to this map */
            if (proc->rank >= map->vpid_start && proc->rank < (map->vpid_start + map->vpid_range)) {
                if (ORTE_SUCCESS != (rc = orte_find_unallocated_proc_in_map(proc, map, &mproc))){
                    free (used_ranks);
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                mproc->slot_list = strdup(proc->cpu_list);
                mproc->rank = proc->rank;
                mproc->name.vpid = proc->rank;
                mproc->maped_rank = true;
                used_rank_index = proc->rank - map->vpid_start;
                used_ranks[used_rank_index] = true;
                assigned_procs ++;
            }
        }else if (NULL != proc->cpu_list) {
            if (ORTE_SUCCESS != (rc = orte_find_unallocated_proc_in_map(proc, map, &mproc))){
                continue; /* since there is not a specifiv rank continue searching */
            }
            mproc->slot_list = strdup(proc->cpu_list);
            assigned_procs ++;
        }
    }
    if(assigned_procs > 0) {
        used_rank_index = 0;
        for (map_node_item = opal_list_get_first(&map->nodes);
             map_node_item != opal_list_get_end(&map->nodes);
             map_node_item = opal_list_get_next(map_node_item)) {
            mnode = (orte_mapped_node_t*)map_node_item;
            for (map_proc_item = opal_list_get_first(&mnode->procs);
                 map_proc_item != opal_list_get_end(&mnode->procs);
                 map_proc_item = opal_list_get_next(map_proc_item)) {
                mproc = (orte_mapped_proc_t*)map_proc_item;
                if (mproc->maped_rank) {
                    continue;
                }
                while (used_ranks[used_rank_index]){
                    used_rank_index++;
                }
                mproc->rank = map->vpid_start + used_rank_index;
                mproc->name.vpid = mproc->rank;
                used_rank_index++;
            }
        }
    }
    free (used_ranks);
#endif
    return ORTE_SUCCESS;
}

int orte_rmaps_base_compute_usage(orte_job_t *jdata)
{
    orte_std_cntr_t i, j;
    orte_node_t **nodes;
    orte_proc_t **procs, *psave = NULL;
    orte_vpid_t minv, local_rank;
    orte_job_map_t *map;
    
    OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                         "%s rmaps:base:compute_usage",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* point to map */
    map = jdata->map;
    
    /* for each node in the map... */
    nodes = (orte_node_t**)map->nodes->addr;
    for (i=0; i < map->nodes->size; i++) {
        if (NULL != nodes[i]) {
            /* cycle through the array of procs on this node, looking for
             * the minimum vpid one and setting that local rank, until we
             * have done so for all procs on the node
             */
            
            /* init search values */
            procs = (orte_proc_t**)nodes[i]->procs->addr;
            local_rank = 0;
            
            while (local_rank < nodes[i]->num_procs) {
                minv = ORTE_VPID_MAX;
                /* find the minimum vpid proc */
                for (j=0; j < nodes[i]->procs->size; j++) {
                    if (NULL != procs[j]) {
                        if (ORTE_VPID_INVALID != procs[j]->local_rank) {
                            /* already done this one */
                            continue;
                        }
                        if (procs[j]->name.vpid < minv) {
                            minv = procs[j]->name.vpid;
                            psave = procs[j];
                        }
                    }
                }
                psave->local_rank = local_rank;
                ++local_rank;
            }
        }
    }

    return ORTE_SUCCESS;
}

int orte_rmaps_base_define_daemons(orte_job_map_t *map)
{
    orte_node_t *node, **nodes;
    orte_proc_t *proc;
    orte_job_t *daemons;
    orte_std_cntr_t i;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                         "%s rmaps:base:define_daemons",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* get the daemon job data struct */
    if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        /* bad news */
        ORTE_ERROR_LOG(ORTE_ERR_FATAL);
        return ORTE_ERR_FATAL;
    }
    
    /* go through the nodes in the map, checking each one's daemon name
     */
    nodes = (orte_node_t**)map->nodes->addr;
    for (i=0; i < map->num_nodes; i++) {
        node = nodes[i];
        if (NULL == node->daemon) {
            /* we haven't defined one for it
             * yet, so do so now and indicate it is to be launched
             */
            proc = OBJ_NEW(orte_proc_t);
            if (NULL == proc) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            proc->name.jobid = ORTE_PROC_MY_NAME->jobid;
            if (ORTE_VPID_MAX-1 <= daemons->num_procs) {
                /* no more daemons available */
                opal_show_help("help-orte-rmaps-base.txt", "out-of-vpids", true);
                OBJ_RELEASE(proc);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            proc->name.vpid = daemons->num_procs;  /* take the next available vpid */
            proc->node = node;
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
        } else {
            /* this daemon was previously defined - see if it has launched. The daemons
             * are stored in vpid order, so just look it up
             */
            if (daemons->procs->size < (orte_std_cntr_t)node->daemon->name.vpid ||
                daemons->num_procs < node->daemon->name.vpid) {
                /* well that is bad */
                ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                return ORTE_ERR_BAD_PARAM;                    
            }
            proc = (orte_proc_t*)daemons->procs->addr[node->daemon->name.vpid];
            if (NULL == proc) {
                /* well that is bad too */
                ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                return ORTE_ERR_BAD_PARAM;
            }
            if (NULL != proc->rml_uri) {
                node->daemon_launched = true;
                OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                     "%s rmaps:base:define_daemons existing daemon %s already launched",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc->name)));
            } else {
                node->daemon_launched = false;
                OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                     "%s rmaps:base:define_daemons existing daemon %s has not been launched",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc->name)));
            }
        }
    }

    return ORTE_SUCCESS;
}
