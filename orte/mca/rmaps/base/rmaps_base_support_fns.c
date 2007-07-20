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
#include "orte/orte_constants.h"

#include <string.h>

#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/if.h"
#include "opal/util/show_help.h"

#include "orte/runtime/params.h"
#include "orte/util/sys_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/smr/smr_types.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/ns/ns.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"

/*
 * A sanity check to ensure that all of the requested nodes are actually
 * allocated to this application.
 */
static bool are_all_mapped_valid(char **mapping,
                                 int num_mapped,
                                 opal_list_t* nodes)
{
    opal_list_item_t *item;
    orte_ras_node_t *node;
    int i;
    bool matched;
    
    for (i = 0; i < num_mapped; ++i) {
        matched = false;
        
        for(item  = opal_list_get_first(nodes);
            item != opal_list_get_end(nodes);
            item  = opal_list_get_next(item) ) {
            node = (orte_ras_node_t*) item;
            if( 0 == strcmp(node->node_name, mapping[i]) ) {
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
                      int num_mapped) 
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
 * Query the registry for all nodes allocated to a specified job
 */
int orte_rmaps_base_get_target_nodes(opal_list_t *allocated_nodes, orte_jobid_t jobid, orte_std_cntr_t *total_num_slots, bool nolocal)
{
    opal_list_item_t *item, *next;
    orte_ras_node_t *node;
    int rc;
    size_t nodelist_size;
    orte_std_cntr_t num_slots=0;

    /** set default answer */
    *total_num_slots = 0;
    
    /* get the allocation for this job */
    if(ORTE_SUCCESS != (rc = orte_ras.node_query_alloc(allocated_nodes, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    nodelist_size = opal_list_get_size(allocated_nodes);

    /* If the "no local" option was set, then remove the local node
        from the list */
    if (nolocal) {
        for (item  = opal_list_get_first(allocated_nodes);
             item != opal_list_get_end(allocated_nodes);
             item  = opal_list_get_next(item) ) {
            node = (orte_ras_node_t*)item;
            if (0 == strcmp(node->node_name, orte_system_info.nodename) ||
                opal_ifislocal(node->node_name)) {
                opal_list_remove_item(allocated_nodes, item);
                break;
            }
        }
    }

    /** remove all nodes that are already at max usage */
    item  = opal_list_get_first(allocated_nodes);
    while (item != opal_list_get_end(allocated_nodes)) {

        /** save the next pointer in case we remove this node */
        next  = opal_list_get_next(item);

        /** check to see if this node is fully used - remove if so */
        node = (orte_ras_node_t*)item;
        if (0 != node->node_slots_max && node->node_slots_inuse > node->node_slots_max) {
            opal_list_remove_item(allocated_nodes, item);
        } else { /** otherwise, add the slots for our job to the total */
            num_slots += node->node_slots;
        }

        /** go on to next item */
        item = next;
    }

    /* Sanity check to make sure we have resources available */
    if (0 == opal_list_get_size(allocated_nodes)) {
        /* so there are 3 reasons we could be erroring here:
         * 1. There were no nodes allocated to this job 
         * 2. The local node was the only one available and nolocal was passed 
         * 3. All the nodes were full */
        if(0 == nodelist_size) {
            opal_show_help("help-orte-rmaps-base.txt", 
                           "orte-rmaps-base:no-available-resources", true);
        } else if(nolocal) {
            opal_show_help("help-orte-rmaps-base.txt", 
                           "orte-rmaps-base:nolocal-no-available-resources", true);
        } else {
            opal_show_help("help-orte-rmaps-base.txt", 
                           "orte-rmaps-base:all-available-resources-used", true);
        }
        ORTE_ERROR_LOG(ORTE_ERR_TEMP_OUT_OF_RESOURCE);
        return ORTE_ERR_TEMP_OUT_OF_RESOURCE;
    }
    
    *total_num_slots = num_slots;
    
    return ORTE_SUCCESS;
}


/*
 * Query the registry for all nodes allocated to a specified job
 */

static int compare(opal_list_item_t **a, opal_list_item_t **b)
{
    orte_ras_proc_t *aa = *((orte_ras_proc_t **) a);
    orte_ras_proc_t *bb = *((orte_ras_proc_t **) b);

    return (aa->rank - bb->rank);
}

int orte_rmaps_base_get_target_procs(opal_list_t *procs)
{
    int rc;

    /* get the allocation for this job */
    if(ORTE_SUCCESS != (rc = orte_ras.proc_query(procs))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    opal_list_sort(procs, compare);
    return ORTE_SUCCESS;
}



/*
 * Create a sub-list of nodes to be used for user-specified mappings
 */
int orte_rmaps_base_get_mapped_targets(opal_list_t *mapped_node_list,
                                       orte_app_context_t *app,
                                       opal_list_t *master_node_list,
                                       orte_std_cntr_t *total_num_slots)
{
    orte_app_context_map_t** loc_map = app->map_data;
    opal_list_item_t *item;
    orte_ras_node_t *node, *new_node;
    char **mapped_nodes = NULL;
    int num_mapped_nodes = 0;
    orte_std_cntr_t j, k, num_slots=0;
    int rc;
    
    /** set default answer */
    *total_num_slots = 0;
    
    /* Accumulate all of the host name mappings */
    for(k = 0; k < app->num_map; ++k) {
        if ( ORTE_APP_CONTEXT_MAP_HOSTNAME == loc_map[k]->map_type ) {
            if(mapped_nodes == NULL) {
                mapped_nodes     = opal_argv_split(loc_map[k]->map_data, ',');
                num_mapped_nodes = opal_argv_count(mapped_nodes);
            }
            else { /* Append to the existing mapping */
                char ** mini_map    = opal_argv_split(loc_map[k]->map_data, ',');
                orte_std_cntr_t mini_num_map = opal_argv_count(mini_map);
                for (j = 0; j < mini_num_map; ++j) {
                    rc = opal_argv_append(&num_mapped_nodes, &mapped_nodes, mini_map[j]);
                    if (OPAL_SUCCESS != rc) {
                        ORTE_ERROR_LOG(rc);
                        return rc;
                    }
                }
                opal_argv_free(mini_map);
            }
        }
    }

    /** check to see that all the nodes in the specified mapping have been allocated
     * for our use - if not, then that's an error
     */
    if( !are_all_mapped_valid(mapped_nodes, num_mapped_nodes, master_node_list) ) {
        opal_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:not-all-mapped-alloc",
                       true, app->app, opal_argv_join(mapped_nodes, ','));
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /** setup the working node list to include only those nodes that were specified
     * in this mapping. We don't need to worry about nodes being fully used or not
     * since the master list only includes nodes that aren't.
     */
    for (item  = opal_list_get_first(master_node_list);
         item != opal_list_get_end(master_node_list);
         item  = opal_list_get_next(item) ) {
        node = (orte_ras_node_t*)item;
        
        if( is_mapped(item, mapped_nodes, num_mapped_nodes) ) {
            /** we can't just add this item to the mapped_node_list as it cannot be
             * on two lists at the same time, so we need to copy it first
             */
            if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&new_node, node, ORTE_RAS_NODE))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            opal_list_append(mapped_node_list, &new_node->super);
            num_slots += new_node->node_slots;
        }
    }

    /** check that anything is left! */
    if (0 == opal_list_get_size(mapped_node_list)) {
        opal_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:no-mapped-node",
                       true, app->num_procs, app->app);
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    *total_num_slots = num_slots;

    return ORTE_SUCCESS;
}


int orte_rmaps_base_add_proc_to_map(orte_job_map_t *map, orte_cellid_t cell, char *nodename, int32_t launch_id,
                                    char *username, bool oversubscribed, orte_mapped_proc_t *proc)
{
    opal_list_item_t *item;
    orte_mapped_node_t *node;
    
    for (item = opal_list_get_first(&map->nodes);
         item != opal_list_get_end(&map->nodes);
         item = opal_list_get_next(item)) {
        node = (orte_mapped_node_t*)item;
        
        if (cell == node->cell && 0 == strcmp(nodename, node->nodename)) {
            /* node was found - add this proc to that list */
            opal_list_append(&node->procs, &proc->super);
            /* set the oversubscribed flag */
            node->oversubscribed = oversubscribed;
            return ORTE_SUCCESS;
        }
    }
    
    /* node was NOT found - add this one to the list */
    node = OBJ_NEW(orte_mapped_node_t);
    if (NULL == node) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    node->cell = cell;
    node->nodename = strdup(nodename);
    if (NULL != username) {
        node->username = strdup(username);
    }
    node->launch_id = launch_id;
    node->oversubscribed = oversubscribed;
    opal_list_append(&map->nodes, &node->super);
    
    /* and add this proc to the new node's list of procs */
    opal_list_append(&node->procs, &proc->super);
    
    return ORTE_SUCCESS;
}


/*
 * Claim a slot for a specified job on a node
 */
int orte_rmaps_base_claim_slot(orte_job_map_t *map,
                               orte_ras_node_t *current_node,
                               orte_jobid_t jobid, orte_vpid_t vpid,
                               orte_std_cntr_t app_idx,
                               opal_list_t *nodes,
                               opal_list_t *fully_used_nodes,
                               bool oversubscribe)
{
    orte_process_name_t *name;
    orte_mapped_proc_t *proc;
    bool oversub;
    int rc;
    
    /* create mapped_proc object */
    proc = OBJ_NEW(orte_mapped_proc_t);
    if (NULL == proc) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* create the process name as an offset from the vpid-start */
    rc = orte_ns.create_process_name(&name, current_node->node_cellid,
                                     jobid, vpid);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(proc);
        return rc;
    }
    proc->name = *name;
    proc->rank = vpid;
    proc->app_idx = app_idx;
    
    /* Be sure to demarcate this slot as claimed for the node */
    current_node->node_slots_inuse++;
    
    /* see if this node is oversubscribed now */
    if (current_node->node_slots_inuse > current_node->node_slots) {
        oversub = true;
    } else {
        oversub = false;
    }
    
    /* add the proc to the map */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_add_proc_to_map(map, current_node->node_cellid,
                                                              current_node->node_name,
                                                              current_node->launch_id,
                                                              current_node->node_username,
                                                              oversub, proc))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(proc);
        return rc;
    }
    
    /* Remove this node if it has reached its max number of allocatable slots OR it has
     * reached the soft limit AND we are in a "no oversubscribe" state
     */
    if ((0 != current_node->node_slots_max  &&
        current_node->node_slots_inuse >= current_node->node_slots_max) ||
        (!oversubscribe && current_node->node_slots_inuse >= current_node->node_slots)) {
        opal_list_remove_item(nodes, (opal_list_item_t*)current_node);
        /* add it to the list of fully used nodes */
        opal_list_append(fully_used_nodes, &current_node->super);
        /** now return the proper code so the caller knows we removed the node! */
        return ORTE_ERR_NODE_FULLY_USED;
    }
    
    return ORTE_SUCCESS;
}


/*
 * Update the node allocations stored in the registry
 */
int orte_rmaps_base_update_node_usage(opal_list_t *nodes)
{
    opal_list_item_t* item;
    orte_gpr_value_t **values;
    int rc;
    orte_std_cntr_t num_values, i, j;
    orte_ras_node_t* node;
    
    num_values = (orte_std_cntr_t)opal_list_get_size(nodes);
    if (0 >= num_values) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    values = (orte_gpr_value_t**)malloc(num_values * sizeof(orte_gpr_value_t*));
    if (NULL == values) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    for (i=0; i < num_values; i++) {
        if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&(values[i]),
                                                        ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND,
                                                        ORTE_NODE_SEGMENT, 1, 0))) {
            ORTE_ERROR_LOG(rc);
            for (j=0; j < i; j++) {
                OBJ_RELEASE(values[j]);
            }
            free(values);
            return rc;
        }
    }
    
    for(i=0, item =  opal_list_get_first(nodes);
        i < num_values && item != opal_list_get_end(nodes);
        i++, item =  opal_list_get_next(item)) {
        node = (orte_ras_node_t*)item;
        
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[i]->keyvals[0]), ORTE_NODE_SLOTS_IN_USE_KEY,
                                                         ORTE_STD_CNTR, &(node->node_slots_inuse)))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* setup index/keys for this node */
        rc = orte_schema.get_node_tokens(&(values[i]->tokens), &(values[i]->num_tokens), node->node_cellid, node->node_name);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    
    /* try the insert */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(num_values, values))) {
        ORTE_ERROR_LOG(rc);
    }
    
cleanup:
    for (j=0; j < num_values; j++) {
        OBJ_RELEASE(values[j]);
    }
    if (NULL != values) free(values);
    
    return rc;
}


static int orte_find_unallocated_proc_in_map(orte_ras_proc_t *proc, orte_job_map_t *map, orte_mapped_proc_t **mproc)
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

int orte_rmaps_base_rearrange_map(orte_app_context_t *app, orte_job_map_t *map, opal_list_t *procs)
{
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
    return ORTE_SUCCESS;
}

int orte_rmaps_base_compute_usage(orte_job_map_t *map, orte_std_cntr_t num_procs)
{
    opal_list_item_t *item, *item2;
    orte_mapped_node_t *mnode;
    orte_mapped_proc_t *mproc, *psave = NULL;
    orte_vpid_t minv, local_rank;
    
    /* set the vpid range for the job */
    map->vpid_range = num_procs;
    
    /* set the number of total nodes involved */
    map->num_nodes = opal_list_get_size(&map->nodes);
    
    /* for each node being used by this job... */
    for (item = opal_list_get_first(&map->nodes);
         item != opal_list_get_end(&map->nodes);
         item = opal_list_get_next(item)) {
        mnode = (orte_mapped_node_t*)item;
        
        /* set the number of procs for this job on that node */
        mnode->num_procs = opal_list_get_size(&mnode->procs);
        
        /* cycle through the list of procs, looking for the minimum
         * vpid one and setting that local rank, until we have
         * done so for all procs on the node
         */
        /* init search values */
        local_rank = 0;
        
        while (local_rank < mnode->num_procs) {
            minv = ORTE_VPID_MAX;
            /* find the minimum vpid proc */
            for (item2 = opal_list_get_first(&mnode->procs);
                 item2 != opal_list_get_end(&mnode->procs);
                 item2 = opal_list_get_next(item2)) {
                mproc = (orte_mapped_proc_t*)item2;
            
                if (ORTE_VPID_INVALID != mproc->local_rank) {
                    /* already done this one */
                    continue;
                }
                
                if (mproc->rank < minv) {
                    minv = mproc->rank;
                    psave = mproc;
                }
            }
            
            psave->local_rank = local_rank;
            ++local_rank;
        }
    }
    
    return ORTE_SUCCESS;
}

int orte_rmaps_base_define_daemons(orte_job_map_t *map)
{
    opal_list_item_t *item;
    orte_mapped_node_t *node;
    orte_vpid_t vpid;
    orte_std_cntr_t num_daemons;
    char* dkeys[] = {
        ORTE_PROC_NAME_KEY,
        ORTE_NODE_NAME_KEY,
        ORTE_PROC_RML_IP_ADDRESS_KEY,
        NULL
    };
    orte_gpr_value_t **dvalues=NULL, *value;
    orte_gpr_keyval_t* keyval;
    orte_std_cntr_t v, kv, num_dvalues=0;
    char *node_name;
    orte_process_name_t *pptr;
    int rc;
    bool daemon_exists;
    
    /* save the default number of daemons we will need */
    num_daemons = map->num_nodes;
    
    /* with the new launch system based on xcast messages to all daemons,
     * there is no choice but to reuse existing daemons for dynamic spawns
     */

    /* get the current list of daemons off of the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
                                           "orte-job-0",
                                           NULL,
                                           dkeys,
                                           &num_dvalues,
                                           &dvalues))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* process the results, storing info in the mapped_node objects */
    for(v=0; v<num_dvalues; v++) {
        value = dvalues[v];
        node_name = NULL;
        daemon_exists = false;
        for(kv = 0; kv<value->cnt; kv++) {
            keyval = value->keyvals[kv];
            if(strcmp(keyval->key, ORTE_NODE_NAME_KEY) == 0) {
                /* use the dss.copy function here to protect us against zero-length strings */
                if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&node_name, keyval->value->data, ORTE_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                continue;
            }
            if (strcmp(keyval->key, ORTE_PROC_NAME_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&pptr, keyval->value, ORTE_NAME))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                continue;
            }
            if (strcmp(keyval->key, ORTE_PROC_RML_IP_ADDRESS_KEY) == 0) {
                /* we don't care about the value here - the existence of the key is
                 * enough to indicate that this daemon must already exist, so flag it
                 */
                daemon_exists = true;
                continue;
            }
        }
        if (NULL == node_name) continue;
        /* find this node on the map */
        for (item = opal_list_get_first(&map->nodes);
             item != opal_list_get_end(&map->nodes);
             item = opal_list_get_next(item)) {
            node = (orte_mapped_node_t*)item;
            if (strcmp(node->nodename, node_name) == 0) {
                /* got it! store the daemon name here */
                if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&node->daemon, pptr, ORTE_NAME))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                /* flag that this daemon already exists, if it does */
                if (daemon_exists) {
                    node->daemon_preexists = true;
                    /* decrease the number of daemons we will need to start */
                    --num_daemons;
                }
            }
        }
    }
    
    /* do we need to create any new ones? */
    if (0 >= num_daemons) {
        /* nope - we are done! */
        return ORTE_SUCCESS;
    }
    
    /* get a vpid range for the daemons still to be created */
    if (ORTE_SUCCESS != (rc = orte_ns.reserve_range(0, num_daemons, &vpid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* store the info in the map */
    map->num_new_daemons = num_daemons;
    map->daemon_vpid_start = vpid;
    
    /* for each node being used by this job... */
    for (item = opal_list_get_first(&map->nodes);
         item != opal_list_get_end(&map->nodes);
         item = opal_list_get_next(item)) {
        node = (orte_mapped_node_t*)item;
        
        /* if the daemon already exists...do nothing */
        if (node->daemon_preexists) continue;
        
        /* otherwise, create the daemon's process name and store it on the mapped_node... */
        if (ORTE_SUCCESS != (rc = orte_ns.create_process_name(&node->daemon, ORTE_PROC_MY_NAME->cellid,
                                                              0, vpid))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* ...and increment the vpid for the next one */
        ++vpid;
    }
    
    return ORTE_SUCCESS;
}
