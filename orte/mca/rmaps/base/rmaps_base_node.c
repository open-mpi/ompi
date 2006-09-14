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
int orte_rmaps_base_get_target_nodes(opal_list_t* nodes, orte_jobid_t jobid, orte_std_cntr_t *total_num_slots)
{
    opal_list_item_t *item, *next;
    orte_ras_node_t *node;
    int id, rc, nolocal;
    orte_std_cntr_t num_slots=0;

    /** set default answer */
    *total_num_slots = 0;
    
    if(ORTE_SUCCESS != (rc = orte_ras.node_query_alloc(nodes, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* If the "no local" option was set, then remove the local node
        from the list */
    
    id = mca_base_param_find("rmaps", NULL, "base_schedule_local");
    mca_base_param_lookup_int(id, &nolocal);
    if (0 == nolocal) {
        for (item  = opal_list_get_first(nodes);
             item != opal_list_get_end(nodes);
             item  = opal_list_get_next(item) ) {
            if (0 == strcmp(((orte_ras_node_t *) item)->node_name, 
                            orte_system_info.nodename) ||
                opal_ifislocal(((orte_ras_node_t *) item)->node_name)) {
                opal_list_remove_item(nodes, item);
                break;
            }
        }
    }

    /** remove all nodes that are already at max usage */
    item  = opal_list_get_first(nodes);
    while (item != opal_list_get_end(nodes)) {

        /** save the next pointer in case we remove this node */
        next  = opal_list_get_next(item);

        /** check to see if this node is fully used - remove if so */
        node = (orte_ras_node_t*)item;
        if (0 != node->node_slots_max && node->node_slots_inuse > node->node_slots_max) {
            opal_list_remove_item(nodes, item);
        } else { /** otherwise, add its slots to the total */
            num_slots += node->node_slots;
        }

        /** go on to next item */
        item = next;
    }

    /* Sanity check to make sure we have been allocated nodes */
    if (0 == opal_list_get_size(nodes)) {
        ORTE_ERROR_LOG(ORTE_ERR_TEMP_OUT_OF_RESOURCE);
        return ORTE_ERR_TEMP_OUT_OF_RESOURCE;
    }
    
    *total_num_slots = num_slots;
    
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


/*
 * Claim a slot for a specified job on a node
 */
int orte_rmaps_base_claim_slot(orte_rmaps_base_map_t *map,
                               orte_ras_node_t *current_node,
                               orte_jobid_t jobid, orte_vpid_t vpid,
                               int proc_index,
                               opal_list_t *nodes,
                               opal_list_t *fully_used_nodes)
{
    orte_rmaps_base_proc_t *proc;
    orte_process_name_t *proc_name;
    orte_rmaps_base_node_t *rmaps_node;
    int rc;
    
    /* create objects */
    rmaps_node = OBJ_NEW(orte_rmaps_base_node_t);
    if (NULL == rmaps_node) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    OBJ_RETAIN(current_node);
    rmaps_node->node = current_node;
    proc = OBJ_NEW(orte_rmaps_base_proc_t);
    if (NULL == proc) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(rmaps_node);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* create the process name as an offset from the vpid-start */
    rc = orte_ns.create_process_name(&proc_name, current_node->node_cellid,
                                     jobid, vpid);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
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
    
    /* Be sure to demarcate this slot as claimed for the node */
    current_node->node_slots_inuse++;
    
    /* Remove this node if it has reached its max number of allocatable slots OR it has
     * reached the soft limit AND we are in a "no oversubscribe" state
     */
    if ((0 != current_node->node_slots_max  &&
        current_node->node_slots_inuse >= current_node->node_slots_max) ||
        (!orte_rmaps_base.oversubscribe &&
         current_node->node_slots_inuse >= current_node->node_slots)) {
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
    
    num_values = opal_list_get_size(nodes);
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
