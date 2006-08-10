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
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 *                         Use is subject to license terms.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file:
 * Resource Allocation for Grid Engine
 */

#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <sys/systeminfo.h>
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/ras/gridengine/ras_gridengine.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/ras/base/ras_base_node.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"

/*
 * Local functions
 */
static int orte_ras_gridengine_allocate(orte_jobid_t jobid);
static int orte_ras_gridengine_discover(opal_list_t* nodelist,
    orte_app_context_t** context, size_t num_context);
static int orte_ras_gridengine_node_insert(opal_list_t* nodes);
static int orte_ras_gridengine_node_query(opal_list_t* nodes);
static int orte_ras_gridengine_deallocate(orte_jobid_t jobid);
static int orte_ras_gridengine_finalize(void);
static int get_slot_count(char* node_name, int* slot_cnt);
static int put_slot_keyval(orte_ras_node_t* node, int slot_cnt);
static int get_slot_keyval(orte_ras_node_t* node, int* slot_cnt);

/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_gridengine_module = {
    orte_ras_gridengine_allocate,
    orte_ras_gridengine_node_insert,
    orte_ras_gridengine_node_query,
    orte_ras_gridengine_deallocate,
    orte_ras_gridengine_finalize
};

/**
 *  Discover available (pre-allocated) nodes. Allocate the
 *  requested number of nodes/process slots to the job.
 *  
 */
static int orte_ras_gridengine_allocate(orte_jobid_t jobid)
{
    opal_list_t nodes;
    opal_list_item_t* item;
    int rc;
    orte_app_context_t **context = NULL;
    size_t i, num_context;
  
    /* get the context */
    rc = orte_rmgr_base_get_app_context(jobid, &context, &num_context);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* construct a node object and pass to discover to gather valid nodes */
    OBJ_CONSTRUCT(&nodes, opal_list_t);
    if(ORTE_SUCCESS != (rc =
        orte_ras_gridengine_discover(&nodes, context, num_context))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* call the base allocator to allocate the nodes to the jobid */
    if(ORTE_SUCCESS != (rc = orte_ras_base_allocate_nodes(jobid, &nodes))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if (mca_ras_gridengine_component.debug) {
        opal_output(0, "ras:gridengine: dumping the orte node segment"); 
        orte_gpr.dump_segment(ORTE_NODE_SEGMENT);
    }
    
  cleanup:
    while(NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);
    for(i=0; i<num_context; i++) {
        OBJ_RELEASE(context[i]);
    }
    if (NULL != context) {
        free(context);
    }
    return rc;
}

/**
 *  Discover the available resources. 
 *  - validate any nodes specified via hostfile/commandline
 *  - check for additional nodes that have already been allocated
 */
static int orte_ras_gridengine_discover(opal_list_t* nodelist,
    orte_app_context_t** context, size_t num_context)
{    
    char *pe_hostfile = getenv("PE_HOSTFILE");
    char buf[1024];
    int rc, gridengine_slot_cnt;
    opal_list_item_t* item;
    opal_list_t new_nodes;
    FILE *fp;
   
    /* query the nodelist from the registry */
    if(ORTE_SUCCESS != (rc = orte_ras_gridengine_node_query(nodelist))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* check the PE_HOSTFILE before continuing on */
    if (!(fp = fopen(pe_hostfile, "r"))) {
        opal_show_help("help-ras-gridengine.txt", "cannot-read-pe-hostfile",
            true, pe_hostfile, strerror(errno));
        rc = ORTE_ERROR;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* parse the pe_hostfile for hostname, slots, etc, then compare the
     * current node with a list of hosts in the nodelist, if the current
     * node is not found in nodelist, add it in */
    OBJ_CONSTRUCT(&new_nodes, opal_list_t);
    while (fgets(buf, sizeof(buf), fp)) {
        char *tok, *ptr = strtok_r(buf, " \n", &tok);
        char *num = strtok_r(NULL, " \n", &tok);
        char *queue = strtok_r(NULL, " \n", &tok);
        char *arch = strtok_r(NULL, " \n", &tok);
        orte_ras_node_t *node;
        
        /* is this node already in the list */ 
        for(item =  opal_list_get_first(nodelist);
            item != opal_list_get_end(nodelist);
            item =  opal_list_get_next(item)) {
            node = (orte_ras_node_t*)item;

            if(strcmp(node->node_name, ptr) == 0) {
                opal_output(mca_ras_gridengine_component.verbose,
                    "ras:gridengine: %s: node already in nodelist", node->node_name);
                break; /* break so that the current 'item' is in nodelist */
            }
        }
        
        /* If the current 'item' is already in the nodelist, then continue
         * with the while loop to check next node in the PE_HOSTFILE. */
        if(item != opal_list_get_end(nodelist)) {
            opal_output(mca_ras_gridengine_component.verbose,
                "ras:gridengine: checking next node in pe_hostfile");
            continue;
        }
        
        /* otherwise, it's a new node.  Then create a new node entry */
        node = OBJ_NEW(orte_ras_node_t);
        if (NULL == node) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        node->node_name = strdup(ptr);
        node->node_arch = strdup(arch);
        node->node_state = ORTE_NODE_STATE_UP;
        node->node_cellid = 0;
        node->node_slots_inuse = 0;
        node->node_slots_max = 0;
        if (ORTE_SUCCESS != (rc =
            get_slot_count(node->node_name, &gridengine_slot_cnt))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        node->node_slots = gridengine_slot_cnt;
        opal_list_append(&new_nodes, &node->super);

        /* put the gridengine slot into the gpr to use later */
        if (ORTE_SUCCESS != (rc = put_slot_keyval(node, gridengine_slot_cnt))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    } /* finished reading the $PE_HOSTFILE */
    
    /* adding new / undiscovered nodes to the registry */
    if(opal_list_get_size(&new_nodes)) {
        opal_output(mca_ras_gridengine_component.verbose,
            "ras:gridengine: adding new nodes to the registry");
        rc = orte_ras_gridengine_node_insert(&new_nodes);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    /* append new_nodes to the nodelist */
    while(NULL != (item = opal_list_remove_first(&new_nodes)))
        opal_list_append(nodelist, item);
           
    /* get the registry key of the remaining gridengine slot count for
     * each node. The keyval for each node tells us the number of
     * gridengine launches is left for each node in the nodelist */
    for(item =  opal_list_get_first(nodelist);
        item != opal_list_get_end(nodelist);
        item =  opal_list_get_next(item)) {
        orte_ras_node_t *node = (orte_ras_node_t*)item;
        int remain_slot_cnt;

        opal_output(mca_ras_gridengine_component.verbose,
            "ras:gridengine: %s: checking gpr key", node->node_name);
        
        if (ORTE_SUCCESS != (rc = get_slot_keyval(node, &remain_slot_cnt))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        opal_output(mca_ras_gridengine_component.verbose,
            "ras:gridengine: %s: remaining PE slots=%d",
            node->node_name, remain_slot_cnt);
        
        /* if the remaining gridengine slot reaches down to 0 for this node,
         * then remove this node from the nodelist */
        if (remain_slot_cnt == 0) {
            opal_output(mca_ras_gridengine_component.verbose,
                "ras:gridengine: %s: used up all PE slots, removing node",
                node->node_name);
            opal_list_remove_item(nodelist,item);
            OBJ_DESTRUCT(item);
        }
    }
    
    /* If there are no more nodes available in the nodelist, then quit this job
     * because otherwise, other RAS (like localhost) might be able to allocate
     * the resource and use the gridengine PLS to do the process launching with qrsh.
     * This will lead to failure eventually in gridengine PLS. */
    if(opal_list_get_size(nodelist) == 0) {
        opal_show_help("help-ras-gridengine.txt", "empty-nodelist-error", true);
        rc = ORTE_ERR_NOT_AVAILABLE;
        goto cleanup;
    }
    
  cleanup:
    OBJ_DESTRUCT(&new_nodes);
    return rc;
}

/**
 * Use this function to set the initial gridengine slot count for the given node
 * to the registry.
 */
static int put_slot_keyval(orte_ras_node_t* node, int slot_cnt)
{
    /* put our contact info into the registry */
    orte_data_value_t *put_value;
    int rc, ivalue;
    size_t num_tokens;
    char **tokens;
       
    opal_output(mca_ras_gridengine_component.verbose,
        "ras:gridengine: %s: putting PE slots=%d",
        node->node_name, slot_cnt);

    put_value = OBJ_NEW(orte_data_value_t);
    if (NULL == put_value) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    ivalue = slot_cnt;
    put_value->type = ORTE_INT;
    put_value->data = &ivalue;
    
    /* get token */
    if (ORTE_SUCCESS != (rc = orte_schema.get_node_tokens(&tokens,
        &num_tokens, node->node_cellid, node->node_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* put the keyval in the segment */
    if (ORTE_SUCCESS != (rc = orte_gpr.put_1(
        ORTE_GPR_OVERWRITE|ORTE_GPR_TOKENS_XAND,
            ORTE_NODE_SEGMENT,
            tokens,
            "orte-gridengine-slot-cnt",
            put_value
        ))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    return rc;
}

/**
 * Use this function to get the remaining gridengine slot count for the given
 * node. This will query the registry for the slot count by providing a
 * key and set the remaining slot count as a result.
 */
static int get_slot_keyval(orte_ras_node_t* node, int* slot_cnt) {
    char **tokens;
    size_t num_tokens, i, get_cnt=0;
    int rc, *iptr;
    orte_gpr_keyval_t *condition;
    orte_gpr_value_t** get_values;
    char *get_keys[] = {"orte-gridengine-slot-cnt", NULL};

    /* get token */
    if (ORTE_SUCCESS != (rc = orte_schema.get_node_tokens(&tokens,
        &num_tokens, node->node_cellid, node->node_name))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* setup condition/filter for query - return only processes that
     * are assigned to the specified node name */
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&condition,
        ORTE_NODE_NAME_KEY, ORTE_STRING, (void*)node->node_name))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* get the keyvalue from the node segment */
    if(ORTE_SUCCESS != (rc = orte_gpr.get_conditional(
        ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
            ORTE_NODE_SEGMENT,
            tokens,
            get_keys,
            1,
            &condition,
            &get_cnt,
            &get_values))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
     
    /* parse the response */
    for(i=0; i<get_cnt; i++) {
        orte_gpr_value_t* value = get_values[i];
        size_t k;

        /* looking in each GPR container for keyvals */
        for(k=0; k < value->cnt; k++) {
            orte_gpr_keyval_t* keyval = value->keyvals[k];
            if(strcmp(keyval->key, "orte-gridengine-slot-cnt") == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get(
                    (void**)&iptr, keyval->value, ORTE_INT))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                *slot_cnt = *iptr;
                free(iptr);
                opal_output(mca_ras_gridengine_component.verbose,
                    "ras:gridengine: %s: registry shows PE slots=%d",
                    node->node_name, *slot_cnt);
                continue;
            }
        }
    }
  cleanup:
    for(i=1; i<get_cnt; i++)
        OBJ_RELEASE(get_values[i]);
    if (NULL != get_values) free(get_values);
    opal_argv_free(tokens);

    return rc;
}

/**
 * Parse the PE_HOSTFILE to determine the number of process
 * slots/processors available on the node.
 */
static int get_slot_count(char* node_name, int* slot_cnt)
{   
    char buf[1024];
    char *pe_hostfile = getenv("PE_HOSTFILE");
    FILE *fp;
    
    /* check the PE_HOSTFILE before continuing on */
    if (!(fp = fopen(pe_hostfile, "r"))) {
        opal_show_help("help-ras-gridengine.txt", "cannot-read-pe-hostfile",
            true, pe_hostfile, strerror(errno));
        ORTE_ERROR_LOG(ORTE_ERROR);
        return(ORTE_ERROR);
    }
        
    while (fgets(buf, sizeof(buf), fp)) {
        char *tok, *name = strtok_r(buf, " \n", &tok);
        char *num = strtok_r(NULL, " \n", &tok);
        char *queue = strtok_r(NULL, " \n", &tok);
        char *arch = strtok_r(NULL, " \n", &tok);

        if(strcmp(node_name,name) == 0) {
            *slot_cnt = (int) strtol(num, (char **)NULL, 10);
            opal_output(mca_ras_gridengine_component.verbose,
                "ras:gridengine: %s: PE_HOSTFILE shows slots=%d",
                node_name, *slot_cnt);
            return ORTE_SUCCESS;
        }
    }

    /* when there is no match */
    return ORTE_ERROR;
}

/**
 * call the base class to insert nodes
 */
static int orte_ras_gridengine_node_insert(opal_list_t *nodes)
{
    return orte_ras_base_node_insert(nodes);
}

/**
 * call the base class to query nodes
 */
static int orte_ras_gridengine_node_query(opal_list_t *nodes)
{
    return orte_ras_base_node_query(nodes);
}

/**
 * call the base class to deallocate nodes
 */
static int orte_ras_gridengine_deallocate(orte_jobid_t jobid)
{
    /* Nothing to do */
    opal_output(mca_ras_gridengine_component.verbose,
        "ras:gridengine:deallocate: success (nothing to do)");
    return ORTE_SUCCESS;
}

/**
 * finalize
 */
static int orte_ras_gridengine_finalize(void)
{
    /* Nothing to do */
    opal_output(mca_ras_gridengine_component.verbose,
        "ras:gridengine:finalize: success (nothing to do)");
    return ORTE_SUCCESS;
}
