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
#include <unistd.h>
#include <string.h>

#include "opal/util/argv.h"
#include "orte/orte_constants.h"
#include "orte/mca/ras/poe/ras_poe.h"
#include "orte/mca/ras/base/ras_private.h"

static int orte_ras_poe_allocate(orte_jobid_t jobid)
{
    char *poe_node_str;
    char **names; 
    int i, ret, nnode;
    opal_list_t nodes_list;
    orte_ras_node_t *node;
    opal_list_item_t* item;

    poe_node_str = getenv("LOADL_PROCESSOR_LIST");
    if (NULL == poe_node_str) {
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* poe_node_str is something like "nodeA nodeA nodeB nodeB" */

    names = opal_argv_copy(opal_argv_split(poe_node_str, ' '));
    nnode = opal_argv_count(names);

    /* iterate through all the nodes listed. If a single node is listed more than once,
     * it means that we have multiple slots allocated on that node */ 
    OBJ_CONSTRUCT(&nodes_list, opal_list_t);
    for (i = 0; i < nnode; i++) {
        /* check for duplicated nodes */
        for (item = opal_list_get_first(&nodes_list); 
             opal_list_get_end(&nodes_list) != item;
             item = opal_list_get_next(item)) {
             node = (orte_ras_node_t*) item;
             if (0 == strcmp(node->node_name, names[i])) {
                ++node->node_slots;
                break;
            }
        }
        
        if(opal_list_get_end(&nodes_list) == item) {
            /* we did not find a duplicate, so add a new item to the list */
            node = OBJ_NEW(orte_ras_node_t);
            if (NULL == node) {
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            node->node_name = strdup(names[i]);
            node->node_arch = NULL;
            node->node_state = ORTE_NODE_STATE_UP;
            node->node_cellid = 0;
            node->node_slots_inuse = 0;
            node->node_slots_max = 0;
            node->node_slots = 1;
            opal_list_append(&nodes_list, &node->super);
        }
    }
    ret = orte_ras_base_node_insert(&nodes_list);
    ret = orte_ras_base_allocate_nodes(jobid, &nodes_list);

    while (NULL != (item = opal_list_remove_first(&nodes_list))) { 
        OBJ_RELEASE(item);    
    }
    OBJ_DESTRUCT(&nodes_list);
    return ret;
}

static int orte_ras_poe_deallocate(orte_jobid_t jobid)
{
    return ORTE_SUCCESS;
}

static int orte_ras_poe_finalize(void)
{
    return ORTE_SUCCESS;
}

orte_ras_base_module_t orte_ras_poe_module = {
    orte_ras_poe_allocate,
    orte_ras_base_node_insert,
    orte_ras_base_node_query,
    orte_ras_base_node_query_alloc,
    orte_ras_base_node_lookup,
    orte_ras_poe_deallocate,
    orte_ras_poe_finalize
};
