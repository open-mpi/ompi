/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include <sys/bproc.h>

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/ras/base/base.h"
#include "mca/ras/base/ras_base_node.h"
#include "ras_bjs.h"


/**
 * Query the bproc node status 
 */

static int orte_ras_bjs_node_state(int node)
{
    switch(bproc_nodestatus(node)) {
    case bproc_node_up:
        return ORTE_NODE_STATE_UP;
    case bproc_node_down:
        return ORTE_NODE_STATE_DOWN;
    case bproc_node_boot:
        return ORTE_NODE_STATE_REBOOT;
    default:
        return ORTE_NODE_STATE_UNKNOWN;
    }
}


/**
 * Parse the NODELIST to determine the number of process
 * slots/processors available on the node.
 */

static size_t orte_ras_bjs_node_slots(char* node_name)
{
    static char** nodelist = NULL;
    char** ptr;
    size_t count = 0;
    if(nodelist == NULL)
        nodelist = ompi_argv_split(getenv("NODELIST"), ',');
    ptr = nodelist;
    while(ptr && *ptr) {
        if(strcmp(*ptr, node_name) == 0)
            count++;
        ptr++;
    }
    return count;
}


/**
 * Resolve the node name to node number.
 */

static int orte_ras_bjs_node_resolve(char* node_name, int* node_num)
{
    /* for now we expect this to be the node number */
    if(sscanf(node_name, "%d", node_num) != 1)
        return ORTE_NODE_ERROR;
    return ORTE_SUCCESS;
}


/**
 *  Discover the available resources. 
 *  - validate any nodes specified via hostfile/commandline
 *  - check for additional nodes that have already been allocated
 */

static int orte_ras_bjs_discover(ompi_list_t* nodelist)
{
    char* nodes;
    char* ptr;
    ompi_list_item_t* item;
    ompi_list_t new_nodes;
    int rc;

    /* query the nodelist from the registry */
    OBJ_CONSTRUCT(&new_nodes, ompi_list_t);
    if(ORTE_SUCCESS != (rc = orte_ras_base_node_query(nodelist)))
        return rc;

    /* validate that any user supplied nodes actually exist, etc. */
    for(item =  ompi_list_get_first(nodelist);
        item != ompi_list_get_end(nodelist);
        item =  ompi_list_get_next(item)) {
        int node_num;

        orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
        if(ORTE_SUCCESS != orte_ras_bjs_node_resolve(node->node_name, &node_num)) {
            ompi_output(0, "error: a specified node (%s) is invalid.\n", node->node_name);
            return ORTE_NODE_ERROR;
        }

        if(orte_ras_bjs_node_state(node_num) != ORTE_NODE_STATE_UP) {
            ompi_output(0, "error: a specified node (%s) is not up.\n", 
                node->node_name);
            return ORTE_NODE_DOWN;
        }

        if(bproc_access(node_num, BPROC_X_OK) != 0) {
            ompi_output(0, "error: a specified node (%s) is not accessible.\n", 
                node->node_name);
            return ORTE_NODE_ERROR;
        }

        /* try and determine the number of available slots */
        if(node->node_slots == 0) {
            node->node_slots_inuse = 0;
            node->node_slots_max = 0;
            node->node_slots = orte_ras_bjs_node_slots(node->node_name);
        }
    }

    /* parse the node list and check node status/access */
    nodes = getenv("NODES");
    if(NULL == nodes) {
        return ompi_list_get_size(nodelist) ? ORTE_SUCCESS : ORTE_ERR_NOT_AVAILABLE;
    }

    OBJ_CONSTRUCT(&new_nodes, ompi_list_t);
    while(NULL != (ptr = strsep(&nodes,","))) {
        orte_ras_base_node_t *node;
        orte_node_state_t node_state;
        int node_num;

        /* is this node already in the list */
        for(item =  ompi_list_get_first(nodelist);
            item != ompi_list_get_end(nodelist);
            item =  ompi_list_get_next(item)) {
            node = (orte_ras_base_node_t*)item;
            if(strcmp(node->node_name, ptr) == 0)
                break;
        }
        if(item != ompi_list_get_end(nodelist))
            continue;
        if(sscanf(ptr, "%d", &node_num) != 1) {
            continue;
        }

        if(ORTE_NODE_STATE_UP != (node_state = orte_ras_bjs_node_state(node_num))) {
            ompi_output(0, "error: a specified node (%d) is not up.\n", node_num);
            rc = ORTE_NODE_DOWN;
            goto cleanup;
        }
        if(bproc_access(node_num, BPROC_X_OK) != 0) {
            ompi_output(0, "error: a specified node (%d) is not accessible.\n", node_num);
            rc = ORTE_NODE_ERROR;
            goto cleanup;
        }

        /* create a new node entry */
        node = OBJ_NEW(orte_ras_base_node_t);
        node->node_name = strdup(ptr);
        node->node_state = node_state;
        node->node_cellid = 0;
        node->node_slots_inuse = 0;
        node->node_slots_max = 0;
        node->node_slots = orte_ras_bjs_node_slots(node->node_name);
        ompi_list_append(&new_nodes, &node->super);
    }

    /* add any newly discovered nodes to the registry */
    rc = orte_ras_base_node_insert(&new_nodes);

    /* append them to the nodelist */
    while(NULL != (item = ompi_list_remove_first(&new_nodes)))
        ompi_list_append(nodelist, item);

cleanup:
    OBJ_DESTRUCT(&new_nodes);
    return rc;
}


/**
 *  Discover available (pre-allocated) nodes. Allocate the
 *  requested number of nodes/process slots to the job.
 *  
 */

static int orte_ras_bjs_allocate(orte_jobid_t jobid)
{
    ompi_list_t nodes;
    ompi_list_item_t* item;
    int rc;

    OBJ_CONSTRUCT(&nodes, ompi_list_t);
    if(ORTE_SUCCESS != (rc = orte_ras_bjs_discover(&nodes))) {
        return rc;
    }
    rc = orte_ras_base_allocate_nodes(jobid, &nodes);

    while(NULL != (item = ompi_list_remove_first(&nodes)))
        OBJ_RELEASE(item);
    OBJ_DESTRUCT(&nodes);
    return rc;
}



static int orte_ras_bjs_deallocate(orte_jobid_t jobid)
{
    return ORTE_SUCCESS;
}


static int orte_ras_bjs_finalize(void)
{
    return ORTE_SUCCESS;
}


orte_ras_base_module_t orte_ras_bjs_module = {
    orte_ras_bjs_allocate,
    orte_ras_bjs_deallocate,
    orte_ras_bjs_finalize
};

