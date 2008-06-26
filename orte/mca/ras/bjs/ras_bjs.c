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
#include "orte/constants.h"
#include "orte/types.h"

#include <unistd.h>
#include <string.h>
#include <sys/bproc.h>

#include "opal/class/opal_list.h"
#include "opal/util/output.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_bjs.h"

/* API functions */
static int allocate(opal_list_t *nodes);
static int finalize(void);

orte_ras_base_module_t orte_ras_bjs_module = {
    allocate,
    finalize
};


/**
 * Query the bproc node status 
 */

static int bjs_node_state(int node)
{
#if defined BPROC_API_VERSION && BPROC_API_VERSION >= 4
    char nodestatus[BPROC_STATE_LEN + 1];

    bproc_nodestatus(node, nodestatus, sizeof(nodestatus));
    if (strcmp(nodestatus, "up") == 0)
        return ORTE_NODE_STATE_UP;
    if (strcmp(nodestatus, "down") == 0)
        return ORTE_NODE_STATE_DOWN;
    if (strcmp(nodestatus, "boot") == 0)
        return ORTE_NODE_STATE_REBOOT;
    return ORTE_NODE_STATE_UNKNOWN;
#else
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
#endif
}


static int allocate(opal_list_t *nodes)
{
    char* nodelist;
    char* ptr;
    opal_list_item_t* item;
    orte_node_t *node;
    int rc;
    
    /* parse the node list and check node status/access */
    nodelist = getenv("NODES");
    if (NULL == nodelist) {
        return ORTE_ERR_NOT_AVAILABLE;
    }

    while(NULL != (ptr = strsep(&nodelist,","))) {
        orte_node_state_t node_state;
        int node_num;

        /* is this node already in the list */
        for(item =  opal_list_get_first(nodes);
            item != opal_list_get_end(nodes);
            item =  opal_list_get_next(item)) {
            node = (orte_node_t*)item;
            if(strcmp(node->name, ptr) == 0)
                break;
        }
        /* it if is in the list, then just increment the slot count */
        if(item != opal_list_get_end(nodes)) {
            node->slots++;
            continue;
        }
        
        /* convert to an int node number */
        if(sscanf(ptr, "%d", &node_num) != 1) {
            continue;
        }

        if(ORTE_NODE_STATE_UP != (node_state = bjs_node_state(node_num))) {
            opal_output(0, "error: a specified node (%d) is not up.\n", node_num);
            rc = ORTE_ERROR;
            goto cleanup;
        }
        if(bproc_access(node_num, BPROC_X_OK) != 0) {
            opal_output(0, "error: a specified node (%d) is not accessible.\n", node_num);
            rc = ORTE_ERROR;
            goto cleanup;
        }

        /* create a new node entry */
        node = OBJ_NEW(orte_node_t);
        node->name = strdup(ptr);
        node->state = node_state;
        node->slots = 1;
        opal_list_append(nodes, &node->super);
    }

cleanup:
    return rc;
}


static int finalize(void)
{
    return ORTE_SUCCESS;
}


