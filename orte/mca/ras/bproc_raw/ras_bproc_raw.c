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
#include <sys/bproc.h>

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

#include "orte/dss/dss.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_bproc_raw.h"


/**
 * Query the bproc node status 
 */

static int orte_ras_bproc_raw_node_state(int node)
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


/**
 * Discover available nodes. Allocate anything found
 * that is accessible by this user to the job.
 *  
 */

static int orte_ras_bproc_raw_allocate(orte_jobid_t jobid, opal_list_t *attributes)
{
    struct bproc_node_set_t ns = BPROC_EMPTY_NODESET;
    struct bproc_node_info_t *ni;
    opal_list_t nodes;
    orte_ras_node_t *node;
    opal_list_item_t* item;
    int rc;
    int i;

    /* get the list of all nodes in this cluster */
    if (bproc_nodelist(&ns) < 0) {
        opal_show_help("help-ras-broc-raw.txt", "nodelist-failed", true);
        return ORTE_ERR_NOT_AVAILABLE;
    }
    
    /* if no nodes available, let the user know and return error */
    if (0 == ns.size) {
        opal_show_help("help-ras-broc-raw.txt", "no-nodes-found", true);
        return ORTE_ERR_NOT_AVAILABLE;
    }
    
    /* setup to record the nodes */
    OBJ_CONSTRUCT(&nodes, opal_list_t);

    /* cycle through the list */
    for (i=0; i < ns.size; i++) {
        ni = &ns.node[i];
        
        /* check that the node is alive */
        if(orte_ras_bproc_raw_node_state(ni->node) != ORTE_NODE_STATE_UP) {
            /* if not, ignore this entry */
            continue;
        }
        
        /* are we allowed to execute on this node */
        if(bproc_access(ni->node, BPROC_X_OK) != 0) {
            /* if not, ignore this entry */
            continue;
        }
        
        /* okay, we have access and it is alive - create a new node entry */
        node = OBJ_NEW(orte_ras_node_t);
        asprintf(&node->node_name, "%d", ni->node);
        if (strcmp(ni->status, "up") == 0) {
            node->node_state = ORTE_NODE_STATE_UP;
        } else if (strcmp(ni->status, "down") == 0) {
            node->node_state = ORTE_NODE_STATE_DOWN;
        } else if (strcmp(ni->status, "boot") == 0) {
            node->node_state = ORTE_NODE_STATE_REBOOT;
        } else {
            node->node_state = ORTE_NODE_STATE_UNKNOWN;
        }
        /* RHC - until we can find some way of querying bproc for the number of
         * available slots, just assume two
         */
        node->node_slots = 2;
        opal_list_append(&nodes, &node->super);
    }
    
    /* add any newly discovered nodes to the registry */
    if (0 < opal_list_get_size(&nodes)) {
        rc = orte_ras_base_node_insert(&nodes); 
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    } else {
        /* we didn't find anything - report that and return error */
        opal_show_help("help-ras-broc-raw.txt", "no-nodes-avail", true);
        rc = ORTE_ERR_FATAL;
        goto cleanup;
    }
    
    /* now allocate them to this job */
    rc = orte_ras_base_allocate_nodes(jobid, &nodes);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }

cleanup:
    while (NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);

    bproc_nodeset_free(&ns);

    return rc;
}

static int orte_ras_bproc_raw_deallocate(orte_jobid_t jobid)
{
    return ORTE_SUCCESS;
}


static int orte_ras_bproc_raw_finalize(void)
{
    return ORTE_SUCCESS;
}


orte_ras_base_module_t orte_ras_bproc_raw_module = {
    orte_ras_bproc_raw_allocate,
    orte_ras_base_node_insert,
    orte_ras_base_node_query,
    orte_ras_base_node_query_alloc,
    orte_ras_base_node_lookup,
    orte_ras_bproc_raw_deallocate,
    orte_ras_bproc_raw_finalize
};

