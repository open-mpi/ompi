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
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#include <errno.h>
#include <unistd.h>
#include <string.h>

#define SR1_PJOBS
#include <lsf/lsbatch.h>

#include "opal/util/argv.h"
#include "opal/util/show_help.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_lsf.h"


/*
 * Local functions
 */
static int allocate(orte_jobid_t jobid, opal_list_t *attributes);
static int deallocate(orte_jobid_t jobid);
static int finalize(void);


/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_lsf_module = {
    allocate,
    orte_ras_base_node_insert,
    orte_ras_base_node_query,
    orte_ras_base_node_query_alloc,
    orte_ras_base_node_lookup,
    orte_ras_base_proc_query_alloc,
    deallocate,
    finalize
};


static int allocate(orte_jobid_t jobid, opal_list_t *attributes)
{
    char **nodelist;
    opal_list_t nodes;
    opal_list_item_t *item;
    orte_ras_node_t *node;
    int i, count, rc, num_nodes;

    /* get the list of allocated nodes */
    if ((num_nodes = lsb_getalloc(&nodelist)) < 0) {
        opal_show_help("help-ras-lsf.txt", "nodelist-failed", true);
        return ORTE_ERR_NOT_AVAILABLE;
    }
    
    OBJ_CONSTRUCT(&nodes, opal_list_t);
    node = NULL;
    
    /* step through the list */
    for (count = i = 0; i < num_nodes; i++) {
        opal_output(0, "lsf got node: %s", nodelist[i]);
        /* is this a repeat of the current node? */
        if (NULL != node && 0 == strcmp(nodelist[i], node->node_name)) {
            /* it is a repeat - just bump the slot count */
            ++node->node_slots;
            opal_output(0, "lsf ras repeat -- slot count now %d",
                        node->node_slots);
            continue;
        }
        
        opal_output(0, "lsf ras new node");
        /* not a repeat - create a node entry for it */
        node = OBJ_NEW(orte_ras_node_t);
        node->node_name = strdup(nodelist[i]);
        node->launch_id = count++;
        node->node_slots_inuse = 0;
        node->node_slots_max = 0;
        node->node_slots = 1;
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
        opal_show_help("help-ras-lsf.txt", "no-nodes-avail", true);
        rc = ORTE_ERR_NOT_AVAILABLE;
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
    
    /* release the nodelist from lsf */
    opal_argv_free(nodelist);

    return rc;
}

static int deallocate(orte_jobid_t jobid)
{
    return ORTE_SUCCESS;
}


static int finalize(void)
{
    return ORTE_SUCCESS;
}
