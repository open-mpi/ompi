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
#include "orte/orte_types.h"

#include "opal/class/opal_list.h"
#include "opal/util/output.h"

#include "orte/dss/dss.h"
#include "orte/util/sys_info.h"
#include "orte/mca/ras/base/ras_private.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/proc_info.h"

#include "orte/mca/ras/localhost/ras_localhost.h"


/*
 * Local functions
 */
static int orte_ras_localhost_allocate(orte_jobid_t jobid, opal_list_t *attributes);
static int orte_ras_localhost_deallocate(orte_jobid_t jobid);
static int orte_ras_localhost_finalize(void);


/*
 * Local variables
 */
orte_ras_base_module_t orte_ras_localhost_module = {
    orte_ras_localhost_allocate,
    orte_ras_base_node_insert,
    orte_ras_base_node_query,
    orte_ras_base_node_query_alloc,
    orte_ras_base_node_lookup,
    orte_ras_localhost_deallocate,
    orte_ras_localhost_finalize
};


orte_ras_base_module_t *orte_ras_localhost_init(int* priority)
{
    /* if we are not an HNP, then we must not be selected */
    if (!orte_process_info.seed) {
        return NULL;
    }
    
    *priority = mca_ras_localhost_component.priority;
    return &orte_ras_localhost_module;
}


static int orte_ras_localhost_allocate(orte_jobid_t jobid, opal_list_t *attributes)
{
    bool empty;
    int ret;
    opal_list_t nodes;
    orte_ras_node_t *node;
    opal_list_item_t *item;

    /* If the node segment is not empty, do nothing */

    if (ORTE_SUCCESS != (ret = orte_ras_base_node_segment_empty(&empty))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    if (!empty) {
        opal_output(orte_ras_base.ras_output,
                    "orte:ras:localhost: node segment not empty; not doing anything");
        return ORTE_SUCCESS;
    }
        opal_output(orte_ras_base.ras_output,
                    "orte:ras:localhost: node segment empty; adding \"localhost\"");

    /* Ok, the node segment is empty -- so add a localhost node */

    node = OBJ_NEW(orte_ras_node_t);
    if (NULL == node) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    /* use the same name we got in orte_system_info so we avoid confusion in
     * the session directories
     */
    node->node_name = strdup(orte_system_info.nodename);
    node->node_arch = NULL;
    node->node_state = ORTE_NODE_STATE_UP;
    /* JMS: this should not be hard-wired to 0, but there's no
       other value to put it to [yet]... */
    node->node_cellid = 0;
    node->node_slots_inuse = 0;
    node->node_slots_max = 0;
    node->node_slots = 1;
    OBJ_CONSTRUCT(&nodes, opal_list_t);
    opal_list_append(&nodes, &node->super);

    /* Put it on the segment and allocate it */

    if (ORTE_SUCCESS !=
        (ret = orte_ras_base_node_insert(&nodes)) ||
        ORTE_SUCCESS != 
        (ret = orte_ras_base_allocate_nodes(jobid, &nodes))) {
        goto cleanup;
    }
    
    /* now indicate that there is uncertainty about the number of slots here,
        * so the launcher should use knowledge of the local number of processors to
        * override any oversubscription flags
        */
    ret = orte_ras_base_set_oversubscribe_override(jobid);
    if (ORTE_SUCCESS != ret) {
        goto cleanup;
    }
    
cleanup:
    item = opal_list_remove_first(&nodes);
    OBJ_RELEASE(item);
    OBJ_DESTRUCT(&nodes);

    /* All done */

    return ret;
}


static int orte_ras_localhost_deallocate(orte_jobid_t jobid)
{
    /* Nothing to do */

    opal_output(orte_ras_base.ras_output, 
                "ras:localhost:deallocate: success (nothing to do)");
    return ORTE_SUCCESS;
}


static int orte_ras_localhost_finalize(void)
{
    /* Nothing to do */

    opal_output(orte_ras_base.ras_output, 
                "ras:localhost:finalize: success (nothing to do)");
    return ORTE_SUCCESS;
}
