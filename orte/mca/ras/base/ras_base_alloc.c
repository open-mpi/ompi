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

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ras/base/ras_private.h"

/*
 * Mark nodes as allocated on the registry
 */
int orte_ras_base_allocate_nodes(orte_jobid_t jobid, 
                                 opal_list_t* nodes)
{
    opal_list_item_t* item;
    int rc;

    /* Increment the allocation field on each node so we know that 
     * it has been allocated to us. No further logic needed, that is left to rmaps */
    for (item  = opal_list_get_first(nodes);
         item != opal_list_get_end(nodes);
         item  = opal_list_get_next(item)) {
        orte_ras_node_t* node = (orte_ras_node_t*)item;
        node->node_slots_alloc++;
    }

    rc = orte_ras_base_node_assign(nodes, jobid);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}
