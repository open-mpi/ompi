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
 *
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/dss/dss.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns_types.h"

#include "orte/mca/pls/base/pls_private.h"

int orte_pls_base_launch_on_existing_daemons(orte_job_map_t *map)
{
    orte_gpr_value_t **values;  /* the gpr initializes this to NULL */
    orte_gpr_keyval_t *kv;
    orte_std_cntr_t cnt, i;
    char *keys[] = {
        ORTE_NODE_NAME_KEY,
        NULL
    };
    opal_list_item_t *item2, *next;
    orte_mapped_node_t *node;
    orte_gpr_notify_data_t *ndat;
    bool found;
    char *nodename;
    int rc;
    
    
    /* query the daemon info */
    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
                                           "orte-job-0",   /* the daemon job segment */
                                           NULL, /* all containers */
                                           keys,
                                           &cnt, &values))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* if no daemons are around (except HNP), then don't worry about this */
    if (cnt <= 1) {
        rc = ORTE_SUCCESS;
        goto CLEANUP;
    }

    /* get here if some daemons, other than HNP, exist
     * go through the list, checking nodenames against what is in the
     * map. If nodes match, then construct and send an appropriate command
     * to that daemon to launch the local procs - remove that node structure
     * from the map so that the main launcher doesn't also try to start procs
     * on that node!
     */
    found = false;
    item2 = opal_list_get_first(&map->nodes);
    while (item2 != opal_list_get_end(&map->nodes)) {
        node = (orte_mapped_node_t*)item2;
        
        /* save the next position in case we remove this one */
        next = opal_list_get_next(item2);
        
        /* check the returned values and see if the nodenames match */
        for (i=0; i < cnt; i++) {
            kv = values[i]->keyvals[0];
            if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&nodename, kv->value, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            
            if (0 == strcmp(node->nodename, nodename)) {
                /* get the launch message only once - do it the first time
                 * through so all the nodes are still on the map!
                 */
                if (!found) {
                    if (ORTE_SUCCESS != (rc = orte_odls.get_add_procs_data(&ndat, map))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(ndat);
                        return rc;
                    }
                    /* indicate that at least one daemon was found */
                    found = true;
                }
                /* procs on this node will be taken care of, so remove it from
                 * the map list so the main launcher won't try to launch them
                 */
                opal_list_remove_item(&map->nodes, item2);
                OBJ_RELEASE(item2);
            }
        }
        
        /* move to next position */
        item2 = next;
    }
    
    if (!found) {
        /* if no daemons were reused, then just return */
        rc = ORTE_SUCCESS;
        goto CLEANUP;
    }
    
    /* launch any procs that are using existing daemons */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_add_local_procs(ndat))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(ndat);
    
CLEANUP:
    for (i=0; i < cnt; i++) {
        if (NULL != values[i]) OBJ_RELEASE(values[i]);
    }
    if (NULL != values) free(values);
    
    return rc;
}
