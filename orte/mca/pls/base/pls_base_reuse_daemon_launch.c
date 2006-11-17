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
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns_types.h"

#include "orte/mca/pls/base/pls_private.h"

int orte_pls_base_launch_on_existing_daemons(orte_job_map_t *map)
{
    opal_list_t avail_daemons;
    opal_list_item_t *item, *item2, *next;
    orte_pls_daemon_info_t *dmn, *newdmn;
    orte_mapped_node_t *node;
    opal_list_t used_daemons;
    orte_gpr_notify_data_t *ndat;
    bool found;
    int rc;
    
    OBJ_CONSTRUCT(&avail_daemons, opal_list_t);
    OBJ_CONSTRUCT(&used_daemons, opal_list_t);
   
    /* check for available daemons we could use */
    if (ORTE_SUCCESS != (rc = orte_pls_base_check_avail_daemons(&avail_daemons, map->job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* go through the list, checking nodenames against what is in the
     * map. If nodes match, then construct and send an appropriate command
     * to that daemon to launch the local procs - remove that node structure
     * from the map so that the main launcher doesn't also try to start procs
     * on that node!
     */
    
    found = false;
    while (NULL != (item = opal_list_remove_first(&avail_daemons))) {
        dmn = (orte_pls_daemon_info_t*)item;
        
        item2 = opal_list_get_first(&map->nodes);
        while (item2 != opal_list_get_end(&map->nodes)) {
            node = (orte_mapped_node_t*)item2;
            
            /* save the next position in case we remove this one */
            next = opal_list_get_next(item2);
            
            if (0 == strcmp(node->nodename, dmn->nodename)) {
                newdmn = OBJ_NEW(orte_pls_daemon_info_t);
                newdmn->cell = dmn->cell;
                newdmn->nodename = strdup(dmn->nodename);
                newdmn->active_job = map->job;
                orte_dss.copy((void**)&(newdmn->name), dmn->name, ORTE_NAME);
                opal_list_append(&used_daemons, &newdmn->super);
                /* get the launch message only once - do it the first time
                 * through so all the nodes are still on the map!
                 */
                if (!found) {
                    if (ORTE_SUCCESS != (rc = orte_odls.get_add_procs_data(&ndat, map))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(ndat);
                        return rc;
                    }                
                    found = true;
                }
                /* procs on this node will be taken care of, so remove it from
                 * the map list so the main launcher won't try to launch them
                 */
                opal_list_remove_item(&map->nodes, item2);
                OBJ_RELEASE(item2);
            }
            
            /* move to next position */
            item2 = next;
        }
    }
    
    if (!found) {
        /* if no daemons were reused, then just return */
        OBJ_DESTRUCT(&used_daemons);
        return ORTE_SUCCESS;
    }
    
    /* store the bootproxy records */
    orte_pls_base_store_active_daemons(&used_daemons);
    
    /* launch any procs that are using existing daemons */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_add_local_procs(&used_daemons, ndat))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    OBJ_RELEASE(ndat);
    
    /* cleanup */
    while (NULL != (item = opal_list_remove_first(&used_daemons))) OBJ_RELEASE(item);
    OBJ_DESTRUCT(&used_daemons);
    
    return ORTE_SUCCESS;
}
