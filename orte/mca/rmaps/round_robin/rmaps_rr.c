/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "opal/mca/base/mca_base_param.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "rmaps_rr.h"


/*
 * Create a round-robin mapping for the job.
 */
static int orte_rmaps_rr_map(orte_job_t *jdata)
{
    orte_app_context_t *app;
    int i;
    opal_list_t node_list;
    opal_list_item_t *item;
    orte_vpid_t vpid_start;
    orte_std_cntr_t num_nodes, num_slots;
    int rc;
    orte_std_cntr_t slots_per_node;
    int ppn = 0;
    opal_list_item_t *cur_node_item;
    
    /* start at the beginning... */
    vpid_start = 0;
    jdata->num_procs = 0;
    
    /* if loadbalancing is requested, then we need to compute
     * the #procs/node - note that this cannot be done
     * if we are doing pernode or if #procs was not given
     */
    if (orte_rmaps_base.loadbalance && !jdata->map->pernode) {
        float res;
        /* compute total #procs we are going to add */
        for(i=0; i < jdata->apps->size; i++) {
            if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
                continue;
            }
            if (0 == app->num_procs) {
                /* can't do it - tell user and quit */
                orte_show_help("help-orte-rmaps-rr.txt",
                               "orte-rmaps-rr:loadbalance-and-zero-np",
                               true);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
            ppn += app->num_procs;
        }
        /* get the total avail nodes and the number
         * of procs already using them
         */
        num_nodes=0;
        for (i=0; i < orte_node_pool->size; i++) {
            if (NULL == opal_pointer_array_get_item(orte_node_pool, i)) {
                continue;
            } 
            num_nodes++;
        }
        /* compute the balance */
        res = ((float)ppn / num_nodes);
        ppn = ppn / num_nodes;
        if (0 < (res-ppn)) {
            ppn++;
        }
    }

    /* cycle through the app_contexts, mapping them sequentially */
    for(i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        
        /* if the number of processes wasn't specified, then we know there can be only
         * one app_context allowed in the launch, and that we are to launch it across
         * all available slots. We'll double-check the single app_context rule first
         */
        if (0 == app->num_procs && 1 < jdata->num_apps) {
            orte_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:multi-apps-and-zero-np",
                           true, jdata->num_apps, NULL);
            rc = ORTE_ERR_SILENT;
            goto error;
        }

        /* for each app_context, we have to get the list of nodes that it can
         * use since that can now be modified with a hostfile and/or -host
         * option
         */
        OBJ_CONSTRUCT(&node_list, opal_list_t);
        if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app,
                                                                  jdata->map->policy))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
        num_nodes = (orte_std_cntr_t)opal_list_get_size(&node_list);

        /* if a bookmark exists from some prior mapping, set us to start there */
        cur_node_item = orte_rmaps_base_get_starting_point(&node_list, jdata);
        
        if (jdata->map->pernode && jdata->map->npernode == 1) {
            /* there are three use-cases that we need to deal with:
            * (a) if -np was not provided, then we just use the number of nodes
            * (b) if -np was provided AND #procs > #nodes, then error out
            * (c) if -np was provided AND #procs <= #nodes, then launch
            *     the specified #procs one/node. In this case, we just
            *     leave app->num_procs alone
            */
            if (0 == app->num_procs) {
                app->num_procs = num_nodes;
            } else if (app->num_procs > num_nodes) {
                orte_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:per-node-and-too-many-procs",
                               true, app->num_procs, num_nodes, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
        } else if (jdata->map->pernode && jdata->map->npernode > 1) {
            /* first, let's check to see if there are enough slots/node to
             * meet the request - error out if not
             */
            slots_per_node = num_slots / num_nodes;
            if (jdata->map->npernode > slots_per_node) {
                orte_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:n-per-node-and-not-enough-slots",
                               true, jdata->map->npernode, slots_per_node, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
            /* there are three use-cases that we need to deal with:
            * (a) if -np was not provided, then we just use the n/node * #nodes
            * (b) if -np was provided AND #procs > (n/node * #nodes), then error out
            * (c) if -np was provided AND #procs <= (n/node * #nodes), then launch
            *     the specified #procs n/node. In this case, we just
            *     leave app->num_procs alone
            */
            if (0 == app->num_procs) {
                /* set the num_procs to equal the specified num/node * the number of nodes */
                app->num_procs = jdata->map->npernode * num_nodes;
            } else if (app->num_procs > (jdata->map->npernode * num_nodes)) {
                orte_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:n-per-node-and-too-many-procs",
                               true, app->num_procs, jdata->map->npernode, num_nodes, num_slots, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
        } else if (0 == app->num_procs) {
            if (jdata->map->policy & ORTE_RMAPS_BYUSER) {
                /* we can't handle this - it should have been set when we got
                 * the map info. If it wasn't, then we can only error out
                 */
                orte_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:no-np-and-user-map",
                               true, app->num_procs, jdata->map->npernode, num_nodes, num_slots, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
            /** set the num_procs to equal the number of slots on these mapped nodes */
            app->num_procs = num_slots;
        }
        
        /** track the total number of processes we mapped */
        jdata->num_procs += app->num_procs;

        /* Make assignments */
        if (jdata->map->policy & ORTE_RMAPS_BYUSER) {
            rc = ORTE_ERR_NOT_IMPLEMENTED;
            goto error;
        } else if (jdata->map->policy & ORTE_RMAPS_BYNODE) {
            rc = orte_rmaps_base_map_bynode(jdata, app, &node_list,
                                            app->num_procs, vpid_start,
                                            cur_node_item);
        } else {
            rc = orte_rmaps_base_map_byslot(jdata, app, &node_list,
                                            app->num_procs, vpid_start,
                                            cur_node_item, ppn);
        }

        /* update the starting vpid for the next app_context */
        vpid_start += app->num_procs;
        
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }

        /* cleanup the node list - it can differ from one app_context
         * to another, so we have to get it every time
         */
        while(NULL != (item = opal_list_remove_first(&node_list))) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&node_list);
    }

    /* compute and save convenience values */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_usage(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* define the daemons that we will use for this job */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_define_daemons(jdata->map))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;

error:
    while(NULL != (item = opal_list_remove_first(&node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node_list);

    return rc;
}

orte_rmaps_base_module_t orte_rmaps_round_robin_module = {
    orte_rmaps_rr_map
};

