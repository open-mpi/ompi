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
#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "opal/util/show_help.h"
#include "opal/util/argv.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "rmaps_seq.h"



/*
 * Sequentially map the ranks according to the placement in the
 * specified hostfile
 */
static int orte_rmaps_seq_map(orte_job_t *jdata)
{
#if 0    
    orte_job_map_t *map;
    orte_app_context_t *app, **apps;
    orte_std_cntr_t i;
    opal_list_t node_list, procs;
    opal_list_item_t *item;
    orte_node_t *node;
    orte_vpid_t vpid_start;
    orte_std_cntr_t num_nodes, num_slots;
    int rc;
    orte_std_cntr_t slots_per_node;

    OPAL_TRACE(1);
    /* conveniece def */
    map = jdata->map;
    apps = (orte_app_context_t**)jdata->apps->addr;
    
    /* start at the beginning... */
    vpid_start = 0;

    /* cycle through the app_contexts, mapping them sequentially */
    for(i=0; i < jdata->num_apps; i++) {
        app = apps[i];

        /* if the number of processes wasn't specified, then we know there can be only
         * one app_context allowed in the launch, and that we are to launch it across
         * all available slots. We'll double-check the single app_context rule first
         */
        if (0 == app->num_procs && 1 < jdata->num_apps) {
            opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:multi-apps-and-zero-np",
                           true, jdata->num_apps, NULL);
            rc = ORTE_ERR_SILENT;
            goto error;
        }

        /* for each app_context, we have to get an ordered list of nodes
         * from the specified hostfile
         */
        OBJ_CONSTRUCT(&node_list, opal_list_t);
        if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app,
                                                                  map->policy))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
        num_nodes = (orte_std_cntr_t)opal_list_get_size(&node_list);

        /* if a bookmark exists from some prior mapping, set us to start there */
        if (NULL != jdata->bookmark) {
            cur_node_item = NULL;
            /* find this node on the list */
            for (item = opal_list_get_first(&node_list);
                 item != opal_list_get_end(&node_list);
                 item = opal_list_get_next(item)) {
                node = (orte_node_t*)item;
                
                if (node->index == jdata->bookmark->index) {
                    cur_node_item = item;
                    break;
                }
            }
            /* see if we found it - if not, just start at the beginning */
            if (NULL == cur_node_item) {
                cur_node_item = opal_list_get_first(&node_list); 
            }
        } else {
            /* if no bookmark, then just start at the beginning of the list */
            cur_node_item = opal_list_get_first(&node_list);
        }

        if (map->pernode && map->npernode == 1) {
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
                opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:per-node-and-too-many-procs",
                               true, app->num_procs, num_nodes, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
        } else if (map->pernode && map->npernode > 1) {
            /* first, let's check to see if there are enough slots/node to
             * meet the request - error out if not
             */
            slots_per_node = num_slots / num_nodes;
            if (map->npernode > slots_per_node) {
                opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:n-per-node-and-not-enough-slots",
                               true, map->npernode, slots_per_node, NULL);
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
                app->num_procs = map->npernode * num_nodes;
            } else if (app->num_procs > (map->npernode * num_nodes)) {
                opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:n-per-node-and-too-many-procs",
                               true, app->num_procs, map->npernode, num_nodes, num_slots, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
        } else if (0 == app->num_procs) {
            /** set the num_procs to equal the number of slots on these mapped nodes - if
            user has specified "-bynode", then set it to the number of nodes
            */
            if (map->policy == ORTE_RMAPS_BYNODE) {
                app->num_procs = num_nodes;
            } else if (map->policy == ORTE_RMAPS_BYSLOT) {
                app->num_procs = num_slots;
            } else if (map->policy == ORTE_RMAPS_BYUSER) {
                /* we can't handle this - it should have been set when we got
                 * the map info. If it wasn't, then we can only error out
                 */
                opal_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:no-np-and-user-map",
                               true, app->num_procs, map->npernode, num_nodes, num_slots, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
        }

        /** track the total number of processes we mapped */
        jdata->num_procs += app->num_procs;

        /* Make assignments */
        if (map->policy == ORTE_RMAPS_BYUSER) {
            rc = map_app_by_user_map(app, jdata, vpid_start, &node_list, &procs);         
        } else if (map->policy == ORTE_RMAPS_BYNODE) {
            rc = map_app_by_node(app, jdata, vpid_start, &node_list);
        } else {
            rc = map_app_by_slot(app, jdata, vpid_start, &node_list);
        }

        /* update the starting vpid for the next app_context */
        vpid_start += app->num_procs;
        
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }

        /* save the bookmark */
        jdata->bookmark = (orte_node_t*)cur_node_item;
        
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
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_define_daemons(map))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;

#if 0
    if(!opal_list_is_empty(&procs)){
        if(ORTE_SUCCESS != (rc = orte_rmaps_base_rearrange_map(app, map, &procs))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }
#endif

error:
    while(NULL != (item = opal_list_remove_first(&node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node_list);

    return rc;
#endif
    return ORTE_SUCCESS;
}

orte_rmaps_base_module_t orte_rmaps_seq_module = {
    orte_rmaps_seq_map
};

