/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <string.h>

#include "opal/util/if.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/threads/tsd.h"

#include "orte/types.h"
#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/hostfile/hostfile.h"
#include "orte/util/dash_host/dash_host.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/runtime/data_type_support/orte_dt_support.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"

/*
 * Query the registry for all nodes allocated to a specified app_context
 */
int orte_rmaps_base_get_target_nodes(opal_list_t *allocated_nodes, orte_std_cntr_t *total_num_slots,
                                     orte_app_context_t *app, orte_mapping_policy_t policy)
{
    opal_list_item_t *item, *next;
    orte_node_t *node, *nd;
    orte_std_cntr_t num_slots;
    orte_std_cntr_t i;
    int rc;

    /** set default answer */
    *total_num_slots = 0;
    
    /* if the hnp was allocated, include it unless flagged not to */
    if (orte_hnp_is_allocated && !(policy & ORTE_MAPPING_NO_USE_LOCAL)) {
        if (NULL != (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 0))) {
            if (ORTE_NODE_STATE_DO_NOT_USE == node->state) {
                /* clear this for future use, but don't include it */
                node->state = ORTE_NODE_STATE_UP;
            } else if (ORTE_NODE_STATE_NOT_INCLUDED != node->state) {
                OBJ_RETAIN(node);
                node->mapped = false;
                opal_list_append(allocated_nodes, &node->super);
            }
        }
    }
    
    /* add everything in the node pool that can be used - add them
     * in daemon order, which may be different than the order in the
     * node pool
     */
    if (0 == opal_list_get_size(allocated_nodes)) {
        /* the list is empty */
        nd = NULL;
    } else {
        nd = (orte_node_t*)opal_list_get_last(allocated_nodes);
    }
    for (i=1; i < orte_node_pool->size; i++) {
        if (NULL != (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
            /* ignore nodes that are marked as do-not-use for this mapping */
            if (ORTE_NODE_STATE_DO_NOT_USE == node->state) {
                /* reset the state so it can be used another time */
                node->state = ORTE_NODE_STATE_UP;
                continue;
            }
            if (ORTE_NODE_STATE_DOWN == node->state) {
                continue;
            }
            if (ORTE_NODE_STATE_NOT_INCLUDED == node->state) {
                /* not to be used */
                continue;
            }
            /* retain a copy for our use in case the item gets
             * destructed along the way
             */
            OBJ_RETAIN(node);
            node->mapped = false;
            if (NULL == nd || nd->daemon->name.vpid < node->daemon->name.vpid) {
                /* just append to end */
                opal_list_append(allocated_nodes, &node->super);
                nd = node;
            } else {
                /* starting from end, put this node in daemon-vpid order */
                while (node->daemon->name.vpid < nd->daemon->name.vpid) {
                    if (opal_list_get_begin(allocated_nodes) == opal_list_get_prev(&nd->super)) {
                        /* insert at beginning */
                        opal_list_prepend(allocated_nodes, &node->super);
                        continue;
                    }
                    nd = (orte_node_t*)opal_list_get_prev(&nd->super);
                }
                opal_list_insert_pos(allocated_nodes, &nd->super, &node->super);
            }
        }
    }

    /** check that anything is here */
    if (0 == opal_list_get_size(allocated_nodes)) {
        orte_show_help("help-orte-rmaps-base.txt",
                       "orte-rmaps-base:no-available-resources",
                       true);
        return ORTE_ERR_SILENT;
    }
    
    /* is there a default hostfile? */
    if (NULL != orte_default_hostfile) {
        /* yes - filter the node list through the file, removing
         * any nodes not in the file -or- excluded via ^
         */
        if (ORTE_SUCCESS != (rc = orte_util_filter_hostfile_nodes(allocated_nodes,
                                                                  orte_default_hostfile))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /** check that anything is here */
        if (0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt",
                           "orte-rmaps-base:no-available-resources",
                           true);
            return ORTE_ERR_SILENT;
        }
    }
    
    
    /* did the app_context contain a hostfile? */
    if (NULL != app && NULL != app->hostfile) {
        /* yes - filter the node list through the file, removing
         * any nodes not found in the file
         */
        if (ORTE_SUCCESS != (rc = orte_util_filter_hostfile_nodes(allocated_nodes,
                                                                  app->hostfile))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /** check that anything is here */
        if (0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:no-mapped-node",
                           true, app->app, app->hostfile);
            return ORTE_ERR_SILENT;
        }
    }
    
    
    /* did the app_context contain an add-hostfile? */
    if (NULL != app && NULL != app->add_hostfile) {
        /* yes - filter the node list through the file, removing
         * any nodes not found in the file
         */
        if (ORTE_SUCCESS != (rc = orte_util_filter_hostfile_nodes(allocated_nodes,
                                                                  app->add_hostfile))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /** check that anything is here */
        if (0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:no-mapped-node",
                           true, app->app, app->hostfile);
            return ORTE_ERR_SILENT;
        }
    }
    
    
    /* now filter the list through any -host specification */
    if (NULL != app && NULL != app->dash_host) {
        if (ORTE_SUCCESS != (rc = orte_util_filter_dash_host_nodes(allocated_nodes,
                                                                   app->dash_host))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /** check that anything is left! */
        if (0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:no-mapped-node",
                           true, app->app, "");
            return ORTE_ERR_SILENT;
        }
    }
    
    /* now filter the list through any add-host specification */
    if (NULL != app && NULL != app->add_host) {
        if (ORTE_SUCCESS != (rc = orte_util_filter_dash_host_nodes(allocated_nodes,
                                                                   app->add_host))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /** check that anything is left! */
        if (0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:no-mapped-node",
                           true, app->app, "");
            return ORTE_ERR_SILENT;
        }
    }
    
    /* finally, filter thru any resource constraints */
#if 0
    for (item = opal_list_get_first(&app->resource_constraints);
         item != opal_list_get_end(&app->resource_constraints);
         item = opal_list_get_next(item)) {
        req_res = (opal_sysinfo_value_t*)item;

        OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                             "%s CHECKING CONSTRAINT %s FOR APP %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             req_res->key, app->app));

        /* check against node values */
        item2 = opal_list_get_first(allocated_nodes);
        while (item2 != opal_list_get_end(allocated_nodes)) {
            next = opal_list_get_next(item2);
            node = (orte_node_t*)item2;
            found = false;
            for (item3 = opal_list_get_first(&node->resources);
                 item3 != opal_list_get_end(&node->resources);
                 item3 = opal_list_get_next(item3)) {
                ninfo = (opal_sysinfo_value_t*)item3;

                OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                     "%s COMPARING CONSTRAINT %s WITH RESOURCE %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), 
                                     req_res->key, ninfo->key));

                if (0 == strcmp(req_res->key, ninfo->key)) {
                    if (OPAL_STRING == req_res->type) {
                        OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                             "%s CHECKING RESOURCE %s:%s ON NODE %s:%s",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                             ninfo->key, req_res->data.str,
                                             node->name, ninfo->data.str));
                        /* there could be multiple hosts or host-types here */
                        vals = opal_argv_split(req_res->data.str, ',');
                        for (i=0; NULL != vals[i]; i++) {
                            if (0 == strncasecmp(vals[i], ninfo->data.str,
                                                 strlen(vals[i]))) {
                                found = true;
                                break;
                            }
                        }
                        opal_argv_free(vals);
                    } else {
                        OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                             "%s CHECKING RESOURCE %s:%ld ON NODE %s:%ld",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                             ninfo->key, (long)req_res->data.i64,
                                             node->name, (long)ninfo->data.i64));
                        if (req_res->data.i64 <= ninfo->data.i64) {
                            found = true;
                        }
                    }
                    break;
                }
            }
            OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                 "%s CONSTRAINT RESULTED IN %s NODE %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 found ? "RETAINING" : "REMOVING",
                                 node->name));
            if (!found) {
                opal_list_remove_item(allocated_nodes, item2);
                OBJ_RELEASE(item2);
            }
            item2 = next;
        }
    }
#endif

    /* if the app is NULL, then we are mapping daemons - so remove
     * all nodes that already have a daemon on them
     *
     * NOTE: it is okay if the final list is empty. It just means
     * that there are no new daemons to be launched for the
     * virtual machine
     */
    if (NULL == app) {
        item  = opal_list_get_first(allocated_nodes);
        while (item != opal_list_get_end(allocated_nodes)) {
            
            /** save the next pointer in case we remove this node */
            next  = opal_list_get_next(item);
            
            /** already have a daemon? */
            node = (orte_node_t*)item;
            if (NULL != node->daemon) {
                /* if this is the local node, keep it if requested */
                if (node->daemon->name.vpid == ORTE_PROC_MY_HNP->vpid &&
                    !(policy & ORTE_MAPPING_NO_USE_LOCAL)) {
                    item = next;
                    continue;
                }
                opal_list_remove_item(allocated_nodes, item);
                OBJ_RELEASE(item);  /* "un-retain" it */
            }
            
            /** go on to next item */
            item = next;
        }
        *total_num_slots = 0;
        return ORTE_SUCCESS;
    }
    
    /* remove all nodes that are already at max usage, and
     * compute the total number of allocated slots while
     * we do so
     */
    num_slots = 0;
    item  = opal_list_get_first(allocated_nodes);
    while (item != opal_list_get_end(allocated_nodes)) {
        /** save the next pointer in case we remove this node */
        next  = opal_list_get_next(item);

        /** check to see if this node is fully used - remove if so */
        node = (orte_node_t*)item;
        if (0 != node->slots_max && node->slots_inuse > node->slots_max) {
            opal_list_remove_item(allocated_nodes, item);
            OBJ_RELEASE(item);  /* "un-retain" it */
        } else { /** otherwise, add the slots for our job to the total */
            if (0 == node->slots_alloc) {
                /* always allocate at least one */
                num_slots++;
            } else {
                num_slots += node->slots_alloc;
            }
        }

        /** go on to next item */
        item = next;
    }

    /* Sanity check to make sure we have resources available */
    if (0 == num_slots) {
        orte_show_help("help-orte-rmaps-base.txt", 
                       "orte-rmaps-base:all-available-resources-used", true);
        return ORTE_ERR_SILENT;
    }
    
    *total_num_slots = num_slots;
    
    return ORTE_SUCCESS;
}
