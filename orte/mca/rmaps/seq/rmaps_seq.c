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
#include "opal/util/trace.h"
#include "opal/util/argv.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/hostfile/hostfile.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "rmaps_seq.h"

static int orte_rmaps_seq_map(orte_job_t *jdata);

/* define the module */
orte_rmaps_base_module_t orte_rmaps_seq_module = {
    orte_rmaps_seq_map
};


/*
 * Sequentially map the ranks according to the placement in the
 * specified hostfile
 */
static int orte_rmaps_seq_map(orte_job_t *jdata)
{
    orte_job_map_t *map;
    orte_app_context_t *app, **apps;
    orte_std_cntr_t i, j;
    opal_list_item_t *item, *next, *cur_node_item;
    orte_node_t *node, *nd, **nodes;
    orte_vpid_t vpid;
    orte_std_cntr_t num_nodes;
    int rc;
    opal_list_t *default_node_list=NULL;
    opal_list_t *node_list=NULL;

    OPAL_OUTPUT_VERBOSE((1, orte_rmaps_base.rmaps_output,
                         "%s rmaps:seq mapping job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));

    /* conveniece def */
    map = jdata->map;
    apps = (orte_app_context_t**)jdata->apps->addr;
    nodes = (orte_node_t**)orte_node_pool->addr;
      
    /* if there is a default hostfile, go and get its ordered list of nodes */
    if (NULL != orte_default_hostfile) {
        default_node_list = OBJ_NEW(opal_list_t);
        if (ORTE_SUCCESS != (rc = orte_util_get_ordered_host_list(default_node_list, orte_default_hostfile))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
    }
    
    /* start at the beginning... */
    vpid = 0;
    
    /* cycle through the app_contexts, mapping them sequentially */
    for(i=0; i < jdata->num_apps; i++) {
        app = apps[i];

       /* for each app_context, if a hostfile was specified, then we let it
         * override what we may have obtained from the default hostfile
         */
        if (NULL != app->hostfile) {
            node_list = OBJ_NEW(opal_list_t);
            if (ORTE_SUCCESS != (rc = orte_util_get_ordered_host_list(node_list, app->hostfile))) {
                ORTE_ERROR_LOG(rc);
                goto error;
            }
        } else {
            node_list = default_node_list;
        }
        if (NULL == node_list || 0 == (num_nodes = (orte_std_cntr_t)opal_list_get_size(node_list))) {
            orte_show_help("help-orte-rmaps-base.txt",
                           "orte-rmaps-base:no-available-resources",
                           true);
            return ORTE_ERR_SILENT;
        }

        /* if a bookmark exists from some prior mapping, set us to start there */
        if (NULL != jdata->bookmark) {
            cur_node_item = NULL;
            /* find this node on the list */
            for (item = opal_list_get_first(node_list);
                 item != opal_list_get_end(node_list);
                 item = opal_list_get_next(item)) {
                node = (orte_node_t*)item;
                
                if (node->index == jdata->bookmark->index) {
                    cur_node_item = item;
                    break;
                }
            }
            /* see if we found it - if not, just start at the beginning */
            if (NULL == cur_node_item) {
                cur_node_item = opal_list_get_first(node_list); 
            }
        } else {
            /* if no bookmark, then just start at the beginning of the list */
            cur_node_item = opal_list_get_first(node_list);
        }

        /* if num_procs wasn't specified, set it now */
        if (0 == app->num_procs) {
            app->num_procs = num_nodes;
        }
        
        for (i=0; i < app->num_procs; i++) {
            /* see if any nodes remain unused and available. We need to do this check
             * each time since we may remove nodes from the list (as they become fully
             * used) as we cycle through the loop
             */
            if(0 >= opal_list_get_size(node_list) ) {
                /* Everything is at max usage! :( */
                orte_show_help("help-orte-rmaps-seq.txt", "orte-rmaps-seq:alloc-error",
                               true, app->num_procs, app->app);
                return ORTE_ERR_SILENT;
            }
            
            /* Save the next node we can use before claiming slots, since
             * we may need to prune the nodes list removing overused nodes.
             * Wrap around to beginning if we are at the end of the list
             */
            if (opal_list_get_end(node_list) == opal_list_get_next(cur_node_item)) {
                next = opal_list_get_first(node_list);
            }
            else {
                next = opal_list_get_next(cur_node_item);
            }
            
            /* find this node on the global array - this is necessary so
             * that our mapping gets saved on that array as the objects
             * returned by the hostfile function are -not- on the array
             */
            node = NULL;
            nd = (orte_node_t*)cur_node_item;
            for (j=0; j < orte_node_pool->size; j++) {
                if (NULL == nodes[j]) {
                    break;  /* nodes are left aligned, so stop when we hit a null */
                } 
                if (nodes[j]->allocate && 0 == strcmp(nd->name, nodes[j]->name)) {
                    node = nodes[j];
                    break;
                }
            }
            if (NULL == node) {
                /* wasn't found - that is an error */
                orte_show_help("help-orte-rmaps-seq.txt",
                               "orte-rmaps-seq:resource-not-found",
                               true, nd->name);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
            
            /* assign next vpid to this node - do NOT allow claim_slot to remove
             * an oversubscribed node from the list!
             */
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node,
                                                                 vpid, app->idx,
                                                                 node_list,
                                                                 jdata->map->oversubscribe,
                                                                 false))) {
                if (ORTE_ERR_NODE_FULLY_USED != rc) {
                    ORTE_ERROR_LOG(rc);
                    goto error;
                }
            }
            /* increment the vpid */
            vpid++;
            /* move to next node */
            cur_node_item = next;
        }

        /** track the total number of processes we mapped */
        jdata->num_procs += app->num_procs;
        
        /* update the bookmark */
        jdata->bookmark = (orte_node_t*)cur_node_item;
        
        /* cleanup the node list if it came from this app_context */
        if (node_list != default_node_list) {
            while(NULL != (item = opal_list_remove_first(node_list))) {
                OBJ_RELEASE(item);
            }
            OBJ_RELEASE(node_list);
        }
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

error:
    if (NULL != default_node_list) {
        while (NULL != (item = opal_list_remove_first(default_node_list))) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(default_node_list);
    }
    if (NULL != node_list) {
        while (NULL != (item = opal_list_remove_first(node_list))) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(node_list);
    }
    
    return rc;
}

