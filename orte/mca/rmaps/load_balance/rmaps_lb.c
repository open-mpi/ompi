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
#include "rmaps_lb.h"

static int switchyard(orte_job_t *jdata);

orte_rmaps_base_module_t orte_rmaps_load_balance_module = {
    switchyard
};

/* Local functions */
static int npernode(orte_job_t *jdata);
static int nperboard(orte_job_t *jdata);
static int npersocket(orte_job_t *jdata);
static int loadbalance(orte_job_t *jdata);

static int switchyard(orte_job_t *jdata)
{
    int rc;
    
    if (0 < orte_rmaps_base.npernode) {
        rc = npernode(jdata);
    } else if (0 < orte_rmaps_base.nperboard) {
        rc = nperboard(jdata);
    } else if (0 < orte_rmaps_base.npersocket) {
        rc = npersocket(jdata);
    } else {
        rc = loadbalance(jdata);
    }

    if (ORTE_SUCCESS != rc) {
        return rc;
    }
    
    /* compute and save local ranks */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_local_ranks(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* define the daemons that we will use for this job */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_define_daemons(jdata->map))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}


/* place specified #procs on each node, up to the specified total
 * number of procs (if one was given).
 */
static int npernode(orte_job_t *jdata)
{
    orte_app_context_t *app;
    int i, j, rc=ORTE_SUCCESS;
    opal_list_t node_list;
    opal_list_item_t *item;
    orte_std_cntr_t num_slots;
    orte_node_t *node;
    int total_procs=0, np;
    
    /* setup the node list */
    OBJ_CONSTRUCT(&node_list, opal_list_t);
    
    /* loop through the app_contexts */
    for(i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        /* use the number of procs if one was given */
        if (0 < app->num_procs) {
            np = app->num_procs;
        } else {
            np = INT_MAX;
        }
        total_procs = 0;
        /* for each app_context, we have to get the list of nodes that it can
         * use since that can now be modified with a hostfile and/or -host
         * option
         */
        if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app,
                                                                  jdata->map->policy))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
        /* loop through the list of nodes */
        while (NULL != (item = opal_list_remove_first(&node_list))) {
            node = (orte_node_t*)item;
            /* put the specified number of procs on each node */
            for (j=0; j < orte_rmaps_base.npernode && total_procs < np; j++) {
                if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node,
                                                                     jdata->map->cpus_per_rank, app->idx,
                                                                     &node_list, jdata->map->oversubscribe,
                                                                     false, NULL))) {
                    /** if the code is ORTE_ERR_NODE_FULLY_USED, and we still have
                     * more procs to place, then that is an error
                     */
                    if (ORTE_ERR_NODE_FULLY_USED != rc ||
                        j < orte_rmaps_base.npernode-1) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(node);
                        goto error;
                    }
                }
                total_procs++;
            }
            OBJ_RELEASE(node);
        }
        /* compute vpids and add proc objects to the job - this has to be
         * done after each app_context is mapped in order to keep the
         * vpids contiguous within an app_context
         */
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_vpids(jdata))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    jdata->num_procs = total_procs;

error:
    while (NULL != (item = opal_list_remove_first(&node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node_list);
    return rc;
}

static int nperboard(orte_job_t *jdata)
{
    orte_app_context_t *app;
    int i, j, k, rc=ORTE_SUCCESS;
    opal_list_t node_list;
    opal_list_item_t *item;
    orte_std_cntr_t num_slots;
    orte_node_t *node;
    int total_procs=0, np;

    /* setup the node list */
    OBJ_CONSTRUCT(&node_list, opal_list_t);
    
    /* loop through the app_contexts */
    for(i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        /* use the number of procs if one was given */
        if (0 < app->num_procs) {
            np = app->num_procs;
        } else {
            np = INT_MAX;
        }
        total_procs = 0;
        /* for each app_context, we have to get the list of nodes that it can
         * use since that can now be modified with a hostfile and/or -host
         * option
         */
        if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app,
                                                                  jdata->map->policy))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
        /* loop through the list of nodes */
        while (NULL != (item = opal_list_remove_first(&node_list))) {
            node = (orte_node_t*)item;
            /* loop through the number of boards in this node */
            for (k=0; k < node->boards && total_procs < np; k++) {
                /* put the specified number of procs on each board */
                for (j=0; j < orte_rmaps_base.nperboard && total_procs < np; j++) {
                    if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node,
                                                                         jdata->map->cpus_per_rank, app->idx,
                                                                         &node_list, jdata->map->oversubscribe,
                                                                         false, NULL))) {
                        /** if the code is ORTE_ERR_NODE_FULLY_USED, and we still have
                         * more procs to place, then that is an error
                         */
                        if (ORTE_ERR_NODE_FULLY_USED != rc ||
                            j < orte_rmaps_base.nperboard-1) {
                            ORTE_ERROR_LOG(rc);
                            OBJ_RELEASE(node);
                            goto error;
                        }
                    }
                    total_procs++;
                }
            }
            OBJ_RELEASE(node);
        }
        /* compute vpids and add proc objects to the job - this has to be
         * done after each app_context is mapped in order to keep the
         * vpids contiguous within an app_context
         */
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_vpids(jdata))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    jdata->num_procs = total_procs;

error:
    while (NULL != (item = opal_list_remove_first(&node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node_list);
    return rc;
}


static int npersocket(orte_job_t *jdata)
{
    orte_app_context_t *app;
    int i, j, k, n, rc=ORTE_SUCCESS;
    opal_list_t node_list;
    opal_list_item_t *item;
    orte_std_cntr_t num_slots;
    orte_node_t *node;
    int total_procs=0, np;

    /* setup the node list */
    OBJ_CONSTRUCT(&node_list, opal_list_t);
    
    /* loop through the app_contexts */
    for(i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        /* use the number of procs if one was given */
        if (0 < app->num_procs) {
            np = app->num_procs;
        } else {
            np = INT_MAX;
        }
        total_procs = 0;
        /* for each app_context, we have to get the list of nodes that it can
         * use since that can now be modified with a hostfile and/or -host
         * option
         */
        if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app,
                                                                  jdata->map->policy))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
        /* loop through the list of nodes */
        while (NULL != (item = opal_list_remove_first(&node_list))) {
            node = (orte_node_t*)item;
            /* loop through the number of boards in this node */
            for (k=0; k < node->boards && total_procs < np; k++) {
                /* loop through the number of sockets/board */
                for (n=0; n < node->sockets_per_board && total_procs < np; n++) {
                    /* put the specified number of procs on each socket */
                    for (j=0; j < orte_rmaps_base.npersocket && total_procs < np; j++) {
                        if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node,
                                                                             jdata->map->cpus_per_rank, app->idx,
                                                                             &node_list, jdata->map->oversubscribe,
                                                                             false, NULL))) {
                            /** if the code is ORTE_ERR_NODE_FULLY_USED, and we still have
                             * more procs to place, then that is an error
                             */
                            if (ORTE_ERR_NODE_FULLY_USED != rc ||
                                j < orte_rmaps_base.npersocket-1) {
                                ORTE_ERROR_LOG(rc);
                                OBJ_RELEASE(node);
                                goto error;
                            }
                        }
                        /* track the number of procs */
                        total_procs++;
                    }
                }
            }
            OBJ_RELEASE(node);
        }
        /* compute vpids and add proc objects to the job - this has to be
         * done after each app_context is mapped in order to keep the
         * vpids contiguous within an app_context
         */
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_vpids(jdata))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    jdata->num_procs = total_procs;
    
error:
    while (NULL != (item = opal_list_remove_first(&node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node_list);
    return rc;
}


/*
 * Create a load balanced mapping for the job by assigning a constant #procs/node, with
 * leftovers being spread one/node starting from the first node.
 */
static int loadbalance(orte_job_t *jdata)
{
    orte_app_context_t *app;
    int i, j;
    opal_list_t node_list;
    orte_std_cntr_t num_nodes, num_slots;
    int rc=ORTE_SUCCESS, total_procs=0;
    int ppn = 0;
    opal_list_item_t *item, *start;
    orte_node_t *node;

    /* setup */
    OBJ_CONSTRUCT(&node_list, opal_list_t);

    /* compute total #procs we are going to add and the total number of nodes available */
    for(i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        /* get the nodes and #slots available for this app_context */
        if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app,
                                                                  jdata->map->policy))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
        if (0 == app->num_procs) {
            /* set the num_procs to the #slots */
            app->num_procs = num_slots;
        }
        num_nodes = opal_list_get_size(&node_list);
        /* compute the base ppn */
        ppn = app->num_procs / num_nodes;
        /* if a bookmark exists from some prior mapping, set us to start there */
        start = orte_rmaps_base_get_starting_point(&node_list, jdata);
        /* loop through the list of nodes until we either assign all the procs
         * or return to the starting point
         */
        total_procs = 0;
        item = start;
        do {
            node = (orte_node_t*)item;
            /* put the specified number of procs on each node */
            for (j=0; j < ppn; j++) {
                if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node,
                                                                     jdata->map->cpus_per_rank, app->idx,
                                                                     &node_list, jdata->map->oversubscribe,
                                                                     false, NULL))) {
                    /** if the code is ORTE_ERR_NODE_FULLY_USED, and we still have
                     * more procs to place, then that is an error
                     */
                    if (ORTE_ERR_NODE_FULLY_USED != rc ||
                        j < ppn-1) {
                        ORTE_ERROR_LOG(rc);
                        goto error;
                    }
                }
                total_procs++;
            }
            /* move to next node */
            if (opal_list_get_end(&node_list) == opal_list_get_next(item)) {
                item = opal_list_get_first(&node_list);
            }
            else {
                item = opal_list_get_next(item);
            }
        } while (item != start);
        
        /* save the bookmark */
        jdata->bookmark = node;

        /* if we haven't assigned all the procs, then loop through the list
         * again, assigning 1 per node until all are assigned
         */
        item = start;
        while (total_procs < app->num_procs) {
            node = (orte_node_t*)item;
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node,
                                                                 jdata->map->cpus_per_rank, app->idx,
                                                                 &node_list, jdata->map->oversubscribe,
                                                                 false, NULL))) {
                /* if the code is not ORTE_ERR_NODE_FULLY_USED, then that is an error */
                if (ORTE_ERR_NODE_FULLY_USED != rc) {
                    ORTE_ERROR_LOG(rc);
                    goto error;
                }
            }
            total_procs++;
            /* move to next node */
            if (opal_list_get_end(&node_list) == opal_list_get_next(item)) {
                item = opal_list_get_first(&node_list);
            }
            else {
                item = opal_list_get_next(item);
            }
        }
        /* save the bookmark */
        jdata->bookmark = node;
        
        /* cleanup */
        while (NULL != (item = opal_list_remove_first(&node_list))) {
            OBJ_RELEASE(item);
        }
        /* compute vpids and add proc objects to the job - this has to be
         * done after each app_context is mapped in order to keep the
         * vpids contiguous within an app_context
         */
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_vpids(jdata))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    /* record the number of procs */
    jdata->num_procs = total_procs;
    
error:
    while(NULL != (item = opal_list_remove_first(&node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node_list);

    return rc;
}

