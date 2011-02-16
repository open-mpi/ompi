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
#include "opal/util/opal_sos.h"

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
    
    /* only handle initial launch of loadbalanced
     * or NPERxxx jobs - allow restarting of failed apps
     */
    if (ORTE_JOB_STATE_INIT != jdata->state) {
        opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                            "mca:rmaps:lb: not job %s not in initial state - loadbalance cannot map",
                            ORTE_JOBID_PRINT(jdata->jobid));
        return ORTE_ERR_TAKE_NEXT_OPTION;
    }
    if (0 < jdata->map->mapper && ORTE_RMAPS_LOADBALANCE != jdata->map->mapper) {
        /* a mapper has been specified, and it isn't me */
        opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                            "mca:rmaps:lb: job %s not using loadbalance mapper",
                            ORTE_JOBID_PRINT(jdata->jobid));
        return ORTE_ERR_TAKE_NEXT_OPTION;
    }

    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:loadbalance: mapping job %s",
                        ORTE_JOBID_PRINT(jdata->jobid));
 
    /* flag that I did the mapping */
    jdata->map->mapper = ORTE_RMAPS_LOADBALANCE;

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
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_define_daemons(jdata))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


/* place specified #procs on each node, up to the specified total
 * number of procs (if one was given).
 */
static int npernode(orte_job_t *jdata)
{
    orte_app_context_t *app;
    int j, rc=ORTE_SUCCESS;
    opal_list_t node_list;
    opal_list_item_t *item;
    orte_std_cntr_t num_slots;
    orte_node_t *node;
    int np, nprocs;
    int num_nodes;
    
    /* setup the node list */
    OBJ_CONSTRUCT(&node_list, opal_list_t);
   
    /* can only have one app_context here */
    if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, 0))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    /* use the number of procs if one was given */
    if (0 < app->num_procs) {
        np = app->num_procs;
    } else {
        np = INT_MAX;
    }
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
    num_nodes = opal_list_get_size(&node_list);
    nprocs = 0;
    while (NULL != (item = opal_list_remove_first(&node_list))) {
        node = (orte_node_t*)item;
        /* put the specified number of procs on each node */
        for (j=0; j < orte_rmaps_base.npernode && nprocs < np; j++) {
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node,
                                                                 jdata->map->cpus_per_rank, app->idx,
                                                                 &node_list, jdata->map->oversubscribe,
                                                                 false, NULL))) {
                /** if the code is ORTE_ERR_NODE_FULLY_USED, and we still have
                 * more procs to place, then that is an error
                 */
                if (ORTE_ERR_NODE_FULLY_USED != OPAL_SOS_GET_ERROR_CODE(rc) ||
                    j < orte_rmaps_base.npernode-1) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(node);
                    goto error;
                }
            }
            nprocs++;
        }
        OBJ_RELEASE(node);
    }
    /* if the user requested a specific number of procs and
     * the total number of procs we were able to assign
     * doesn't equal the number requested, then we have a
     * problem
     */
    if (0 < app->num_procs && nprocs < app->num_procs) {
        orte_show_help("help-orte-rmaps-base.txt", "rmaps:too-many-procs", true,
                       app->app, app->num_procs,
                       "number of nodes", num_nodes,
                       "npernode", orte_rmaps_base.npernode);
        return ORTE_ERR_SILENT;
    }
    /* update the number of procs in the job */
    jdata->num_procs += nprocs;
    /* compute vpids and add proc objects to the job - this has to be
     * done after each app_context is mapped in order to keep the
     * vpids contiguous within an app_context
     */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_vpids(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

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
    int j, k, rc=ORTE_SUCCESS;
    opal_list_t node_list;
    opal_list_item_t *item;
    orte_std_cntr_t num_slots;
    orte_node_t *node;
    int np, nprocs;
    int num_boards=orte_default_num_boards;

    /* setup the node list */
    OBJ_CONSTRUCT(&node_list, opal_list_t);
    
    /* can only have one app_context here */
    if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, 0))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    /* use the number of procs if one was given */
    if (0 < app->num_procs) {
        np = app->num_procs;
    } else {
        np = INT_MAX;
    }
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
    nprocs = 0;
    while (NULL != (item = opal_list_remove_first(&node_list))) {
        node = (orte_node_t*)item;
        num_boards = node->boards;
        /* loop through the number of boards in this node */
        for (k=0; k < node->boards && nprocs < np; k++) {
            /* put the specified number of procs on each board */
            for (j=0; j < orte_rmaps_base.nperboard && nprocs < np; j++) {
                if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node,
                                                                     jdata->map->cpus_per_rank, app->idx,
                                                                     &node_list, jdata->map->oversubscribe,
                                                                     false, NULL))) {
                    /** if the code is ORTE_ERR_NODE_FULLY_USED, and we still have
                     * more procs to place, then that is an error
                     */
                    if (ORTE_ERR_NODE_FULLY_USED != OPAL_SOS_GET_ERROR_CODE(rc) ||
                        j < orte_rmaps_base.nperboard-1) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(node);
                        goto error;
                    }
                }
                nprocs++;
            }
        }
        OBJ_RELEASE(node);
    }
    /* if the user requested a specific number of procs and
     * the total number of procs we were able to assign
     * doesn't equal the number requested, then we have a
     * problem
     */
    if (0 < app->num_procs && nprocs < app->num_procs) {
        orte_show_help("help-orte-rmaps-base.txt", "rmaps:too-many-procs", true,
                       app->app, app->num_procs,
                       "number of boards", num_boards,
                       "nperboard", orte_rmaps_base.nperboard);
        return ORTE_ERR_SILENT;
    }
    /* update the number of procs in the job */
    jdata->num_procs += nprocs;
    /* compute vpids and add proc objects to the job - this has to be
     * done after each app_context is mapped in order to keep the
     * vpids contiguous within an app_context
     */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_vpids(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

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
    int j, k, n, rc=ORTE_SUCCESS;
    opal_list_t node_list;
    opal_list_item_t *item;
    orte_std_cntr_t num_slots;
    orte_node_t *node;
    int np, nprocs;
    int num_sockets=orte_default_num_sockets_per_board;

    /* setup the node list */
    OBJ_CONSTRUCT(&node_list, opal_list_t);
   
    /* can only have one app_context here */
    if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, 0))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    /* use the number of procs if one was given */
    if (0 < app->num_procs) {
        np = app->num_procs;
    } else {
        np = INT_MAX;
    }
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
    nprocs = 0;
    while (NULL != (item = opal_list_remove_first(&node_list))) {
        node = (orte_node_t*)item;
        num_sockets = node->sockets_per_board;
        /* loop through the number of boards in this node */
        for (k=0; k < node->boards && nprocs < np; k++) {
            /* loop through the number of sockets/board */
            for (n=0; n < node->sockets_per_board && nprocs < np; n++) {
                /* put the specified number of procs on each socket */
                for (j=0; j < orte_rmaps_base.npersocket && nprocs < np; j++) {
                    if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node,
                                                                         jdata->map->cpus_per_rank, app->idx,
                                                                         &node_list, jdata->map->oversubscribe,
                                                                         false, NULL))) {
                        /** if the code is ORTE_ERR_NODE_FULLY_USED, and we still have
                         * more procs to place, then that is an error
                         */
                        if (ORTE_ERR_NODE_FULLY_USED != OPAL_SOS_GET_ERROR_CODE(rc) ||
                            j < orte_rmaps_base.npersocket-1) {
                            ORTE_ERROR_LOG(rc);
                            OBJ_RELEASE(node);
                            goto error;
                        }
                    }
                    /* track the number of procs */
                    nprocs++;
                }
            }
        }
        OBJ_RELEASE(node);
    }
    /* if the user requested a specific number of procs and
     * the total number of procs we were able to assign
     * doesn't equal the number requested, then we have a
     * problem
     */
    if (0 < app->num_procs && nprocs < app->num_procs) {
        orte_show_help("help-orte-rmaps-base.txt", "rmaps:too-many-procs", true,
                       app->app, app->num_procs,
                       "number of sockets", num_sockets,
                       "npersocket", orte_rmaps_base.npersocket);
        return ORTE_ERR_SILENT;
    }
    /* update the number of procs in the job */
    jdata->num_procs += nprocs;
    /* compute vpids and add proc objects to the job - this has to be
     * done after each app_context is mapped in order to keep the
     * vpids contiguous within an app_context
     */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_vpids(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
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
    int rc=ORTE_SUCCESS, np, nprocs;
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
        if (0 < app->num_procs) {
            np = app->num_procs;
        } else {
            /* set the num_procs to the #slots */
            np = num_slots;
        }
        num_nodes = opal_list_get_size(&node_list);
        /* compute the base ppn */
        ppn = np / num_nodes;
        /* if a bookmark exists from some prior mapping, set us to start there */
        start = orte_rmaps_base_get_starting_point(&node_list, jdata);
        /* loop through the list of nodes until we either assign all the procs
         * or return to the starting point
         */
        item = start;
        nprocs = 0;
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
                    if (ORTE_ERR_NODE_FULLY_USED != OPAL_SOS_GET_ERROR_CODE(rc) ||
                        j < ppn-1) {
                        ORTE_ERROR_LOG(rc);
                        goto error;
                    }
                }
                nprocs++;
            }
            /* move to next node */
            if (opal_list_get_end(&node_list) == opal_list_get_next(item)) {
                item = opal_list_get_first(&node_list);
            }
            else {
                item = opal_list_get_next(item);
            }
        } while (item != start && nprocs < np);
        
        /* save the bookmark */
        jdata->bookmark = node;

        /* if we haven't assigned all the procs, then loop through the list
         * again, assigning 1 per node until all are assigned
         */
        item = start;
        while (nprocs < np) {
            node = (orte_node_t*)item;
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node,
                                                                 jdata->map->cpus_per_rank, app->idx,
                                                                 &node_list, jdata->map->oversubscribe,
                                                                 false, NULL))) {
                /* if the code is not ORTE_ERR_NODE_FULLY_USED, then that is an error */
                if (ORTE_ERR_NODE_FULLY_USED != OPAL_SOS_GET_ERROR_CODE(rc)) {
                    ORTE_ERROR_LOG(rc);
                    goto error;
                }
            }
            nprocs++;
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
        /* if the user requested a specific number of procs and
         * the total number of procs we were able to assign
         * doesn't equal the number requested, then we have a
         * problem
         */
        if (0 < app->num_procs && nprocs < app->num_procs) {
            orte_show_help("help-orte-rmaps-base.txt", "rmaps:too-many-procs", true,
                           app->app, app->num_procs,
                           "number of slots", nprocs,
                           "number of nodes", num_nodes);
            return ORTE_ERR_SILENT;
        }
        /* update the number of procs in the job */
        jdata->num_procs += nprocs;
        /* compute vpids and add proc objects to the job - this has to be
         * done after each app_context is mapped in order to keep the
         * vpids contiguous within an app_context
         */
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_vpids(jdata))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
error:
    while(NULL != (item = opal_list_remove_first(&node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node_list);

    return rc;
}

