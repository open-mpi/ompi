/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/rmaps/base/rmaps_private.h"

/*
 * JOB_MAP
 */
int orte_rmaps_base_unpack_map(orte_buffer_t *buffer, void *dest,
                              orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_std_cntr_t i, j, n, num_nodes;
    orte_rmaps_base_map_t **maps;
    orte_rmaps_base_node_t *node;

    /* unpack into array of orte_rmaps_base_map_t objects */
    maps = (orte_rmaps_base_map_t**) dest;
    for (i=0; i < *num_vals; i++) {

        /* create the orte_rmaps_base_map_t object */
        maps[i] = OBJ_NEW(orte_rmaps_base_map_t);
        if (NULL == maps[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* unpack the app_context */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                &(maps[i]->app), &n, ORTE_APP_CONTEXT))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the number of procs */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                &(maps[i]->num_procs), &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if we have some, allocate space for them */
        if (0 < maps[i]->num_procs) {
            maps[i]->procs = (orte_rmaps_base_proc_t**)malloc(maps[i]->num_procs * sizeof(orte_rmaps_base_proc_t*));
            if (NULL == maps[i]->procs) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            /* and unpack them */
            if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, maps[i]->procs, &(maps[i]->num_procs), ORTE_MAPPED_PROC))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* unpack the number of nodes */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &num_nodes, &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        for (j=0; j < num_nodes; j++) {
            n = 1;
            if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &node, &n, ORTE_MAPPED_NODE))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            opal_list_append(&(maps[i]->nodes), &node->super);
        }
    }

    return ORTE_SUCCESS;
}

/*
 * MAPPED_PROC
 */
int orte_rmaps_base_unpack_mapped_proc(orte_buffer_t *buffer, void *dest,
                                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_std_cntr_t i, n;
    orte_rmaps_base_proc_t **procs;
    
    /* unpack into array of orte_rmaps_base_proc_t objects */
    procs = (orte_rmaps_base_proc_t**) dest;
    for (i=0; i < *num_vals; i++) {
        
        /* create the orte_rmaps_base_proc_t object */
        procs[i] = OBJ_NEW(orte_rmaps_base_proc_t);
        if (NULL == procs[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        
        /* unpack the app name */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                                         &(procs[i]->app), &n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the proc_node */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                                         &(procs[i]->proc_node), &n, ORTE_MAPPED_NODE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the proc name */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                                         &(procs[i]->proc_name), &n, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the rank */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                                         &(procs[i]->proc_rank), &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the pls-pid */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                                         &(procs[i]->pid), &n, ORTE_PID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the local pid */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                                         &(procs[i]->local_pid), &n, ORTE_PID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    return ORTE_SUCCESS;
}

/*
 * MAPPED_NODE
 */
int orte_rmaps_base_unpack_mapped_node(orte_buffer_t *buffer, void *dest,
                                       orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_std_cntr_t i, j, n, num_procs;
    orte_rmaps_base_node_t **nodes;
    orte_rmaps_base_proc_t *srcproc;
    
    /* unpack into array of orte_rmaps_base_node_t objects */
    nodes = (orte_rmaps_base_node_t**) dest;
    for (i=0; i < *num_vals; i++) {
        
        /* create the orte_rmaps_base_node_t object */
        nodes[i] = OBJ_NEW(orte_rmaps_base_node_t);
        if (NULL == nodes[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        
        /* unpack the node object */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                                         &(nodes[i]->node), &n, ORTE_RAS_NODE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the number of procs */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &num_procs, &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* if we have some, unpack them */
        if (0 < num_procs) {
            for (j=0; j < num_procs; j++) {
                n = 1;
                if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &srcproc, &n, ORTE_MAPPED_PROC))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                opal_list_append(&(nodes[i]->node_procs), &srcproc->super);
            }
        }
    }
    
    return ORTE_SUCCESS;
}


