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

#include "orte/mca/ras/base/ras_private.h"

/*
    * RAS NODE
    */
int orte_ras_base_unpack_node(orte_buffer_t *buffer, void *dest,
                              orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_std_cntr_t i, n;
    orte_ras_node_t **nodes;

    /* unpack into array of ras_node objects */
    nodes = (orte_ras_node_t**) dest;
    for (i=0; i < *num_vals; i++) {

        /* create the ras_node object */
        nodes[i] = OBJ_NEW(orte_ras_node_t);
        if (NULL == nodes[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* unpack the node name */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                &(nodes[i]->node_name), &n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the launch id */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                (&(nodes[i]->launch_id)), &n, ORTE_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the arch */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                (&(nodes[i]->node_arch)), &n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the cellid */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                (&(nodes[i]->node_cellid)), &n, ORTE_CELLID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the state */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                (&(nodes[i]->node_state)), &n, ORTE_NODE_STATE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the number of slots */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                (&(nodes[i]->node_slots)), &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the number of slots in use */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                (&(nodes[i]->node_slots_inuse)), &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the number of slots allocated */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                           (&(nodes[i]->node_slots_alloc)), &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the max number of slots */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                (&(nodes[i]->node_slots_max)), &n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the username */
        n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                (&(nodes[i]->node_username)), &n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the node launched flag */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer,
                                (&(nodes[i]->node_launched)), &n, ORTE_INT))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    return ORTE_SUCCESS;
}

