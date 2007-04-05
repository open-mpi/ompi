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

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/util/argv.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/ras/base/ras_private.h"

/*
 * RAS NODE
 */
int orte_ras_base_pack_node(orte_buffer_t *buffer, void *src,
                            orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int rc;
    orte_std_cntr_t i;
    orte_ras_node_t **nodes;

    /* array of pointers to orte_ras_node_t objects - need to pack the objects a set of fields at a time */
    nodes = (orte_ras_node_t**) src;

    for (i=0; i < num_vals; i++) {
        /* pack the node name */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&(nodes[i]->node_name)), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the launch id */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&(nodes[i]->launch_id)), 1, ORTE_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the arch */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&(nodes[i]->node_arch)), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the cellid */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&(nodes[i]->node_cellid)), 1, ORTE_CELLID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the state */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&(nodes[i]->node_state)), 1, ORTE_NODE_STATE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the number of slots */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&(nodes[i]->node_slots)), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the number of slots in use */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&(nodes[i]->node_slots_inuse)), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the number of slots allocated */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&(nodes[i]->node_slots_alloc)), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the max number of slots */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&(nodes[i]->node_slots_max)), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the username */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&(nodes[i]->node_username)), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the node launched flag */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&(nodes[i]->node_launched)), 1, ORTE_INT))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
        
    return ORTE_SUCCESS;
}
