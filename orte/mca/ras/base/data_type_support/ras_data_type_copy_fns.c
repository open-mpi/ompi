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
#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/ras/base/ras_private.h"

/**
 * RAS NODE
 */
int orte_ras_base_copy_node(orte_ras_node_t **dest, orte_ras_node_t *src, orte_data_type_t type)
{
    /* create the new object */
    *dest = OBJ_NEW(orte_ras_node_t);
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* copy data into it */
    if (NULL != src->node_name) (*dest)->node_name = strdup(src->node_name);
    (*dest)->launch_id = src->launch_id;
    if (NULL != src->node_arch) (*dest)->node_arch = strdup(src->node_arch);
    (*dest)->node_cellid = src->node_cellid;
    (*dest)->node_state = src->node_state;
    (*dest)->node_slots = src->node_slots;
    (*dest)->node_slots_inuse = src->node_slots_inuse;
    (*dest)->node_slots_alloc = src->node_slots_alloc;
    (*dest)->node_slots_max = src->node_slots_max;
    if (NULL != src->node_username) (*dest)->node_username = strdup(src->node_username);
    (*dest)->node_launched = src->node_launched;

    return ORTE_SUCCESS;
}

