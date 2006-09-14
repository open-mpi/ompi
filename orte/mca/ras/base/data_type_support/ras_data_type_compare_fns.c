/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/ras/base/ras_private.h"

/**
 * RAS NODE
 */
int orte_ras_base_compare_node(orte_ras_node_t *value1, orte_ras_node_t *value2, orte_data_type_t type)
{
    int test;
    
    if (value1->node_cellid > value2->node_cellid) return ORTE_VALUE1_GREATER;
    if (value2->node_cellid > value1->node_cellid) return ORTE_VALUE2_GREATER;

    /** same cell - check node names */
    test = strcmp(value1->node_name, value2->node_name);
    if (0 == test) return ORTE_EQUAL;
    if (0 < test) return ORTE_VALUE2_GREATER;

    return ORTE_VALUE1_GREATER;
}
