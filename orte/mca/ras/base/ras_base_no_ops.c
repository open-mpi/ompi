/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/util/output.h"
#include "orte/orte_constants.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ras/base/ras_private.h"

int orte_ras_base_allocate_no_op(orte_jobid_t jobid, opal_list_t *attributes) 
{
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_ras_base_node_insert_no_op(opal_list_t *list)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_ras_base_node_query_no_op(opal_list_t *list)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_ras_base_node_query_alloc_no_op(opal_list_t* list, orte_jobid_t job)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

orte_ras_node_t* orte_ras_base_node_lookup_no_op(orte_cellid_t cell, const char* nodename)
{
    return NULL;
}

int orte_ras_base_deallocate_no_op(orte_jobid_t job)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

