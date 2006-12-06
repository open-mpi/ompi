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
/** @file:
 */

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#include "opal/class/opal_list.h"

#include "orte/mca/rds/base/rds_private.h"

int orte_rds_base_no_op_query(orte_jobid_t job)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_rds_base_no_op_store_resource(opal_list_t *resources)
{
    return ORTE_ERR_NOT_SUPPORTED;
}
