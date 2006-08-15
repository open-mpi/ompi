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
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "opal/util/trace.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"
#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/base/base.h"

int orte_gpr_base_unpack_increment_value(orte_buffer_t *cmd, int *ret)
{
    orte_gpr_cmd_flag_t command;
    int rc;
    orte_std_cntr_t n;

    OPAL_TRACE(3);

    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(cmd, &command, &n, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_GPR_INCREMENT_VALUE_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }

    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(cmd, ret, &n, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;

}

int orte_gpr_base_unpack_decrement_value(orte_buffer_t *cmd, int *ret)
{
    orte_gpr_cmd_flag_t command;
    int rc;
    orte_std_cntr_t n;

    OPAL_TRACE(3);

    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(cmd, &command, &n, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_GPR_DECREMENT_VALUE_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }

    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(cmd, ret, &n, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}
