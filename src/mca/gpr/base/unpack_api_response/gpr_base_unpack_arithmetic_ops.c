/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "dps/dps.h"
#include "mca/errmgr/errmgr.h"

#include "mca/gpr/base/base.h"

int orte_gpr_base_unpack_increment_value(orte_buffer_t *cmd, int *ret)
{
    orte_gpr_cmd_flag_t command;
    int rc;
    size_t n;
    
    n=1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(cmd, &command, &n, ORTE_GPR_PACK_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_GPR_INCREMENT_VALUE_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }

    n=1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(cmd, ret, &n, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;

}

int orte_gpr_base_unpack_decrement_value(orte_buffer_t *cmd, int *ret)
{
    orte_gpr_cmd_flag_t command;
    int rc;
    size_t n;

    n=1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(cmd, &command, &n, ORTE_GPR_PACK_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_GPR_DECREMENT_VALUE_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }

    n=1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(cmd, ret, &n, ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}
