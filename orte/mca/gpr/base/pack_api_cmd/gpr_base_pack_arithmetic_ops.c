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

int orte_gpr_base_pack_arith(orte_buffer_t *cmd,
                             orte_gpr_addr_mode_t addr_mode,
                             char *segment, char **tokens, char **keys,
                             orte_dss_arith_op_t operation,
                             orte_data_value_t *operand)
{
    orte_gpr_cmd_flag_t command;
    orte_std_cntr_t n;
    char **ptr;
    int rc;
    
    OPAL_TRACE(3);
    
    command = ORTE_GPR_ARITH_CMD;
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &addr_mode, 1, ORTE_GPR_ADDR_MODE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &segment, 1, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* compute number of tokens */
    n = 0;
    if (NULL != tokens) {
        ptr = tokens;
        while (NULL != ptr[n]) {
            n++;
        }
    }
    
    /* pack number of tokens */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &n, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (n > 0) {
        if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, tokens, n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /* compute number of keys */
    n = 0;
    if (NULL != keys) {
        ptr = keys;
        while (NULL != ptr[n]) {
            n++;
        }
    }
    
    /* pack number of keys */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &n, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (n > 0) {
        if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, keys, n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /** pack the operation flag */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &operation, 1, ORTE_ARITH_OP))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /** pack the operand */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &operand, 1, ORTE_DATA_VALUE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

int orte_gpr_base_pack_increment_value(orte_buffer_t *cmd, orte_gpr_value_t *value)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    OPAL_TRACE(3);

    command = ORTE_GPR_INCREMENT_VALUE_CMD;

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &value, 1, ORTE_GPR_VALUE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;

}

int orte_gpr_base_pack_decrement_value(orte_buffer_t *cmd, orte_gpr_value_t *value)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    OPAL_TRACE(3);

    command = ORTE_GPR_DECREMENT_VALUE_CMD;

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &value, 1, ORTE_GPR_VALUE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}
