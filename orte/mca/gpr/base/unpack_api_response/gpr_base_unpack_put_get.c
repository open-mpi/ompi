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
 * The Open MPI general purpose registry - base unpack functions.
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

int orte_gpr_base_unpack_put(orte_buffer_t *buffer, int *ret)
{
    orte_gpr_cmd_flag_t command;
    int rc;
    orte_std_cntr_t n;

    OPAL_TRACE(3);

    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &command, &n, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_GPR_PUT_CMD != command) {
       ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
       return ORTE_ERR_COMM_FAILURE;
    }

    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, ret, &n, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;

}


int orte_gpr_base_unpack_get(orte_buffer_t *buffer, int *ret, orte_std_cntr_t *cnt, orte_gpr_value_t ***values)
{
    orte_gpr_cmd_flag_t command;
    int rc;
    orte_std_cntr_t n, num;

    OPAL_TRACE(3);

    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &command, &n, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_GPR_GET_CMD != command && ORTE_GPR_GET_CONDITIONAL_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }

    /* find out how many values came back */
    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &num, &n, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* if there were some, then get them */
    if (0 < num) {
        *values = (orte_gpr_value_t**)malloc(num*sizeof(orte_gpr_value_t*));
        if (NULL == *values) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, *values, &num, ORTE_GPR_VALUE))) {
            ORTE_ERROR_LOG(rc);
            free(*values);
            return rc;
        }
    }

    /* unpack the response code */
    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, ret, &n, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != *ret) {
        ORTE_ERROR_LOG(*ret);
        return rc;
    }

    *cnt = num;

    return ORTE_SUCCESS;
}
