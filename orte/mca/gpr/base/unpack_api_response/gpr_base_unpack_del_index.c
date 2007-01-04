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
 */

#include "orte_config.h"

#include "opal/util/trace.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"
#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/base/base.h"


int orte_gpr_base_unpack_delete_segment(orte_buffer_t *buffer, int *ret)
{
    orte_gpr_cmd_flag_t command;
    int rc;
    orte_std_cntr_t n;

    OPAL_TRACE(3);

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &command, &n, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_GPR_DELETE_SEGMENT_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
       return ORTE_ERR_COMM_FAILURE;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, ret, &n, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
       return rc;
    }
    return ORTE_SUCCESS;
}


int orte_gpr_base_unpack_delete_entries(orte_buffer_t *buffer, int *ret)
{
    orte_gpr_cmd_flag_t command;
    int rc;
    orte_std_cntr_t n;

    OPAL_TRACE(3);

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &command, &n, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_GPR_DELETE_ENTRIES_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, ret, &n, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
       return rc;
    }
    return ORTE_SUCCESS;
}


int orte_gpr_base_unpack_index(orte_buffer_t *buffer, int *ret, orte_std_cntr_t *cnt, char ***index)
{
    orte_gpr_cmd_flag_t command;
    orte_std_cntr_t n;
    int rc;

    OPAL_TRACE(3);

    *cnt = 0;
    *index = NULL;

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &command, &n, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_GPR_INDEX_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, ret, &n, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (0 < n) {
        *index = (char **)malloc(n*sizeof(char*));
        if (NULL == *index) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, *index, &n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    *cnt = n;

    return ORTE_SUCCESS;
}
