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

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/base/base.h"

int orte_gpr_base_pack_delete_segment(orte_buffer_t *cmd, char *segment)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    OPAL_TRACE(3);

    command = ORTE_GPR_DELETE_SEGMENT_CMD;

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
       return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &segment, 1, ORTE_STRING))) {
       return rc;
    }

    return ORTE_SUCCESS;
}


int orte_gpr_base_pack_delete_entries(orte_buffer_t *cmd,
                                      orte_gpr_addr_mode_t mode,
                                     char *segment, char **tokens, char **keys)
{
    orte_gpr_cmd_flag_t command;
    char **ptr;
    orte_std_cntr_t n;
    int rc;

    OPAL_TRACE(3);

    command = ORTE_GPR_DELETE_ENTRIES_CMD;

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
       return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &mode, 1, ORTE_GPR_ADDR_MODE))) {
       return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &segment, 1, ORTE_STRING))) {
       return rc;
    }

    /* compute number of tokens */
    if (NULL == tokens) {
        n = 0;
    } else {
        ptr = tokens;
        n = 0;
        while (NULL != ptr[n]) {
               n++;
        }
    }

    /* pack number of tokens */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &n, 1, ORTE_STD_CNTR))) {
        return rc;
    }

    /* pack tokens ONLY if n > 0 */
    if (0 < n) {
        if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, tokens, n, ORTE_STRING))) {
            return rc;
        }
    }
    /* compute number of keys */
    if (NULL == keys) {
        n = 0;
    } else {
        ptr = keys;
        n = 0;
        while (NULL != ptr[n]) {
            n++;
        }
    }

    /* pack number of keys */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &n, 1, ORTE_STD_CNTR))) {
        return rc;
    }

    /* pack keys ONLY if n > 0 */
    if (0 < n) {
        if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, keys, n, ORTE_STRING))) {
            return rc;
        }
    }
    return ORTE_SUCCESS;
}


int orte_gpr_base_pack_index(orte_buffer_t *cmd, char *segment)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    OPAL_TRACE(3);

    command = ORTE_GPR_INDEX_CMD;

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* it's okay to pack a NULL string, so pack the segment regardless */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &segment, 1, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}
