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
#include "orte/orte_constants.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_types.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/odls/base/odls_private.h"

/*
 * ORTE_DAEMON_CMD
 */
int orte_odls_unpack_daemon_cmd(orte_buffer_t *buffer, void *dest, orte_std_cntr_t *num_vals,
                                orte_data_type_t type)
{
    int ret;
    orte_data_type_t remote_type;
    
    /* if the buffer is fully described, then we can do some magic to handle
        * the heterogeneous case. if not, then we can only shoot blind - it is the
    * user's responsibility to ensure we are in a homogeneous environment.
        */
    if (ORTE_DSS_BUFFER_FULLY_DESC == buffer->type) {
        /* see what type was actually packed */
        if (ORTE_SUCCESS != (ret = orte_dss_peek_type(buffer, &remote_type))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        
        if (remote_type == ORTE_DAEMON_CMD_T) {
            /* fast path it if the sizes are the same */
            /* Turn around and unpack the real type */
            if (ORTE_SUCCESS != (ret = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_DAEMON_CMD_T))) {
                ORTE_ERROR_LOG(ret);
            }
        } else {
            /* slow path - types are different sizes */
            UNPACK_SIZE_MISMATCH(orte_daemon_cmd_flag_t, remote_type, ret);
        }
        return ret;
    }
    
    /* if we get here, then this buffer is NOT fully described. just unpack it
        * using the local size - user gets the pain if it's wrong
        */
    if (ORTE_SUCCESS != (ret = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_DAEMON_CMD_T))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

