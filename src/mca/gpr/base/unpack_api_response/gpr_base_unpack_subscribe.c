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
 * The Open MPI general purpose registry - unpack functions.
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


int orte_gpr_base_unpack_subscribe(orte_buffer_t *buffer, int *ret, orte_gpr_notify_id_t *remote_idtag)
{
    orte_gpr_cmd_flag_t command;
    size_t n;
    int rc;

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, &command, &n, ORTE_GPR_PACK_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
	if (ORTE_GPR_SUBSCRIBE_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
	   return ORTE_ERR_COMM_FAILURE;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, remote_idtag, &n, ORTE_GPR_NOTIFY_ID))) {
        ORTE_ERROR_LOG(rc);
	    return rc;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, ret, &n, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}


int orte_gpr_base_unpack_unsubscribe(orte_buffer_t *buffer, int *ret)
{
    orte_gpr_cmd_flag_t command;
    size_t n;
    int rc;

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, &command, &n, ORTE_GPR_PACK_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
	if (ORTE_GPR_UNSUBSCRIBE_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
	    return ORTE_ERR_COMM_FAILURE;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, ret, &n, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
	    return rc;
    }

    return ORTE_SUCCESS;
}
