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

int orte_gpr_base_pack_subscribe(orte_buffer_t *cmd,
				orte_gpr_notify_action_t action, int num_subs,
                 orte_gpr_subscription_t **subscriptions,
                 int num_trigs,
                 orte_gpr_value_t **trigs)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    command = ORTE_GPR_SUBSCRIBE_CMD;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
	    return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &action, 1, ORTE_NOTIFY_ACTION))) {
        ORTE_ERROR_LOG(rc);
	    return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, subscriptions, num_subs, ORTE_GPR_SUBSCRIPTION))) {
        ORTE_ERROR_LOG(rc);
	    return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, trigs, num_trigs, ORTE_GPR_VALUE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}


int orte_gpr_base_pack_unsubscribe(orte_buffer_t *cmd,
				  orte_gpr_notify_id_t remote_idtag)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    command = ORTE_GPR_UNSUBSCRIBE_CMD;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &command, 1, ORTE_GPR_PACK_CMD))) {
	   return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &remote_idtag, 1, ORTE_GPR_PACK_NOTIFY_ID))) {
	   return rc;
    }

    return ORTE_SUCCESS;
}
