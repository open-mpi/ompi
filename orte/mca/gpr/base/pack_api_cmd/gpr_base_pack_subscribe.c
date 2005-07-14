/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "dps/dps.h"
#include "mca/errmgr/errmgr.h"

#include "mca/gpr/base/base.h"

int orte_gpr_base_pack_subscribe(orte_buffer_t *cmd,
				size_t num_subs,
                 orte_gpr_subscription_t **subscriptions,
                 size_t num_trigs,
                 orte_gpr_trigger_t **trigs)
{
    orte_gpr_cmd_flag_t command;
    int rc;
    size_t zero=0;

    command = ORTE_GPR_SUBSCRIBE_CMD;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
	    return rc;
    }

    /* there MUST be some subscriptions, so don't have to check that here - it was checked on the API
     * itself.
     */
    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, subscriptions, num_subs, ORTE_GPR_SUBSCRIPTION))) {
        ORTE_ERROR_LOG(rc);
	    return rc;
    }

    /* the API DOES allow there to be no triggers - if that happens, then trigs will be NULL and num_trigs
     * should be set to zero. we can't send that to the DPS though as it is an error condition over there,
     * so check for it here and record a "zero" if nothing is there
     */
    if (NULL != trigs && 0 < num_trigs) {
        if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, trigs, num_trigs, ORTE_GPR_TRIGGER))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    } else {
        if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &zero, 1, ORTE_SIZE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    return ORTE_SUCCESS;
}


int orte_gpr_base_pack_unsubscribe(orte_buffer_t *cmd,
				  orte_gpr_subscription_id_t id)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    command = ORTE_GPR_UNSUBSCRIBE_CMD;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
	   return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &id, 1, ORTE_GPR_SUBSCRIPTION_ID))) {
	   return rc;
    }

    return ORTE_SUCCESS;
}

int orte_gpr_base_pack_cancel_trigger(orte_buffer_t *cmd, orte_gpr_trigger_id_t id)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    command = ORTE_GPR_CANCEL_TRIGGER_CMD;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
     return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &id, 1, ORTE_GPR_TRIGGER_ID))) {
       return rc;
    }

    return ORTE_SUCCESS;
}
