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

#include "dps/dps.h"

#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"

#include "gpr_replica_api.h"

int
orte_gpr_replica_subscribe(orte_gpr_notify_action_t action,
			              int num_subs,
                           orte_gpr_subscription_t **subscriptions,
                           int num_trigs,
                           orte_gpr_value_t **trigs,
                           orte_gpr_notify_id_t *sub_number)
{
    int rc;
    orte_gpr_notify_id_t idtag;

    /* protect against errors */
    if (NULL == subscriptions) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
	    return ORTE_ERR_BAD_PARAM;
    }

    /* if this has a trigger in it, must specify the trigger condition */
    if (ORTE_GPR_TRIG_ANY & action && NULL == trigs) {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            return ORTE_ERR_BAD_PARAM;
    }
    
    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    if (orte_gpr_replica_globals.compound_cmd_mode) {

        	if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_subscribe(orte_gpr_replica_globals.compound_cmd,
        				    action, num_subs, subscriptions, num_trigs, trigs))) {
            ORTE_ERROR_LOG(rc);
            OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            return rc;
        }
        
        /* enter request on notify tracking system */
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_enter_notify_request(&idtag,
                                    NULL, ORTE_GPR_NOTIFY_ID_MAX,
                                    num_subs, subscriptions))) {
            ORTE_ERROR_LOG(rc);
            OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            return rc;
        }
        *sub_number = idtag;
        
        if (ORTE_SUCCESS != (rc = orte_dps.pack(orte_gpr_replica_globals.compound_cmd,
                                &idtag, 1, ORTE_GPR_NOTIFY_ID))) {
            ORTE_ERROR_LOG(rc);
            OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            return rc;
        }

        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

        return ORTE_SUCCESS;
    }
    
    /* enter request on notify tracking system */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_enter_notify_request(&idtag,
                                NULL, ORTE_GPR_NOTIFY_ID_MAX,
                                num_subs, subscriptions))) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }
    *sub_number = idtag;
        
    /* register subscriptions */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_subscribe_fn(action, num_subs,
                                        subscriptions, num_trigs, trigs, idtag))) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    orte_gpr_replica_process_callbacks();
    
    return rc;
}


int orte_gpr_replica_unsubscribe(orte_gpr_notify_id_t sub_number)
{
    int rc;

    if (orte_gpr_replica_globals.compound_cmd_mode) {
	   return orte_gpr_base_pack_unsubscribe(orte_gpr_replica_globals.compound_cmd,
                        sub_number);
    }

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    rc = orte_gpr_replica_unsubscribe_fn(sub_number);

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}
