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

#include "dps/dps.h"

#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"

#include "gpr_replica_api.h"

int
orte_gpr_replica_subscribe(size_t num_subs,
                           orte_gpr_subscription_t **subscriptions,
                           size_t num_trigs,
                           orte_gpr_trigger_t **trigs)
{
    int rc;

    /* protect against errors */
    if (NULL == subscriptions) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
	    return ORTE_ERR_BAD_PARAM;
    }

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    /* store callback function and user_tag in local list for lookup
     * generate id_tag to put in registry to identify lookup entry
     * for each subscription - the subscription id is returned
     * inside the subscription objects
     */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_enter_local_subscription(
                                        num_subs, subscriptions))) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    /* if any triggers were provided, get id tags for them - the
     * idtags are returned inside the trigger objects
     */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_enter_local_trigger(
                                        num_trigs, trigs))) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    /* register subscriptions */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_subscribe_fn(NULL,
                                        num_subs, subscriptions,
                                        num_trigs, trigs))) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_check_events())) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    rc = orte_gpr_replica_process_callbacks();
    
    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}


int orte_gpr_replica_unsubscribe(orte_gpr_subscription_id_t sub_number)
{
    int rc;

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    rc = orte_gpr_replica_remove_subscription(NULL, sub_number);

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}


int orte_gpr_replica_cancel_trigger(orte_gpr_trigger_id_t trig)
{
    int rc;

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    rc = orte_gpr_replica_remove_trigger(NULL, trig);

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}
