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

#include "orte/dss/dss_types.h"

#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"

#include "gpr_replica_api.h"

int
orte_gpr_replica_subscribe(orte_std_cntr_t num_subs,
                           orte_gpr_subscription_t **subscriptions,
                           orte_std_cntr_t num_trigs,
                           orte_gpr_trigger_t **trigs)
{
    int rc;

    OPAL_TRACE(1);

    /* protect against errors */
    if (NULL == subscriptions && NULL == trigs) { /* need at least one */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    /* store callback function and user_tag in local list for lookup
     * generate id_tag to put in registry to identify lookup entry
     * for each subscription - the subscription id is returned
     * inside the subscription objects
     */
    if (NULL != subscriptions) {
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_enter_local_subscription(
                                            num_subs, subscriptions))) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            return rc;
        }
    }

    /* if any triggers were provided, get id tags for them - the
     * idtags are returned inside the trigger objects
     */
    if (NULL != trigs) {
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_enter_local_trigger(
                                            num_trigs, trigs))) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            return rc;
        }
    }

    /* register subscriptions */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_subscribe_fn(NULL,
                                        num_subs, subscriptions,
                                        num_trigs, trigs))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_check_events())) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    rc = orte_gpr_replica_process_callbacks();

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}


int orte_gpr_replica_unsubscribe(orte_gpr_subscription_id_t sub_number)
{
    orte_gpr_replica_local_subscriber_t **subs;
    orte_std_cntr_t i, j;
    int rc;

    OPAL_TRACE(1);

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_remove_subscription(NULL, sub_number))) {
        ORTE_ERROR_LOG(rc);
    }

    if (ORTE_SUCCESS == rc) {
        /* find and remove it from the local subscription tracking system */
        subs = (orte_gpr_replica_local_subscriber_t**)(orte_gpr_replica_globals.local_subscriptions)->addr;
        for (i=0, j=0; j < orte_gpr_replica_globals.num_local_subs &&
                       i < (orte_gpr_replica_globals.local_subscriptions)->size; i++) {
            if (NULL != subs[i]) {
                j++;
                if (sub_number == subs[i]->id) {
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_remove_local_subscription(subs[i]))) {
                        ORTE_ERROR_LOG(rc);
                    }
                }
            }
        }
    }

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}


int orte_gpr_replica_cancel_trigger(orte_gpr_trigger_id_t trig)
{
    orte_gpr_replica_local_trigger_t **trigs;
    orte_std_cntr_t i, j;
    int rc;

    OPAL_TRACE(1);

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    rc = orte_gpr_replica_remove_trigger(NULL, trig);

    if (ORTE_SUCCESS == rc) {
        /* find and remove it from the local trigger tracking system */
        trigs = (orte_gpr_replica_local_trigger_t**)(orte_gpr_replica_globals.local_triggers)->addr;
        for (i=0, j=0; j < orte_gpr_replica_globals.num_local_trigs &&
                       i < (orte_gpr_replica_globals.local_triggers)->size; i++) {
            if (NULL != trigs[i]) {
                j++;
                if (trig == trigs[i]->id) {
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_remove_local_trigger(trigs[i]))) {
                        ORTE_ERROR_LOG(rc);
                    }
                }
            }
        }
    }

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}
