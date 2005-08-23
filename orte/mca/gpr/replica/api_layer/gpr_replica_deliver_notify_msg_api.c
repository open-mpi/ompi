/* -*- C -*-
 *
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
 * The Open MPI General Purpose Registry - Replica component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "include/orte_constants.h"
#include "mca/errmgr/errmgr.h"

#include "mca/gpr/replica/api_layer/gpr_replica_api.h"

int orte_gpr_replica_deliver_notify_msg(orte_gpr_notify_message_t *msg)
{
    orte_gpr_notify_data_t **data;
    orte_gpr_replica_local_trigger_t **local_trigs;
    orte_gpr_replica_local_subscriber_t **local_subs, *sub;
    size_t i, j, k;
    int rc;
    bool processed;
    
    /* we first have to check the trigger id in the message. If that
     * field is set to a valid value (i.e., one other than
     * ORTE_GPR_TRIGGER_ID_MAX), then the message is intended to be
     * sent as a single block to that trigger's callback function.
     */
    if (ORTE_GPR_TRIGGER_ID_MAX > msg->id) {
        /* use the local trigger callback */
        local_trigs = (orte_gpr_replica_local_trigger_t**)
                            (orte_gpr_replica_globals.local_triggers)->addr;
        processed = false;
        for (i=0, j=0; !processed &&
                       j < orte_gpr_replica_globals.num_local_trigs &&
                       i < (orte_gpr_replica_globals.local_triggers)->size; i++) {
            if (NULL != local_trigs[i]) {
                j++;
                if (msg->id == local_trigs[i]->id) {
                    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
                    local_trigs[i]->callback(msg);
                    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);
                    processed = true;
                }
            }
        }
        if (!processed) { /* trigger could not be found */
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        return ORTE_SUCCESS;
    }
    
    /* get here if the trigger id indicated that this was NOT
     * intended for a trigger callback - i.e., the message should
     * be broken into its component parts and delivered separately
     * to the indicated subscribers
     */
    data = (orte_gpr_notify_data_t**)(msg->data)->addr;
    for (i=0; i < msg->cnt; i++) {
        /* for each datagram in the message, we need to lookup
         * the associated subscription (could be specified by name or id) to find the correct
         * callback function. Name specifications are given precedence over id.
         */
        local_subs = (orte_gpr_replica_local_subscriber_t**)
                        (orte_gpr_replica_globals.local_subscriptions)->addr;
        processed = false;
        for (j=0, k=0; !processed &&
                       k < orte_gpr_replica_globals.num_local_subs &&
                       j < (orte_gpr_replica_globals.local_subscriptions)->size; j++) {
            if (NULL != local_subs[j]) {
                k++;
                if ((NULL != local_subs[j]->name &&
                    NULL != data[i]->target &&
                    0 == strcmp(data[i]->target, local_subs[j]->name)) ||
                    (data[i]->id == local_subs[j]->id)) {
                    sub = local_subs[j];
                    processed = true;
                }
            }
        }
        
        /* get here and not processed => not found, abort */
        if (!processed) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }

        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        sub->callback(data[i], sub->user_tag);
        OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

        if (data[i]->remove) {
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_remove_local_subscription(sub))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    
    /* the calling program will release the message object */
    return ORTE_SUCCESS;
}
