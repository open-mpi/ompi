/* -*- C -*-
 *
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
 * The Open MPI General Purpose Registry - Replica component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "orte/orte_constants.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/replica/api_layer/gpr_replica_api.h"

int orte_gpr_replica_deliver_notify_msg(orte_gpr_notify_message_t *msg)
{
    orte_gpr_notify_data_t **data;
    orte_gpr_replica_local_trigger_t **local_trigs;
    orte_gpr_replica_local_subscriber_t **local_subs, *sub;
    orte_gpr_trigger_cb_fn_t trig_cb;
    orte_gpr_notify_cb_fn_t sub_cb;
    void *sub_usertag;
    orte_std_cntr_t i, j, k, n;
    int rc;

    OPAL_TRACE(1);

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    /* we first have to check if the message is a trigger message - if so,
     * then the message is intended to be
     * sent as a single block to that trigger's callback function.
     */
    if (ORTE_GPR_TRIGGER_MSG == msg->msg_type) {
        /* use the local trigger callback */
        local_trigs = (orte_gpr_replica_local_trigger_t**)
                            (orte_gpr_replica_globals.local_triggers)->addr;
        for (i=0, j=0; j < orte_gpr_replica_globals.num_local_trigs &&
                       i < (orte_gpr_replica_globals.local_triggers)->size; i++) {
            if (NULL != local_trigs[i]) {
                j++;
                if (msg->id == local_trigs[i]->id) {
                    trig_cb = local_trigs[i]->callback;
                    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
                    trig_cb(msg); /* JJH This is a potential thread problem. Needs a deeper look */
                    return ORTE_SUCCESS;
                }
            }
        }
        /* trigger could not be found */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return ORTE_ERR_NOT_FOUND;
    }

    /* get here if this wasn't a trigger message. Only other allowed message type
     * is a subscription message - if that isn't the case, then we have corrupt
     * data, so flag it and return
     */
    if (ORTE_GPR_SUBSCRIPTION_MSG != msg->msg_type) {
        ORTE_ERROR_LOG(ORTE_ERR_GPR_DATA_CORRUPT);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return ORTE_ERR_GPR_DATA_CORRUPT;
    }

    /* get here if we have a subscription message - i.e., the message should
     * be broken into its component parts and delivered separately
     * to the indicated subscribers
     */
    data = (orte_gpr_notify_data_t**)(msg->data)->addr;
    for (i=0, n=0; n < msg->cnt &&
                   i < (msg->data)->size; i++) {
        if (NULL != data[i]) {
            n++;
            if (ORTE_GPR_SUBSCRIPTION_ID_MAX != data[i]->id || NULL != data[i]->target) {
                /* for each datagram in the message, we need to lookup
                 * the associated subscription (could be specified by name or id) to find the correct
                 * callback function. Name specifications are given precedence over id.
                 */
                local_subs = (orte_gpr_replica_local_subscriber_t**)
                                (orte_gpr_replica_globals.local_subscriptions)->addr;
                sub = NULL;
                for (j=0, k=0; k < orte_gpr_replica_globals.num_local_subs &&
                               j < (orte_gpr_replica_globals.local_subscriptions)->size; j++) {
                    if (NULL != local_subs[j]) {
                        k++;
                        if (NULL != data[i]->target) {
                            /* if target name provided, must use it */
                            if (NULL != local_subs[j]->name &&
                                0 == strcmp(data[i]->target, local_subs[j]->name)) {
                                sub = local_subs[j];
                                break;
                            }
                        } else if (data[i]->id == local_subs[j]->id) {
                            /* otherwise, see if id's match */
                            sub = local_subs[j];
                            break;
                        }
                    }
                }

                /* get here and not found => abort */
                if (NULL == sub ) {
                    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
                    return ORTE_ERR_NOT_FOUND;
                }

                sub_cb = sub->callback;
                sub_usertag = sub->user_tag;
                OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
                sub_cb(data[i], sub_usertag); /* JJH This is a potential thread problem. Needs a deeper look */
                OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

                if (data[i]->remove) {
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_remove_local_subscription(sub))) {
                        ORTE_ERROR_LOG(rc);
                        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
                        return rc;
                    }
                }
            }
        }
    }
    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    /* the calling program will release the message object */
    return ORTE_SUCCESS;
}
