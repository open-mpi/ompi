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
 * The Open MPI General Purpose Registry - Proxy component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"
#include "orte/dss/dss.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "orte/util/proc_info.h"

#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/oob/oob_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"

#include "gpr_proxy.h"


int orte_gpr_proxy_deliver_notify_msg(orte_gpr_notify_message_t *msg)
{
    orte_gpr_notify_data_t **data;
    orte_gpr_proxy_subscriber_t **subs, *sub;
    orte_gpr_proxy_trigger_t **trigs;
    orte_std_cntr_t i, j, k, n;
    int rc;

    OPAL_THREAD_LOCK(&orte_gpr_proxy_globals.mutex);

    OPAL_TRACE(1);

    /* we first have to check if the message is a trigger message - if so,
     * then the message is intended to be
     * sent as a single block to that trigger's callback function.
     */
    if (ORTE_GPR_TRIGGER_MSG == msg->msg_type) {
        trigs = (orte_gpr_proxy_trigger_t**)(orte_gpr_proxy_globals.triggers)->addr;
        for (i=0, j=0; j < orte_gpr_proxy_globals.num_trigs &&
                       i < (orte_gpr_proxy_globals.triggers)->size; i++) {
            if (NULL != trigs[i]){
                j++;
                if (msg->id == trigs[i]->id) {
                    OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
                    trigs[i]->callback(msg);
                    OPAL_THREAD_LOCK(&orte_gpr_proxy_globals.mutex);
                    rc = ORTE_SUCCESS;
                    if (msg->remove) {
                        /* remove the specified trigger from the local tracker */
                        if (ORTE_SUCCESS != (rc = orte_gpr_proxy_remove_trigger(trigs[i]))) {
                            ORTE_ERROR_LOG(rc);
                        }
                    }
                    OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
                    return rc;
                }
            }
        }

        /* must not have been found - report error */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return ORTE_ERR_NOT_FOUND;
    }


    /* get here if this wasn't a trigger message. Only other allowed message type
     * is a subscription message - if that isn't the case, then we have corrupt
     * data, so flag it and return
     */
    if (ORTE_GPR_SUBSCRIPTION_MSG != msg->msg_type) {
        ORTE_ERROR_LOG(ORTE_ERR_GPR_DATA_CORRUPT);
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
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
                subs = (orte_gpr_proxy_subscriber_t**)
                                (orte_gpr_proxy_globals.subscriptions)->addr;
                sub = NULL;
                for (j=0, k=0; k < orte_gpr_proxy_globals.num_subs &&
                               j < (orte_gpr_proxy_globals.subscriptions)->size; j++) {
                    if (NULL != subs[j]) {
                        k++;
                        if (NULL != data[i]->target) {
                            /* if target name provided, must use it */
                            if (NULL != subs[j]->name &&
                                0 == strcmp(data[i]->target, subs[j]->name)) {
                                sub = subs[j];
                                break;
                            }
                        } else if (data[i]->id == subs[j]->id) {
                            /* otherwise, see if id's match */
                            sub = subs[j];
                            break;
                        }
                    }
                }
                /* get here and not found => abort */
                if (NULL == sub) {
                    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                    OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
                    return ORTE_ERR_NOT_FOUND;
                }
                OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
                sub->callback(data[i], sub->user_tag);
                OPAL_THREAD_LOCK(&orte_gpr_proxy_globals.mutex);

                if (data[i]->remove) {
                    if (ORTE_SUCCESS != (rc = orte_gpr_proxy_remove_subscription(sub))) {
                        ORTE_ERROR_LOG(rc);
                        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
                        return rc;
                    }
                }
            }
        }
    }

    /* all done */
    OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
    return ORTE_SUCCESS;
}
