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
 * The Open MPI General Purpose Registry - Proxy component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "dps/dps.h"
#include "opal/util/output.h"
#include "util/proc_info.h"

#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"
#include "mca/oob/oob_types.h"
#include "mca/rml/rml.h"
#include "mca/errmgr/errmgr.h"

#include "gpr_proxy.h"


int orte_gpr_proxy_deliver_notify_msg(orte_gpr_notify_message_t *msg)
{
    orte_gpr_notify_data_t **data;
    orte_gpr_proxy_subscriber_t **subs, *sub;
    orte_gpr_proxy_trigger_t *trig;
    size_t i, j, k;
    bool processed;
    int rc;

    /* if the message trigger id is valid (i.e., it is set to
     * something other than ORTE_GPR_TRIGGER_ID_MAX), then this
     * is an aggregated message intended for a single receiver.
     * In that case, look up the associated TRIGGER id and pass
     * the entire message to that receiver.
     */
    if (ORTE_GPR_TRIGGER_ID_MAX > msg->id) {
       trig = (orte_gpr_proxy_globals.triggers)->addr[msg->id];
       if (NULL == trig) {
           ORTE_ERROR_LOG(ORTE_ERR_GPR_DATA_CORRUPT);
opal_output(0, "Trigger id: %lu", (unsigned long)msg->id);
orte_gpr.dump_local_triggers(0);
           return ORTE_ERR_GPR_DATA_CORRUPT;
       } else {
           trig->callback(msg);
       }
       if (msg->remove) {
           if (ORTE_SUCCESS != (rc = orte_gpr_proxy_remove_trigger(msg->id))) {
               ORTE_ERROR_LOG(rc);
               return rc;
           }
       }
       return ORTE_SUCCESS;
    }
    
    
    /* if the message trigger id was NOT valid, then we split the
     * message into its component datagrams and send each of them
     * separately to their respective subscriber.
     */
    data = (orte_gpr_notify_data_t**)(msg->data)->addr;
orte_gpr.dump_local_subscriptions(0);
    for (i=0; i < msg->cnt; i++) {
opal_output(0, "[%lu,%lu,%lu] Sub data id %lu", ORTE_NAME_ARGS(orte_process_info.my_name), (unsigned long)data[i]->id);
opal_output(0, "\tSub name %s", data[i]->target);
        /* for each datagram in the message, we need to lookup
         * the associated subscription (could be specified by name or id) to find the correct
         * callback function. Name specifications are given precedence over id.
         */
        subs = (orte_gpr_proxy_subscriber_t**)
                        (orte_gpr_proxy_globals.subscriptions)->addr;
        processed = false;
        for (j=0, k=0; !processed &&
                       k < orte_gpr_proxy_globals.num_subs &&
                       j < (orte_gpr_proxy_globals.subscriptions)->size; j++) {
            if (NULL != subs[j]) {
                k++;
                if ((NULL != subs[j]->name &&
                    NULL != data[i]->target &&
                    0 == strcmp(data[i]->target, subs[j]->name)) ||
                    (data[i]->id == subs[j]->id)) {
                    sub = subs[j];
                    processed = true;
                }
            }
        }
        /* get here and not processed => not found, abort */
        if (!processed) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        sub->callback(data[i], sub->user_tag);
        OPAL_THREAD_LOCK(&orte_gpr_proxy_globals.mutex);

        if (data[i]->remove) {
            if (ORTE_SUCCESS != (rc = orte_gpr_proxy_remove_subscription(sub))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }

    /* all done */
    return ORTE_SUCCESS;
}
