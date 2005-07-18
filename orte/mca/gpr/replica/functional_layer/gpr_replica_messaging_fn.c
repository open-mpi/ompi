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
#include "mca/schema/schema.h"

#include "opal/util/output.h"
#include "util/proc_info.h"

#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"

#include "mca/gpr/base/base.h"
#include "mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "mca/gpr/replica/communications/gpr_replica_comm.h"
#include "gpr_replica_fn.h"

int orte_gpr_replica_process_callbacks(void)
{
    orte_gpr_replica_callbacks_t *cb;
    orte_gpr_notify_data_t **data;
    orte_gpr_replica_trigger_t **trigs;
    orte_gpr_replica_local_trigger_t **local_trigs;
    orte_gpr_replica_subscription_t **subs;
    orte_gpr_replica_local_subscriber_t **local_subs;
    orte_gpr_replica_requestor_t **reqs;
    size_t i, j, k, m;
    bool processed;
    int rc;

    /* check and set flag indicating callbacks being processed */
    if (orte_gpr_replica.processing_callbacks) {
        return ORTE_SUCCESS;
    }
    orte_gpr_replica.processing_callbacks = true;
    
    while (NULL != (cb = (orte_gpr_replica_callbacks_t*)opal_list_remove_last(&orte_gpr_replica.callbacks))) {
        if (NULL == cb->requestor) {  /* local callback */
            /* each callback corresponds to a specific requestor
             * The message in the callback consists of at least one (and can
             * be more) "datagrams" intended for that requestor, each of which
             * is slated to be returned to a specific
             * function on the requestor.
             * 
             * Since this requestor is "local", we simply execute
             * the callbacks ourself.
             */
            /* we first have to check the trigger id in the message. If that
             * field is set to a valid value (i.e., one other than
             * ORTE_GPR_TRIGGER_ID_MAX), then the message is intended to be
             * sent as a single block to that trigger's callback function.
             */
            if (ORTE_GPR_TRIGGER_ID_MAX > (cb->message)->id) {
                /* use the local trigger callback */
                local_trigs = (orte_gpr_replica_local_trigger_t**)
                                    (orte_gpr_replica_globals.local_triggers)->addr;
                for (i=0, j=0; j < orte_gpr_replica_globals.num_local_trigs &&
                               i < (orte_gpr_replica_globals.local_triggers)->size; i++) {
                    if (NULL != local_trigs[i]) {
                        j++;
                        if ((cb->message)->id == local_trigs[i]->id) {
                            OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
                            local_trigs[i]->callback(cb->message, local_trigs[i]->user_tag);
                            OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);
                            goto CLEANUP;
                        }
                    }
                }
                /* get here if the trigger could not be found */
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                goto CLEANUP;
            }
            
            /* get here if the trigger id indicated that this was NOT
             * intended for a trigger callback - i.e., the message should
             * be broken into its component parts and delivered separately
             * to the indicated subscribers
             */
            data = (orte_gpr_notify_data_t**)((cb->message)->data)->addr;
            for (i=0; i < (cb->message)->cnt; i++) {
                /* for each datagram in the message, we need to lookup
                 * the associated subscription id to find the correct
                 * callback function.
                 */
                local_subs = (orte_gpr_replica_local_subscriber_t**)
                                (orte_gpr_replica_globals.local_subscriptions)->addr;
                processed = false;
                for (j=0, k=0; !processed &&
                               k < orte_gpr_replica_globals.num_local_subs &&
                               j < (orte_gpr_replica_globals.local_subscriptions)->size; j++) {
                    if (NULL != local_subs[j]) {
                        k++;
                        if (data[i]->id == local_subs[j]->id) {
                            OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
                            local_subs[j]->callback(data[i], local_subs[j]->user_tag);
                            OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);
                            processed = true;
                        }
                    }
                }
                /* get here and not processed => not found */
                if (!processed) {
                    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                }
            }
            
        } else {  /* remote request - send messages back */
           orte_gpr_replica_remote_notify(cb->requestor, cb->message);
        }

CLEANUP:
        OBJ_RELEASE(cb);
    }

    /* cleanup any one-shot triggers that fired */
    trigs = (orte_gpr_replica_trigger_t**)((orte_gpr_replica.triggers)->addr);
    for (i=0, k=0, m=0; k < orte_gpr_replica.num_trigs &&
                   i < (orte_gpr_replica.triggers)->size; i++) {
        if (NULL != trigs[i]) {
            k++;
            if (trigs[i]->one_shot_fired) {
                OBJ_RELEASE(trigs[i]);
                orte_pointer_array_set_item(orte_gpr_replica.triggers, i, NULL);
                m++;
            }
        }
    }
    orte_gpr_replica.num_trigs -= m;
    
    /* cleanup any subscriptions that are supposed to be
     * removed based on a trigger having fired
     */
    subs = (orte_gpr_replica_subscription_t**)(orte_gpr_replica.subscriptions)->addr;
    for (i=0, k=0; k < orte_gpr_replica.num_subs &&
                   i < (orte_gpr_replica.subscriptions)->size; i++) {
        if (NULL != subs[i]) {
            k++;
            if (subs[i]->cleanup) {
                reqs = (orte_gpr_replica_requestor_t**)(subs[i]->requestors)->addr;
                for (j=0, m=0; NULL != subs[i] &&
                               m < subs[i]->num_requestors &&
                               j < (subs[i]->requestors)->size; j++) {
                    if (NULL != reqs[j]) {
                        m++;
                        if (ORTE_SUCCESS != (rc =
                                orte_gpr_replica_remove_subscription(reqs[j]->requestor, reqs[j]->idtag))) {
                            ORTE_ERROR_LOG(rc);
                            return rc;
                        }
                    }
                }
            }
        }
    }

    /* all callbacks processed - indicate list is open */
    orte_gpr_replica.processing_callbacks = false;
    
    return ORTE_SUCCESS;
}



int orte_gpr_replica_register_callback(orte_gpr_replica_trigger_t *trig,
                                       orte_gpr_replica_subscription_t *sub,
                                       orte_gpr_value_t *value)
{
    orte_gpr_replica_callbacks_t *cb;
    orte_gpr_replica_requestor_t **reqs;
    size_t interim, cnt, num_tokens, num_keys;
    orte_gpr_value_t **vals, **values;
    orte_gpr_replica_ivalue_t **ivals;
    size_t i, j, k;
    bool cleanup_reqd;
    int rc;
    
    /* The data to be returned will be the same for all requestors
     * on this subscription. First, let's get the data (if it hasn't
     * already been provided) so we have it ready to be added to
     * the callback
     */
     
    /* check to see if value provided - if so, we'll just use it */
    if (NULL != value) {
        values = &value;
        cnt = 1;
        cleanup_reqd = false;
    } else {
        /* value not provided - get the data off the registry. since a
         * subscription can have multiple data sources specified, we
         * have to loop through those sources, constructing an aggregated
         * array of data values that we can work with in composing the
         * final message
         */
        ivals = (orte_gpr_replica_ivalue_t**)(sub->values)->addr;
        cnt = 0;
        values = NULL;
        for (i=0, j=0; j < sub->num_values &&
                       i < (sub->values)->size; i++) {
            if (NULL != ivals[i]) {
                j++;
                num_tokens = orte_value_array_get_size(&(ivals[i]->tokentags));
                num_keys = orte_value_array_get_size(&(ivals[i]->keytags));
                /* get the data for this description off the registry */
                if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_fn(ivals[i]->addr_mode,
                        ivals[i]->seg,
                        ORTE_VALUE_ARRAY_GET_BASE(&(ivals[i]->tokentags), orte_gpr_replica_itag_t),
                        num_tokens,
                        ORTE_VALUE_ARRAY_GET_BASE(&(ivals[i]->keytags), orte_gpr_replica_itag_t),
                        num_keys,
                        &interim, &vals))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                /* if we don't get any data back, just continue - don't
                 * try to add it to the values since that would cause a
                 * zero-byte malloc
                 */
                if (0 == interim) {
                    continue;
                }
                /* add these results to those we have already obtained */
                if (0 == cnt) { /* first time through */
                    values = (orte_gpr_value_t**)malloc(interim *
                                                    sizeof(orte_gpr_value_t*));
                    if (NULL == values) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                } else {
                    /* reallocate values array */
                    values = (orte_gpr_value_t**)realloc(values,
                                    (cnt+interim)*sizeof(orte_gpr_value_t*));
                    if (NULL == values) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                }
                /* add data to end of array */
                for (k=0; k < interim; k++) {
                    values[k+cnt] = vals[k];
                }
                /* release the array of pointers - the pointers themselves
                 * will remain "alive" in the values array to be released
                 * later
                 */
                free(vals);
                /* update the count */
                cnt += interim;
            }
        }
        cleanup_reqd = true;
    }

    /* We now have the data to be sent to each requestor attached
     * to this subscription.
     * Each subscription that was placed on the system has an associated
     * structure containing the process name and array of callback info where
     * data is to be returned. For remote processes, the callback
     * info is omitted and a subscription id is recorded - this tells
     * the remote process which callback function to use when it receives
     * a message from us.
     * Each subscription can have multiple "requestors" attached to it,
     * each "requestor" consisting of the process name and 
     * subscription id (for remote processes), and callback info (for local
     * processes).
     * For each requestor, we need to check to see if a callback has
     * already been scheduled to that destination - if so, we piggyback
     * another datagram onto it to minimize communication costs.
     */
    
    /* first, we need to determine if the data in this message
     * is to be sent back through the trigger callback function
     * or not. if it is, then we set the callback's message
     * to point at the correct trigger id for that requestor
     * so the message goes to the correct place, and we go ahead
     * and store the data in the message
     */
    if (NULL != trig && NULL != trig->master) {
        /* define the callback */
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_define_callback(&cb, (trig->master)->requestor))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        /* transfer the trigger name, if available */
        if (NULL != trig->name) {
            (cb->message)->name = strdup(trig->name);
        }
        /* set the callback id to point to the trigger callback function */
        (cb->message)->id = (trig->master)->idtag;
        /* cycle through all the subscription's requestors and place
         * the data on the message so that the trigger master can distribute
         * it as required
         */
        reqs = (orte_gpr_replica_requestor_t**)(sub->requestors)->addr;
        for (i=0, j=0; j < sub->num_requestors &&
                       i < (sub->requestors)->size; i++) {
            if (NULL != reqs[i]) {
                j++;
                if (ORTE_SUCCESS != (rc = orte_gpr_replica_store_value_in_msg(reqs[i]->idtag,
                                                cb->message, cnt, values))) {
                    ORTE_ERROR_LOG(rc);
                    goto CLEANUP;
                }
            }
        }
    } else {
        /* this data is intended to be sent to the individual
         * subscribers themselves. Cycle through the subscription's
         * requestors, define callbacks to them appropriately,
         * and set the id to indicate that it does NOT go
         * to a trigger
         */
        reqs = (orte_gpr_replica_requestor_t**)(sub->requestors)->addr;
        for (i=0, j=0; j < sub->num_requestors &&
                       i < (sub->requestors)->size; i++) {
            if (NULL != reqs[i]) {
                j++;
                /* define the callback */
                if (ORTE_SUCCESS != (rc = orte_gpr_replica_define_callback(&cb, reqs[i]->requestor))) {
                    ORTE_ERROR_LOG(rc);
                    goto CLEANUP;
                }
                /* set the callback id to indicate not a trigger callback */
                (cb->message)->id = ORTE_GPR_TRIGGER_ID_MAX;
                /* okay, now we have a message going to the requestor. We need to
                 * store the values in the notify_data structure corresponding to this
                 * subscription id, combining data where the id's match
                 */
                if (ORTE_SUCCESS != (rc = orte_gpr_replica_store_value_in_msg(reqs[i]->idtag,
                                                cb->message, cnt, values))) {
                    ORTE_ERROR_LOG(rc);
                    goto CLEANUP;
                }
            }
        }  /* for i */
    } /* if else */
    
CLEANUP:
    /* release the values here - the value objects have been "retained" in
     * the store_value function, so this just ensures that they will be
     * released after the last datagram lets go of them
     */
    for (i=0; i < cnt; i++) OBJ_RELEASE(values[i]);
    /* release the values array IF and only IF it was malloc'd here.
     * otherwise, the value is coming in from the outside - when that happens,
     * only a single value is passed in, so there is no array to free
     */
    if (cleanup_reqd) free(values);
    
    return rc;
}


int orte_gpr_replica_define_callback(orte_gpr_replica_callbacks_t **cbptr,
                                     orte_process_name_t *recipient)
{
    orte_gpr_replica_callbacks_t *cb;
    int rc;
    
    /* see if a callback has already been registered for this recipient */
    for (cb = (orte_gpr_replica_callbacks_t*)opal_list_get_first(&(orte_gpr_replica.callbacks));
         cb != (orte_gpr_replica_callbacks_t*)opal_list_get_end(&(orte_gpr_replica.callbacks));
         cb = (orte_gpr_replica_callbacks_t*)opal_list_get_next(cb)) {

         if ((NULL == recipient && NULL == cb->requestor) ||
             ((NULL != recipient && NULL != cb->requestor) &&
              (0 == orte_ns.compare(ORTE_NS_CMP_ALL,
                                    recipient,
                                    cb->requestor)))) {
             /* okay, a callback has been registered to send data to this
              * recipient - return this location
              */
             *cbptr = cb;
             return ORTE_SUCCESS;
         }
    }
    
    /* this is going to somebody new - create a new callback
     * for this recipient
     */
    cb = OBJ_NEW(orte_gpr_replica_callbacks_t);
    if (NULL == cb) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    opal_list_append(&orte_gpr_replica.callbacks, &cb->item);
    
    /* construct the message */
    cb->message = OBJ_NEW(orte_gpr_notify_message_t);
    if (NULL == cb->message) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (NULL == recipient) {
        cb->requestor = NULL;
    } else {
        if (ORTE_SUCCESS != (rc = orte_ns.copy_process_name(&(cb->requestor), recipient))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /* return the pointer to the new callback */
    *cbptr = cb;
    
    return ORTE_SUCCESS;
}


int orte_gpr_replica_store_value_in_msg(orte_gpr_subscription_id_t id,
                                        orte_gpr_notify_message_t *msg,
                                        size_t cnt,
                                        orte_gpr_value_t **values)
{
    size_t i, j, k, index;
    orte_gpr_notify_data_t **data, *dptr;
    
    /* check to see if this data is going to the same place as
     * any prior data on the message. if so, then we add the values
     * to that existing data structure. if not, then we realloc to
     * establish a new data structure and store the data there
     */
    data = (orte_gpr_notify_data_t**)(msg->data)->addr;
    for (i=0, k=0; k < msg->cnt &&
                   i < (msg->data)->size; i++) {
        if (NULL != data[i]) {
            k++;
            if (data[i]->id == id) { /* going to the same place */
                for (j=0; j < cnt; j++) {
                    if (0 > orte_pointer_array_add(&index, data[i]->values, values[j])) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                }
                /* must "retain" the value object to ensure that it is
                 * there for this datagram. Since we are only storing
                 * pointers to the object (and not actually copying it),
                 * datagrams may wind up sharing the object. Hence, when
                 * a datagram is released, it will release the object. Without
                 * the retain, the next datagram that shares that object
                 * will see trash
                 */
                OBJ_RETAIN(values[j]);
                data[i]->cnt += cnt;
                return ORTE_SUCCESS;
            }
        }
    }

    /* no prior matching data found, so add another data location to
     * the message and store the values there
     */
    dptr = OBJ_NEW(orte_gpr_notify_data_t);
    if (NULL == dptr) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    dptr->id = id;
    if (0 > orte_pointer_array_add(&index, msg->data, dptr)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    (msg->cnt)++;
    for (j=0; j < cnt; j++) {
        if (0 > orte_pointer_array_add(&index, dptr->values, values[j])) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* must "retain" the value object to ensure that it is
         * there for this datagram. Since we are only storing
         * pointers to the object (and not actually copying it),
         * datagrams may wind up sharing the object. Hence, when
         * a datagram is released, it will release the object. Without
         * the retain, the next datagram that shares that object
         * will see trash
         */
        OBJ_RETAIN(values[j]);
    }
    dptr->cnt = cnt;
    return ORTE_SUCCESS;
}

