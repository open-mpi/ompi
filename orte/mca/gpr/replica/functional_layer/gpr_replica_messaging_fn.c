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

#include "opal/util/output.h"

#include "orte/dss/dss.h"
#include "orte/util/proc_info.h"

#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/schema/schema.h"

#include "orte/mca/gpr/base/base.h"
#include "orte/mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "orte/mca/gpr/replica/communications/gpr_replica_comm.h"
#include "gpr_replica_fn.h"

static int orte_gpr_replica_get_callback_data(orte_gpr_value_t ***values, orte_std_cntr_t *num_vals,
                                              orte_gpr_replica_subscription_t *sub);

static int orte_gpr_replica_store_value_in_trigger_msg(orte_gpr_replica_subscription_t *sub,
                                                       orte_gpr_notify_message_t *msg,
                                                       orte_std_cntr_t cnt,
                                                       orte_gpr_value_t **values);

int orte_gpr_replica_process_callbacks(void)
{
    orte_gpr_replica_callbacks_t *cb;
    orte_gpr_replica_trigger_t **trigs;
    orte_gpr_replica_subscription_t **subs;
    orte_gpr_replica_requestor_t **reqs;
    orte_std_cntr_t i, j, k, m;
    int rc;

    /* check and set flag indicating callbacks being processed */
    if (orte_gpr_replica.processing_callbacks) {
        return ORTE_SUCCESS;
    }
    orte_gpr_replica.processing_callbacks = true;

    while (NULL != (cb = (orte_gpr_replica_callbacks_t*)opal_list_remove_last(&orte_gpr_replica.callbacks))) {
        /* each callback corresponds to a specific requestor
         * The message in the callback consists of at least one (and can
         * be more) "datagrams" intended for that requestor, each of which
         * is slated to be returned to a specific function on the requestor.
         */
        if (NULL == cb->requestor) {  /* local callback */
            /* Since this requestor is "local", we simply execute
             * the callbacks ourself.
             */
            OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_deliver_notify_msg(cb->message))) {
                ORTE_ERROR_LOG(rc);
            }
            OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);
        } else {  /* remote request - send messages back */
           orte_gpr_replica_remote_notify(cb->requestor, cb->message);
        }

        OBJ_RELEASE(cb);
    }

    /* cleanup any one-shot triggers that fired and set processing to
     * false on all others
     */
    trigs = (orte_gpr_replica_trigger_t**)((orte_gpr_replica.triggers)->addr);
    for (i=0, k=0, m=0; k < orte_gpr_replica.num_trigs &&
                   i < (orte_gpr_replica.triggers)->size; i++) {
        if (NULL != trigs[i]) {
            k++;
            if (trigs[i]->one_shot_fired) {
                OBJ_RELEASE(trigs[i]);
                orte_pointer_array_set_item(orte_gpr_replica.triggers, i, NULL);
                m++;
            } else {
                trigs[i]->processing = false;
            }
        }
    }
    orte_gpr_replica.num_trigs -= m;

    /* cleanup any subscriptions that are supposed to be
     * removed based on a trigger having fired - set processing to false
     * on all others
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
            } else {
                subs[i]->processing = false;
            }
        }
    }

    /* all callbacks processed - indicate list is open */
    orte_gpr_replica.processing_callbacks = false;

    return ORTE_SUCCESS;
}



int orte_gpr_replica_register_callback(orte_gpr_replica_subscription_t *sub,
                                       orte_gpr_value_t *value)
{
    orte_gpr_replica_callbacks_t *cb;
    orte_gpr_replica_requestor_t **reqs;
    orte_gpr_value_t **values;
    orte_std_cntr_t cnt;
    orte_std_cntr_t i, j;
    bool cleanup_reqd;
    int rc=ORTE_SUCCESS;

    /* The data to be returned will be the same for all requestors
     * on this subscription. First, let's get the data (if it hasn't
     * already been provided) so we have it ready to be added to
     * the callback
     */
    if (NULL != value) { /* no need to get data - already provided */
        values = &value;
        cnt = 1;
        cleanup_reqd = false;
    } else {
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_callback_data(&values, &cnt, sub))) {
            ORTE_ERROR_LOG(rc);
            return rc;
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
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_define_callback(ORTE_GPR_SUBSCRIPTION_MSG,
                                            &cb, reqs[i]->requestor))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            /* set the callback id to indicate not a trigger callback */
            (cb->message)->id = ORTE_GPR_TRIGGER_ID_MAX;
            /* okay, now we have a message going to the requestor. We need to
             * store the values in the notify_data structure corresponding to this
             * subscription id, combining data where the id's match
             */
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_store_value_in_msg(reqs[i],
                                            cb->message, sub->name, cnt, values))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
        }
    }  /* for i */

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
    if (cleanup_reqd && NULL != values) free(values);

    return rc;
}


int orte_gpr_replica_register_trigger_callback(orte_gpr_replica_trigger_t *trig)
{
    orte_gpr_replica_callbacks_t *cb;
    orte_gpr_replica_counter_t **cntr;
    orte_gpr_replica_subscription_t **subs;
    orte_gpr_value_t **values, *value;
    orte_std_cntr_t i, j, k, cnt;
    int rc;

    /* set the callback's message
     * to point at the correct trigger id for that requestor
     * so the message goes to the correct place, and go ahead
     * and store the data in the message
     */
    /* define the callback */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_define_callback(ORTE_GPR_TRIGGER_MSG,
                                    &cb, (trig->master)->requestor))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* transfer the trigger name, if available */
    if (NULL != trig->name) {
        (cb->message)->target = strdup(trig->name);
    }
    /* set the callback id to point to the trigger callback function */
    (cb->message)->id = (trig->master)->idtag;

    /* if the trigger counters are to be included, do so */
    if (ORTE_GPR_TRIG_INCLUDE_TRIG_CNTRS & trig->action) {
        cntr = (orte_gpr_replica_counter_t**)((trig->counters)->addr);
        for (i=0, j=0; j < trig->num_counters &&
                       i < (trig->counters)->size; i++) {
            if (NULL != cntr[i]) {
                j++;
                value = OBJ_NEW(orte_gpr_value_t);
                if (NULL == value) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                value->segment = strdup(cntr[i]->seg->name);
                value->cnt = 1;
                value->keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
                if (NULL == value->keyvals) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                value->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
                if (NULL == value->keyvals[0]) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                if (ORTE_SUCCESS != (rc = orte_gpr_replica_dict_reverse_lookup(
                                        &(value->keyvals[0]->key), cntr[i]->seg,
                                        cntr[i]->iptr->itag))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(value);
                    return rc;
                }
                value->keyvals[0]->value = OBJ_NEW(orte_data_value_t);
                if (NULL == value->keyvals[0]->value) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                value->keyvals[0]->value->type = cntr[i]->iptr->value->type;
                if (ORTE_SUCCESS != (rc = orte_dss.copy(&((value->keyvals[0]->value)->data), cntr[i]->iptr->value->data, cntr[i]->iptr->value->type))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(value);
                    return rc;
                }
               /*
                * store the data in the message
                */
                if (ORTE_SUCCESS != (rc = orte_gpr_replica_store_value_in_trigger_msg(NULL,
                                                cb->message, 1, &value))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                /* release the storage */
                OBJ_RELEASE(value);
            }
        }
    }

    /* cycle through all the trigger's subscriptions and place
     * that data on the message
     */
    subs = (orte_gpr_replica_subscription_t**)(trig->subscriptions)->addr;
    for (i=0, j=0; j < trig->num_subscriptions &&
                   i < (trig->subscriptions)->size; i++) {
        if (NULL != subs[i]) {
            j++;
            if (NULL != subs[i]->name) {
                /* if it's a named subscription, we will deliver it via the
                 * trigger callback function. The data to be returned will
                 * be the same for all requestors.
                 */
                if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_callback_data(&values, &cnt, subs[i]))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                /*
                 * store the data in the message
                 */
                if (ORTE_SUCCESS != (rc = orte_gpr_replica_store_value_in_trigger_msg(subs[i],
                                                cb->message, cnt, values))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                /* release the storage */
                for (k=0; k < cnt; k++) OBJ_RELEASE(values[k]);
                if (NULL != values) free(values);
            } else {
                /* in the case of a non-named subscription, we know that someone
                 * has attached a subscription to this trigger, and that the
                 * requestor needs the data to be returned directly to them. This
                 * occurs in the case of orterun, which attaches subscriptions to
                 * the standard triggers so it can monitor the progress of a job
                 * it has launched. To facilitate this, we register a separate
                 * callback for this subscription
                 */
                if (ORTE_SUCCESS != (rc = orte_gpr_replica_register_callback(subs[i], NULL))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }
    }
    return ORTE_SUCCESS;
}


int orte_gpr_replica_define_callback(orte_gpr_notify_msg_type_t msg_type,
                                     orte_gpr_replica_callbacks_t **cbptr,
                                     orte_process_name_t *recipient)
{
    orte_gpr_replica_callbacks_t *cb;
    int rc;

    /* see if a callback has already been registered for this recipient */
    for (cb = (orte_gpr_replica_callbacks_t*)opal_list_get_first(&(orte_gpr_replica.callbacks));
         cb != (orte_gpr_replica_callbacks_t*)opal_list_get_end(&(orte_gpr_replica.callbacks));
         cb = (orte_gpr_replica_callbacks_t*)opal_list_get_next(cb)) {
        /* must check to see if both the recipient is the same AND that the
         * message type being sent is identical (i.e., that messages going back
         * to trigger callbacks do NOT get mixed with messages going back to
         * subscription callbacks). This is critical as the deliver_notify_msg
         * functions handle these message types in different ways
         */
         if (((NULL == recipient && NULL == cb->requestor) &&
              (msg_type == cb->message->msg_type)) ||
             (((NULL != recipient && NULL != cb->requestor) &&
              (ORTE_EQUAL == orte_dss.compare(recipient, cb->requestor, ORTE_NAME))) &&
               (msg_type == cb->message->msg_type))) {
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
    cb->message->msg_type = msg_type;

    if (NULL == recipient) {
        cb->requestor = NULL;
    } else {
        if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&(cb->requestor), recipient, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* return the pointer to the new callback */
    *cbptr = cb;

    return ORTE_SUCCESS;
}


int orte_gpr_replica_store_value_in_msg(orte_gpr_replica_requestor_t *req,
                                        orte_gpr_notify_message_t *msg,
                                        char *sub_name,
                                        orte_std_cntr_t cnt,
                                        orte_gpr_value_t **values)
{
    orte_std_cntr_t i, j, k, index;
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
            if (data[i]->id == req->idtag) { /* going to the same place */
                for (j=0; j < cnt; j++) {
                    if (0 > orte_pointer_array_add(&index, data[i]->values, values[j])) {
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
    /* set the name of the subscription, if provided */
    if (NULL != sub_name) {
        dptr->target = strdup(sub_name);
    }
    dptr->id = req->idtag;
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

static int orte_gpr_replica_store_value_in_trigger_msg(orte_gpr_replica_subscription_t *sub,
                                                       orte_gpr_notify_message_t *msg,
                                                       orte_std_cntr_t cnt,
                                                       orte_gpr_value_t **values)
{
    orte_std_cntr_t i, j, k, index;
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
            if ((NULL == data[i]->target && NULL == sub) ||
                 (NULL != data[i]->target && NULL != sub->name &&
                 0 == strcmp(data[i]->target, sub->name))) { /* going to the same place */
                for (j=0; j < cnt; j++) {
                    if (0 > orte_pointer_array_add(&index, data[i]->values, values[j])) {
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
    if (NULL != sub && NULL != sub->name) {
        dptr->target = strdup(sub->name);
    }
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

static int orte_gpr_replica_get_callback_data(orte_gpr_value_t ***ret_values, orte_std_cntr_t *cnt,
                                              orte_gpr_replica_subscription_t *sub)
{
    orte_gpr_value_t **vals, **values;
    orte_gpr_replica_ivalue_t **ivals;
    orte_std_cntr_t i, j, k, num_tokens, num_keys, interim, count;
    int rc;

    /* setup default error returns */
    *ret_values = NULL;
    *cnt = 0;

    /* get the data off the registry. since a
     * subscription can have multiple data sources specified, we
     * have to loop through those sources, constructing an aggregated
     * array of data values that we can work with in composing the
     * final message
     */
    ivals = (orte_gpr_replica_ivalue_t**)(sub->values)->addr;
    count = 0;
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
            if (0 == count) { /* first time through */
                values = (orte_gpr_value_t**)malloc(interim *
                                                sizeof(orte_gpr_value_t*));
                if (NULL == values) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
            } else {
                /* reallocate values array */
                values = (orte_gpr_value_t**)realloc(values,
                                (count+interim)*sizeof(orte_gpr_value_t*));
                if (NULL == values) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
            }
            /* add data to end of array */
            for (k=0; k < interim; k++) {
                values[k+count] = vals[k];
            }
            /* release the array of pointers - the pointers themselves
             * will remain "alive" in the values array to be released
             * later
             */
            free(vals);
            /* update the count */
            count += interim;
        }
    }
    *ret_values = values;
    *cnt = count;
    return ORTE_SUCCESS;
}
