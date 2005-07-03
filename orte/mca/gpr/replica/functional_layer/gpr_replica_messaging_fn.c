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

#include "util/output.h"
#include "util/proc_info.h"

#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"

#include "mca/gpr/base/base.h"
#include "mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "mca/gpr/replica/communications/gpr_replica_comm.h"
#include "gpr_replica_fn.h"

static int orte_gpr_replica_add_value_to_datagram(orte_gpr_notify_data_t *data,
                        size_t cnt, orte_gpr_value_t **values);

int orte_gpr_replica_process_callbacks(void)
{
    orte_gpr_replica_callbacks_t *cb;
    orte_gpr_notify_data_t **data;
    orte_gpr_replica_trigger_t **trigs;
    orte_gpr_replica_subscription_t **subs;
    orte_gpr_replica_requestor_t **reqs;
    orte_gpr_replica_local_subscriber_t *local_sub;
    size_t i, j, k, m;
    int rc;

    if (orte_gpr_replica_globals.debug) {
	   ompi_output(0, "gpr replica: process_callbacks entered");
    }

    /* check and set flag indicating callbacks being processed */
    if (orte_gpr_replica.processing_callbacks) {
        return ORTE_SUCCESS;
    }
    orte_gpr_replica.processing_callbacks = true;
    
    while (NULL != (cb = (orte_gpr_replica_callbacks_t*)opal_list_remove_last(&orte_gpr_replica.callbacks))) {
	    if (NULL == cb->requestor) {  /* local callback */
	        if (orte_gpr_replica_globals.debug) {
		      ompi_output(0, "process_callbacks: local");
	        }
            /* each callback corresponds to a specific requestor
             * The message in the callback consists of at least one (and can
             * be more) "datagrams" intended for that requestor, each of which
             * is slated to be returned to a specific
             * subscription that corresponds to a specific callback
             * function on the requestor.
             * 
             * Since this requestor is "local", we simply execute
             * the callbacks ourself.
             */
            data = (orte_gpr_notify_data_t**)((cb->message)->data);
            for (i=0; i < (cb->message)->cnt; i++) {
                /* for each datagram in the message, we need to lookup
                 * the associated subscription id to find the correct
                 * callback function. This subscription id is in the
                 * data object itself, and references the local_subscriptions
                 * array of objects.
                 */
                local_sub = (orte_gpr_replica_local_subscriber_t*)
                                (orte_gpr_replica_globals.local_subscriptions)->addr[data[i]->id];
                if (NULL == local_sub) {  /* this subscription has been deleted - error */
                    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                    goto CLEANUP;
                }
                OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
                local_sub->callback(data[i], local_sub->user_tag);
                OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);
            }
            
        	} else {  /* remote request - send messages back */
    	       if (orte_gpr_replica_globals.debug) {
    		      ompi_output(0, "process_callbacks: remote to [%lu,%lu,%lu]",
                        ORTE_NAME_ARGS(cb->requestor));
    	       }
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



int orte_gpr_replica_register_callback(orte_gpr_replica_subscription_t *sub,
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
     
    reqs = (orte_gpr_replica_requestor_t**)(sub->requestors)->addr;
    for (i=0, j=0; j < sub->num_requestors &&
                   i < (sub->requestors)->size; i++) {
        if (NULL != reqs[i]) {
            j++;
            
            /* see if a callback has already been registered for this process */
            for (cb = (orte_gpr_replica_callbacks_t*)opal_list_get_first(&(orte_gpr_replica.callbacks));
                 cb != (orte_gpr_replica_callbacks_t*)opal_list_get_end(&(orte_gpr_replica.callbacks));
                 cb = (orte_gpr_replica_callbacks_t*)opal_list_get_next(cb)) {
    
                 if ((NULL == reqs[i]->requestor && NULL == cb->requestor) ||
                     ((NULL != reqs[i]->requestor && NULL != cb->requestor) &&
                      (0 == orte_ns.compare(ORTE_NS_CMP_ALL,
                                            reqs[i]->requestor,
                                            cb->requestor)))) {
                     /* okay, a callback has been registered to send data to this
                      * process - add to that message
                      */
                     goto PROCESS;
                 }
            }
            
            /* this is going to somebody new - create a new callback
             * for this requestor
             */
            cb = OBJ_NEW(orte_gpr_replica_callbacks_t);
            if (NULL == cb) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
            
            if (NULL == reqs[i]->requestor) {
                cb->requestor = NULL;
            } else {
                if (ORTE_SUCCESS != (rc = orte_ns.copy_process_name(&(cb->requestor), reqs[i]->requestor))) {
                    ORTE_ERROR_LOG(rc);
                    goto CLEANUP;
                }
            }
            opal_list_append(&orte_gpr_replica.callbacks, &cb->item);
            
            /* construct the message */
            cb->message = OBJ_NEW(orte_gpr_notify_message_t);
            if (NULL == cb->message) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }

PROCESS:
            /* okay, now we have a message going to the requestor. We need to
             * store the values in the notify_data structure corresponding to this
             * subscription id, combining data where the id's match
             */
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_store_value_in_msg(reqs[i]->idtag,
                                            cb->message, cnt, values))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
        } /* if NULL */
    }  /* for i */
    
CLEANUP:
    if (cleanup_reqd) {
        for (i=0; i < cnt; i++) OBJ_RELEASE(values[i]);
        if (NULL != values) free(values);
    }
    
    return rc;
}


int orte_gpr_replica_store_value_in_msg(orte_gpr_subscription_id_t id,
                                        orte_gpr_notify_message_t *msg,
                                        size_t cnt,
                                        orte_gpr_value_t **values)
{
    size_t j, k, n, index;
    int rc;
    orte_gpr_value_t **vals;
    orte_gpr_keyval_t **kptr;
    
    /* find the datagram corresponding to the provided subscription id */
    if (NULL == msg->data) { /* first datagram on message */
        msg->data = (orte_gpr_notify_data_t**)malloc(sizeof(orte_gpr_notify_data_t*));
        if (NULL == msg->data) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        index = 0; /* need to assign location */
        msg->cnt = 1;
    } else {
        /* check to see if this data is going to the same callback as
         * any prior data on the message. if so, then we add the values
         * to that existing data structure. if not, then we realloc to
         * establish a new data structure and store the data there
         */
        for (k=0; k < msg->cnt; k++) {
            if (msg->data[k]->id == id) { /* going to the same place */
                if (ORTE_SUCCESS != (rc =
                        orte_gpr_replica_add_value_to_datagram(
                                msg->data[k], cnt, values))) {
                    ORTE_ERROR_LOG(rc);
                }
                return rc;
            }
        }
        /* no prior matching data found, so add another data location to the message */
        msg->data = (orte_gpr_notify_data_t **) realloc(msg->data, (msg->cnt + 1)*sizeof(orte_gpr_notify_data_t*));
        if (NULL == msg->data) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        index = msg->cnt;
        (msg->cnt)++;
    }

    msg->data[index] = OBJ_NEW(orte_gpr_notify_data_t);
    if (NULL == msg->data[index]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    /* store the callback id */
    msg->data[index]->id = id;
    
    /* since this datagram is new, allocate the required data locations */
    msg->data[index]->cnt = cnt;
    if (0 == cnt) { /* no data to attach */
        return ORTE_SUCCESS;
    }
    msg->data[index]->values = (orte_gpr_value_t**)malloc(cnt * sizeof(orte_gpr_value_t*));
    if (NULL == msg->data[index]->values) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    /* transfer the values to the datagram */
    vals = msg->data[index]->values;
    for (j=0; j < cnt; j++) {
        vals[j] = OBJ_NEW(orte_gpr_value_t);
        if (NULL == vals[j]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* record the addressing mode */
        vals[j]->addr_mode = values[j]->addr_mode;
        /* record the segment these values came from */
        vals[j]->segment = strdup(values[j]->segment);
        if (NULL == (vals[j]->segment)) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* record the tokens describing the container */
        vals[j]->num_tokens = values[j]->num_tokens;
        if (0 == values[j]->num_tokens) {
            /* this is an illegal case - the tokens here describe
             * the container from which this data was obtained. The
             * container MUST have tokens that describe it
             */
            ORTE_ERROR_LOG(ORTE_ERR_GPR_DATA_CORRUPT);
            return ORTE_ERR_GPR_DATA_CORRUPT;
        }
        vals[j]->tokens = (char **)malloc(values[j]->num_tokens *
                                    sizeof(char*));
        if (NULL == vals[j]->tokens) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        for (n=0; n < values[j]->num_tokens; n++) {
            vals[j]->tokens[n] = strdup(values[j]->tokens[n]);
            if (NULL == vals[j]->tokens[n]) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
        }
        /* record the keyvals */
        vals[j]->keyvals = (orte_gpr_keyval_t**)malloc(values[j]->cnt *
                                    sizeof(orte_gpr_keyval_t*));
        if (NULL == vals[j]->keyvals) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        vals[j]->cnt = values[j]->cnt;
        kptr = vals[j]->keyvals;
        for (n=0; n < values[j]->cnt; n++) {
            kptr[n] = OBJ_NEW(orte_gpr_keyval_t);
            if (NULL == kptr[n]) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            kptr[n]->key = strdup((values[j]->keyvals[n])->key);
            if (NULL == kptr[n]->key) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            kptr[n]->type = (values[j]->keyvals[n])->type;
            if (ORTE_SUCCESS != (rc = orte_gpr_base_xfer_payload(
                        &(kptr[n]->value), &((values[j]->keyvals[n])->value),
                        (values[j]->keyvals[n])->type))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    return ORTE_SUCCESS;
}

static int orte_gpr_replica_add_value_to_datagram(orte_gpr_notify_data_t *data,
                        size_t cnt, orte_gpr_value_t **values)
{
    size_t i, j, k, n, m, index, matches, num_tokens;
    int rc;
    orte_gpr_value_t *value;
    orte_gpr_keyval_t **kptr;
    
    for (i=0; i < cnt; i++) {
        value = values[i];
        /* check to see if this value is from the same container
         * as some prior one. if so, then we add those itagvals
         * to the existing value structure. if not, then we realloc to
         * establish a new value structure and store the data there
         */
        for (k=0; k < data->cnt; k++) {
            matches = 0;
            num_tokens = data->values[k]->num_tokens;
            if (num_tokens == value->num_tokens) { /* must have same number or can't match */
                for (j=0; j < num_tokens; j++) {
                    for (m=0; m < num_tokens; m++) {
                        if (0 == strcmp((data->values[k])->tokens[j], value->tokens[m])) {
                        matches++;
                        }
                    }
                    if (num_tokens == matches) { /* from same container - just add keyvals to it */
                        index = k;
                        goto ADDKVALS;
                    }
                }
            }
        }
        /* no prior matching data found, so add another value structure to the object */
        data->values = (orte_gpr_value_t**)realloc(data->values, (data->cnt + 1)*sizeof(orte_gpr_value_t*));
        if (NULL == data->values) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        index = data->cnt;
        (data->cnt)++;

        data->values[index] = OBJ_NEW(orte_gpr_value_t);
        if (NULL == data->values[index]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
    
        /* record the addressing mode */
        data->values[index]->addr_mode = value->addr_mode;
        /* record the segment these values came from */
        data->values[index]->segment = strdup(value->segment);
        if (NULL == data->values[index]->segment) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* record the tokens describing the container */
        data->values[index]->num_tokens = value->num_tokens;
        if (0 == value->num_tokens) {
            /* this is an illegal case - the tokens here describe
             * the container from which this data was obtained. The
             * container MUST have tokens that describe it
             */
            ORTE_ERROR_LOG(ORTE_ERR_GPR_DATA_CORRUPT);
            return ORTE_ERR_GPR_DATA_CORRUPT;
        }
        data->values[index]->tokens = (char **)malloc(value->num_tokens * sizeof(char*));
        if (NULL == data->values[index]->tokens) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        for (n=0; n < value->num_tokens; n++) {
            data->values[index]->tokens[n] = strdup(value->tokens[n]);
            if (NULL == data->values[index]->tokens[n]) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
        }
        
ADDKVALS:
        /* transfer the data in the value to be returned */
        if (0 < data->values[index]->cnt) {  /* already have some data here, so add to the space */
            n = data->values[index]->cnt + value->cnt;
            data->values[index]->keyvals = (orte_gpr_keyval_t**)
                    realloc(data->values[index]->keyvals, n * sizeof(orte_gpr_keyval_t*));
            if (NULL == data->values[index]->keyvals) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            kptr = &(data->values[index]->keyvals[data->values[index]->cnt]);
            data->values[index]->cnt = n;
        } else {
            data->values[index]->keyvals = (orte_gpr_keyval_t**)malloc(value->cnt * sizeof(orte_gpr_keyval_t*));
            if (NULL == data->values[index]->keyvals) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            data->values[index]->cnt = value->cnt;
            kptr = data->values[index]->keyvals;
        }
        
        for (n=0; n < value->cnt; n++) {
            kptr[n] = OBJ_NEW(orte_gpr_keyval_t);
            if (NULL == kptr[n]) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            kptr[n]->key = strdup((value->keyvals[n])->key);
            if (NULL == kptr[n]->key) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            kptr[n]->type = (value->keyvals[n])->type;
            if (ORTE_SUCCESS != (rc = orte_gpr_base_xfer_payload(
                        &(kptr[n]->value), &((value->keyvals[n])->value),
                        (value->keyvals[n])->type))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    
    return ORTE_SUCCESS;
}

