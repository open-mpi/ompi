/* -*- C -*-
 *
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
 * The Open MPI General Purpose Registry - Replica component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "include/orte_constants.h"
#include "include/orte_schema.h"

#include "util/output.h"
#include "util/proc_info.h"

#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"

#include "mca/gpr/replica/communications/gpr_replica_comm.h"
#include "gpr_replica_fn.h"


int orte_gpr_replica_process_callbacks(void)
{
    orte_gpr_replica_callbacks_t *cb;
    orte_gpr_notify_data_t **data;
    orte_gpr_replica_subscribed_data_t **sdata;
    orte_gpr_replica_triggers_t *trig;
    bool processed;
    int i, k, rc;

    if (orte_gpr_replica_globals.debug) {
	   ompi_output(0, "gpr replica: process_callbacks entered");
    }


    while (NULL != (cb = (orte_gpr_replica_callbacks_t*)ompi_list_remove_first(&orte_gpr_replica.callbacks))) {
        /* get this request off of the local notify request tracker */
        trig = (orte_gpr_replica_triggers_t*)((orte_gpr_replica.triggers)->addr[(cb->message)->idtag]);
        if (NULL == trig) {
            ORTE_ERROR_LOG(ORTE_ERR_GPR_DATA_CORRUPT);
            goto CLEANUP;
        }
        
	    if (NULL == cb->requestor) {  /* local callback */
	        if (orte_gpr_replica_globals.debug) {
		      ompi_output(0, "process_callbacks: local");
	        }
            data = (cb->message)->data;
            sdata = (orte_gpr_replica_subscribed_data_t**)((trig->subscribed_data)->addr);
            for (i=0; i < (cb->message)->cnt; i++) {
                processed = false;
                for (k=0; k < (trig->subscribed_data)->size && !processed; k++) {
                    if (NULL != sdata[k] && sdata[k]->index == data[i]->cb_num) {
                        sdata[k]->callback(data[i], sdata[k]->user_tag);
                        processed = true;
                    }
                }
            } 
            cb->message->data = NULL;
    	   } else {  /* remote request - send message back */
    	       if (orte_gpr_replica_globals.debug) {
    		      ompi_output(0, "process_callbacks: remote to [%d,%d,%d]",
                        ORTE_NAME_ARGS(cb->requestor));
    	       }
    	       orte_gpr_replica_remote_notify(cb->requestor, cb->remote_idtag, cb->message);
        }
CLEANUP:
        /* if one_shot, remove trigger action */
        if (ORTE_GPR_TRIG_ONE_SHOT & trig->action) {
            if (ORTE_SUCCESS != (rc = orte_pointer_array_set_item(orte_gpr_replica.triggers,
                                                trig->index, NULL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            OBJ_RELEASE(trig);
        }
        
    	    OBJ_RELEASE(cb);
    }

    return ORTE_SUCCESS;
}



int orte_gpr_replica_register_callback(orte_gpr_replica_triggers_t *trig)
{
    orte_gpr_replica_callbacks_t *cb;
    int rc;
    
    /* see if a callback has already been requested for this requestor */
    for (cb = (orte_gpr_replica_callbacks_t*)ompi_list_get_first(&(orte_gpr_replica.callbacks));
         cb != (orte_gpr_replica_callbacks_t*)ompi_list_get_end(&(orte_gpr_replica.callbacks));
         cb = (orte_gpr_replica_callbacks_t*)ompi_list_get_next(cb)) {
         if (trig->requestor == cb->requestor) { /* same destination - add to existing callback */
             if (ORTE_SUCCESS != (rc = orte_gpr_replica_construct_notify_message(&(cb->message), trig))) {
                ORTE_ERROR_LOG(rc);
                return rc;
             }
         }
    }
    /* got a new callback, generate the request */
    cb = OBJ_NEW(orte_gpr_replica_callbacks_t);
    if (NULL == cb) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* queue the callback */
    if (NULL == trig->requestor) {  /* local request - queue local callback */
        cb->requestor = NULL;
        cb->remote_idtag = ORTE_GPR_NOTIFY_ID_MAX;
        if (orte_gpr_replica_globals.debug) {
           ompi_output(0, "[%d,%d,%d] process_trig: queueing local message\n",
                        ORTE_NAME_ARGS(orte_process_info.my_name));
        }
  
    } else {  /* remote request - queue remote callback */
        if (ORTE_SUCCESS != (rc = orte_ns.copy_process_name(&(cb->requestor), trig->requestor))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(cb);
            return rc;
        }
        cb->remote_idtag = trig->remote_idtag;
        if (orte_gpr_replica_globals.debug) {
            ompi_output(0, "[%d,%d,%d] process_trig: queueing message for [%d,%d,%d] with idtag %d using remoteid %d\n",
                   ORTE_NAME_ARGS(orte_process_info.my_name), ORTE_NAME_ARGS(cb->requestor),
                    (int)cb->remote_idtag, (int)trig->remote_idtag);
        }
    }
    ompi_list_append(&orte_gpr_replica.callbacks, &cb->item);
    
    /* construct the message */
    cb->message = OBJ_NEW(orte_gpr_notify_message_t);
    if (NULL == cb->message) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    cb->message->idtag = trig->index;

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_construct_notify_message(&(cb->message), trig))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cb);
        return rc;
    }
    
    if (orte_gpr_replica_globals.debug) {
        ompi_output(0, "[%d,%d,%d] gpr replica-process_trig: complete",
            ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    return ORTE_SUCCESS;
}


int orte_gpr_replica_construct_notify_message(orte_gpr_notify_message_t **msg,
                                              orte_gpr_replica_triggers_t *trig)
{
    int rc=ORTE_SUCCESS;
    orte_gpr_notify_data_t **data;
    orte_gpr_replica_subscribed_data_t **sptr;
    int i, k;
    
    /* if we don't have data, just return */
    if (0 >= trig->num_subscribed_data) {
        return ORTE_SUCCESS;
    }
    
    sptr = (orte_gpr_replica_subscribed_data_t**)((trig->subscribed_data)->addr);
    for (i=0; i < (trig->subscribed_data)->size; i++) {
        if (NULL != sptr[i]) {
            if (NULL == (*msg)->data) { /* first data item on the message */
                (*msg)->data = (orte_gpr_notify_data_t**)malloc(sizeof(orte_gpr_notify_data_t*));
                if (NULL == (*msg)->data) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                data = &((*msg)->data[0]); /* need to assign location */
                (*msg)->cnt = 1;
            } else {
                /* check to see if this data is going to the same callback as
                 * any prior data on the message. if so, then we add those keyvals
                 * to the existing data structure. if not, then we realloc to
                 * establish a new data structure and store the data there
                 */
                for (k=0; k < (*msg)->cnt; k++) {
                    if ((*msg)->data[k]->cb_num == sptr[i]->index) { /* going to the same place */
                        data = &((*msg)->data[k]);
                        goto MOVEON;
                    }
                }
                /* no prior matching data found, so add another data location to the message */
                (*msg)->data = realloc((*msg)->data, ((*msg)->cnt + 1)*sizeof(orte_gpr_notify_data_t*));
                if (NULL == (*msg)->data) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                data = &((*msg)->data[(*msg)->cnt]);
                ((*msg)->cnt)++;
            }

            *data = OBJ_NEW(orte_gpr_notify_data_t);
            if (NULL == *data) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            /* for each data object, store the callback_number, addressing mode, and name
             * of the segment this data came from
             */
            (*data)->cb_num = sptr[i]->index;
            (*data)->addr_mode = sptr[i]->addr_mode;
            (*data)->segment = strdup((sptr[i]->seg)->name);
            if (NULL == (*data)->segment) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
MOVEON:
            /* add the values to the data object */
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_add_values(data, sptr[i]))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }  /* if sptr not NULL */
    }  /* for i */

    return ORTE_SUCCESS;
}


int orte_gpr_replica_add_values(orte_gpr_notify_data_t **data,
                                orte_gpr_replica_subscribed_data_t *sptr)
{
    int i, rc, j, k, n, m, matches, num_tokens, num_keys, cnt;
    orte_gpr_value_t **values, **data_values;
    orte_gpr_keyval_t **kptr;
    
    /* get the data off the registry */
    num_tokens = (int)orte_value_array_get_size(&(sptr->tokentags));
    num_keys = (int) orte_value_array_get_size(&(sptr->keytags));

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_fn(sptr->addr_mode, sptr->seg,
            ORTE_VALUE_ARRAY_GET_BASE(&(sptr->tokentags), orte_gpr_replica_itag_t),
            num_tokens,
            ORTE_VALUE_ARRAY_GET_BASE(&(sptr->keytags), orte_gpr_replica_itag_t),
            num_keys,
            &cnt, &values))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* store these values in the notify_data structure, combining data
     * where containers match
     */
   for (i=0; i < cnt; i++) {
       if (NULL == (*data)->values) { /* first value on the structure */
            (*data)->values = (orte_gpr_value_t**)malloc(sizeof(orte_gpr_value_t*));
            if (NULL == (*data)->values) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            data_values = &((*data)->values[0]); /* need to assign location */
            (*data)->cnt = 1;
        } else {
            /* check to see if this value is from the same container
             * as some prior one. if so, then we add those itagvals
             * to the existing value structure. if not, then we realloc to
             * establish a new value structure and store the data there
             */
            for (k=0; k < (*data)->cnt; k++) {
                matches = 0;
                num_tokens = (*data)->values[k]->num_tokens;
                if (num_tokens == values[i]->num_tokens) { /* must have same number or can't match */
                    for (j=0; j < num_tokens; j++) {
                        for (m=0; m < num_tokens; m++) {
                            if (0 == strcmp(((*data)->values[k])->tokens[j], values[i]->tokens[m])) {
                            matches++;
                            }
                        }
                        if (num_tokens == matches) { /* from same container - just add keyvals to it */
                            data_values = &((*data)->values[k]);
                            goto MOVEON;
                        }
                    }
                }
            }
            /* no prior matching data found, so add another value location to the object */
            (*data)->values = (orte_gpr_value_t**)realloc((*data)->values, ((*data)->cnt + 1)*sizeof(orte_gpr_value_t*));
            if (NULL == (*data)->values) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            data_values = &((*data)->values[(*data)->cnt]);
            ((*data)->cnt)++;
        }
    
        *data_values = OBJ_NEW(orte_gpr_value_t);
        if (NULL == *data_values) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
    
        /* record the addressing mode */
        (*data_values)->addr_mode = sptr->addr_mode;
        /* record the segment these values came from */
        (*data_values)->segment = strdup((sptr->seg)->name);
        if (NULL == ((*data_values)->segment)) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* record the tokens describing the container */
        (*data_values)->num_tokens = values[i]->num_tokens;
        (*data_values)->tokens = (char **)malloc(values[i]->num_tokens * sizeof(char*));
        if (NULL == (*data_values)->tokens) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        for (n=0; n < values[i]->num_tokens; n++) {
            (*data_values)->tokens[n] = strdup(values[i]->tokens[n]);
            if (NULL == (*data_values)->tokens[n]) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
        }
MOVEON:
        /* record the values to be returned */
        if (0 < (*data_values)->cnt) {  /* already have some data here, so add to the space */
            n = (*data_values)->cnt + values[i]->cnt;
            (*data_values)->keyvals = (orte_gpr_keyval_t**)
                    realloc((*data_values)->keyvals, n * sizeof(orte_gpr_keyval_t*));
            if (NULL == (*data_values)->keyvals) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            (*data_values)->cnt = n;
            kptr = &((*data_values)->keyvals[n-cnt-1]);
        } else {
            (*data_values)->keyvals = (orte_gpr_keyval_t**)malloc(values[i]->cnt * sizeof(orte_gpr_keyval_t*));
            if (NULL == (*data_values)->keyvals) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            (*data_values)->cnt = values[i]->cnt;
            kptr = (*data_values)->keyvals;
        }
        
        for (n=0; n < values[i]->cnt; n++) {
            kptr[n] = OBJ_NEW(orte_gpr_keyval_t);
            if (NULL == kptr[n]) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            kptr[n]->key = strdup((values[i]->keyvals[n])->key);
            if (NULL == kptr[n]->key) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            kptr[n]->type = (values[i]->keyvals[n])->type;
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_xfer_payload(
                        &(kptr[n]->value), &((values[i]->keyvals[n])->value),
                        (values[i]->keyvals[n])->type))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }  /* for i */
    
    return ORTE_SUCCESS;
}
                    
