/*
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
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "util/output.h"
#include "util/proc_info.h"
#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"

#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"
#include "gpr_replica_fn.h"

int orte_gpr_replica_subscribe_fn(orte_gpr_notify_action_t action, int num_subs,
                                  orte_gpr_subscription_t **subscriptions,
                                  int num_trigs,
                                  orte_gpr_value_t **trigs,
                                  orte_gpr_notify_id_t idtag)
{
    orte_gpr_replica_triggers_t *trig=NULL;
    orte_gpr_replica_subscribed_data_t *data=NULL, **data2=NULL;
    orte_gpr_replica_counter_t *cntr;
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_container_t **cptr=NULL, *cptr2=NULL;
    orte_gpr_replica_itag_t itag, *tokentags=NULL, *keytags=NULL;
    orte_gpr_replica_itagval_t *iptr=NULL;
    orte_gpr_replica_addr_mode_t tok_mode, key_mode;
    int i, j, k, rc, num_tokens, num_keys, num_found;
    bool found;

    if (orte_gpr_replica_globals.debug) {
	   ompi_output(0, "[%d,%d,%d] gpr replica: subscribe entered",
		    ORTE_NAME_ARGS(orte_process_info.my_name));
        ompi_output(0, "Received %d subscriptions", num_subs);
        for (i=0; i < num_subs; i++) {
            ompi_output(0, "Subscription %d on segment %s with %d tokens, %d keys",
                    i, subscriptions[i]->segment, subscriptions[i]->num_tokens,
                    subscriptions[i]->num_keys);
            for (j=0; j < subscriptions[i]->num_tokens; j++) {
                ompi_output(0, "\tToken num: %d\tToken: %s", j, subscriptions[i]->tokens[j]);
            }
            for (j=0; j < subscriptions[i]->num_keys; j++) {
                ompi_output(0, "\tKey num: %d\tKey: %s", j, subscriptions[i]->keys[j]);
            }
        }
    }

    trig = (orte_gpr_replica_triggers_t*)((orte_gpr_replica.triggers)->addr[idtag]);
    if (NULL == trig) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    trig->action = action;

    for (i=0; i < num_subs; i++) {
        /* find the subscribed_data entry in the trigger pointer array - placed
         * there initially by the enter_notify_request function so we could store
         * the callback function and user_tag pointers
         */
        data2 = (orte_gpr_replica_subscribed_data_t**)((trig->subscribed_data)->addr);
        for (j=0, data=NULL; j < (trig->subscribed_data)->size && NULL == data; j++) {
            if (NULL != data2[j] && i == data2[j]->index) {
                data = data2[j];
            }
        }
        if (NULL == data) { /* if not found, then something very wrong */
            ORTE_ERROR_LOG(ORTE_ERR_GPR_DATA_CORRUPT);
            return ORTE_ERR_GPR_DATA_CORRUPT;
        }
        
        /* find and store the segment */
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&(data->seg), true,
                                                    subscriptions[i]->segment))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        tok_mode = 0x004f & subscriptions[i]->addr_mode;
        if (0x00 == tok_mode) {  /* default token address mode to AND */
            tok_mode = ORTE_GPR_REPLICA_AND;
        }
        key_mode = ((0x4f00 & subscriptions[i]->addr_mode) >> 8) & 0x004f;
        if (0x00 == key_mode) {  /* default key address mode to OR */
            key_mode = ORTE_GPR_REPLICA_OR;
        }
        data->addr_mode = ((orte_gpr_addr_mode_t)(key_mode) << 8) | (orte_gpr_addr_mode_t)tok_mode;
        
        if (NULL != subscriptions[i]->tokens && 0 < subscriptions[i]->num_tokens) {
            num_tokens = subscriptions[i]->num_tokens; /* indicates non-NULL terminated list */
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&tokentags, data->seg,
                                    subscriptions[i]->tokens, &num_tokens))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            
            }
            if (ORTE_SUCCESS != (rc = orte_value_array_set_size(&(data->tokentags), (size_t)num_tokens))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            for (j=0; j < num_tokens; j++) {
                ORTE_VALUE_ARRAY_SET_ITEM(&(data->tokentags), orte_gpr_replica_itag_t,
                                                j, tokentags[j]);
            }
            free(tokentags);
            tokentags = NULL;
        }
        
        if (NULL != subscriptions[i]->keys && 0 < subscriptions[i]->num_keys) {
            num_keys = subscriptions[i]->num_keys; /* indicates non-NULL terminated list */
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&keytags, data->seg,
                                    subscriptions[i]->keys, &num_keys))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            
            }
            if (ORTE_SUCCESS != (rc = orte_value_array_set_size(&(data->keytags), (size_t)num_keys))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            for (j=0; j < num_keys; j++) {
                ORTE_VALUE_ARRAY_SET_ITEM(&(data->keytags), orte_gpr_replica_itag_t,
                                                j, keytags[j]);
            }
            free(keytags);
            keytags = NULL;
        }
    }
    
    /* if this has a trigger in it, need to setup the counters */
    if (ORTE_GPR_TRIG_ANY & action) {
        trig->num_counters = 0;
        for (j=0; j < num_trigs; j++) {
            /* get this counter's addressing modes */
            tok_mode = 0x004f & trigs[j]->addr_mode;
            if (0x00 == tok_mode) {  /* default token address mode to AND */
                tok_mode = ORTE_GPR_REPLICA_AND;
            }
            key_mode = ((0x4f00 & trigs[j]->addr_mode) >> 8) & 0x004f;
            if (0x00 == key_mode) {  /* default key address mode to OR */
                key_mode = ORTE_GPR_REPLICA_OR;
            }
        
            /* locate this counter's segment - this is where the counter will be */
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, trigs[j]->segment))) {
                ORTE_ERROR_LOG(rc);
                OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
                return rc;
            }
        
            /* convert the counter's tokens to an itaglist */
            if (NULL != trigs[j]->tokens && 0 < trigs[j]->num_tokens) {
                num_tokens = trigs[j]->num_tokens; /* indicates non-NULL terminated list */
                if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&tokentags, seg,
                                    trigs[j]->tokens, &num_tokens))) {
                    ORTE_ERROR_LOG(rc);
                    goto CLEANUP;
                }
            }
        
            /* find the specified container(s) */
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_containers(&num_found, seg, tok_mode,
                                            tokentags, num_tokens))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
        
            if (0 == num_found) {  /* no existing container found - create one using all the tokens */
                if (ORTE_SUCCESS != (rc = orte_gpr_replica_create_container(&cptr2, seg,
                                                    num_tokens, tokentags))) {
                    ORTE_ERROR_LOG(rc);
                    goto CLEANUP;
                }
         
                /* ok, store all of this counters values in the new container, adding a pointer to each
                 * one in the trigger's counter array
                 */
                for (i=0; i < trigs[j]->cnt; i++) {
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_add_keyval(&iptr, seg, cptr2, trigs[j]->keyvals[i]))) {
                        ORTE_ERROR_LOG(rc);
                        goto CLEANUP;
                    }
                    cntr = OBJ_NEW(orte_gpr_replica_counter_t);
                    if (NULL == cntr) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                    cntr->seg = seg;
                    cntr->cptr = cptr2;
                    cntr->iptr = iptr;
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_value((void*)(&(cntr->trigger_level)), iptr))) {
                        ORTE_ERROR_LOG(rc);
                        goto CLEANUP;
                    }
                    if (0 > orte_pointer_array_add(trig->counters, cntr)) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        rc = ORTE_ERR_OUT_OF_RESOURCE;
                        goto CLEANUP;
                    }
                }
                trig->num_counters += trigs[j]->cnt;
            } else {  /* For each counter, go through the list of containers and
                         see if it already exists in container. Only allow each
                         counter to be identified once - error if either a counter is never
                         found or already existing in more than one place. */
                cptr = (orte_gpr_replica_container_t**)(orte_gpr_replica_globals.srch_cptr)->addr;
                for (i=0; i < trigs[j]->cnt; i++) {
                    found = false;
                    for (k=0; k < (orte_gpr_replica_globals.srch_cptr)->size; k++) {
                        if (NULL != cptr[k]) {
                            if (ORTE_SUCCESS == orte_gpr_replica_dict_lookup(&itag, seg, trigs[j]->keyvals[i]->key) &&
                                ORTE_SUCCESS == orte_gpr_replica_search_container(&num_found,
                                                        ORTE_GPR_REPLICA_OR,
                                                        &itag, 1, cptr[k]) &&
                                0 < num_found) {
                                /* this key already exists - make sure it's unique
                                 */
                                if (1 < num_found || found) { /* not unique - error out */
                                    ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                                    rc = ORTE_ERR_BAD_PARAM;
                                    goto CLEANUP;
                                }
                                /* okay, add to trigger's counter array */
                                found = true;
                                iptr = (orte_gpr_replica_itagval_t*)((orte_gpr_replica_globals.srch_ival)->addr[0]);
                                cntr = OBJ_NEW(orte_gpr_replica_counter_t);
                                if (NULL == cntr) {
                                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                                    return ORTE_ERR_OUT_OF_RESOURCE;
                                }
                                cntr->seg = seg;
                                cntr->cptr = cptr[k];
                                cntr->iptr = iptr;
                                if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_value((void*)(&(cntr->trigger_level)), iptr))) {
                                    ORTE_ERROR_LOG(rc);
                                    goto CLEANUP;
                                }
                                if (0 > orte_pointer_array_add(trig->counters, cntr)) {
                                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                                    rc = ORTE_ERR_OUT_OF_RESOURCE;
                                    goto CLEANUP;
                                }
                                (trig->num_counters)++;
                            }  /* end if found */
                        }  /* end if cptr NULL */
                    }  /* end for j */
                    if (!found) {  /* specified counter never found - error */
                        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                        rc = ORTE_ERR_BAD_PARAM;
                        goto CLEANUP;
                    } /* end if found */
                }  /* end for i */
            }  /* end if/else container found */
            
            /* check the triggers on this segment before leaving to see if they are already fired */
            if (ORTE_SUCCESS != 
                (rc = orte_gpr_replica_check_subscriptions(seg, ORTE_GPR_REPLICA_NO_ACTION))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }  /* end for j */
    }  /* end if trigger */
    
CLEANUP:
    if (NULL != tokentags) {
        free(tokentags);
    }
    
    if (ORTE_SUCCESS != rc) OBJ_RELEASE(trig);
    
    return rc;
}


int orte_gpr_replica_unsubscribe_fn(orte_gpr_notify_id_t sub_number)
{
    orte_gpr_replica_triggers_t *trig;

    if (orte_gpr_replica_globals.debug) {
	   ompi_output(0, "[%d,%d,%d] gpr replica: unsubscribe entered for sub number %d",
		    ORTE_NAME_ARGS(orte_process_info.my_name), sub_number);
    }

    /* release trigger on replica and remove it */
    trig = (orte_gpr_replica_triggers_t*)((orte_gpr_replica.triggers)->addr[sub_number]);
    if (NULL == trig) {
        return ORTE_ERR_BAD_PARAM;
    }
    OBJ_RELEASE(trig);
    
    orte_pointer_array_set_item(orte_gpr_replica.triggers, sub_number, NULL);
    return ORTE_SUCCESS;
}
