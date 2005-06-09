/*
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
 * The Open MPI general purpose registry - support functions.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "mca/errmgr/errmgr.h"
#include "mca/ns/ns.h"
#include "util/output.h"
#include "mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"
#include "gpr_replica_fn.h"


int
orte_gpr_replica_enter_notify_request(orte_gpr_notify_id_t *local_idtag,
                                      orte_process_name_t *requestor,
                                      orte_gpr_notify_id_t remote_idtag,
                                      size_t cnt, orte_gpr_subscription_t **subscriptions)
{
    orte_gpr_replica_triggers_t *trig;
    int rc;
    size_t i, index;
    orte_gpr_replica_subscribed_data_t *data;
    
    *local_idtag = ORTE_GPR_NOTIFY_ID_MAX;
    
    trig = OBJ_NEW(orte_gpr_replica_triggers_t);
    if (NULL == trig) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (NULL != requestor) {
        if (ORTE_SUCCESS != (rc = orte_ns.copy_process_name(&(trig->requestor),
                                            requestor))) {
              ORTE_ERROR_LOG(rc);
              return rc;
        }
    } else {
         trig->requestor = NULL;
    }

    trig->remote_idtag = remote_idtag;

    for (i=0; i < cnt; i++) {
        data = OBJ_NEW(orte_gpr_replica_subscribed_data_t);
        if (NULL == data) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* store the callback function and user_tag pointers */
        data->callback = subscriptions[i]->cbfunc;
        data->user_tag = subscriptions[i]->user_tag;
        /* add the object to the trigger's subscribed_data pointer array */
        if (0 > (rc = orte_pointer_array_add(&index, trig->subscribed_data, data))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        data->index = i;
    }
    trig->num_subscribed_data = cnt;
    
    if (0 > (rc = orte_pointer_array_add(&(trig->index), orte_gpr_replica.triggers, trig))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    *local_idtag = (orte_gpr_notify_id_t)trig->index;

    return ORTE_SUCCESS;
}


int
orte_gpr_replica_remove_notify_request(orte_gpr_notify_id_t local_idtag,
                                       orte_gpr_notify_id_t *remote_idtag)
{
    orte_gpr_replica_triggers_t *trig;

    trig = (orte_gpr_replica_triggers_t*)((orte_gpr_replica.triggers)->addr[local_idtag]);
    if (NULL == trig) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    *remote_idtag = trig->remote_idtag;
    OBJ_RELEASE(trig);
    orte_pointer_array_set_item(orte_gpr_replica.triggers, local_idtag, NULL);

    return ORTE_SUCCESS;
}


int orte_gpr_replica_record_action(orte_gpr_replica_segment_t *seg,
                                   orte_gpr_replica_container_t *cptr,
                                   orte_gpr_replica_itagval_t *iptr,
                                   orte_gpr_replica_action_t action)
{
    orte_gpr_replica_action_taken_t *new;
    size_t index;
    int rc;
    
    new = OBJ_NEW(orte_gpr_replica_action_taken_t);
    if (NULL == new) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    new->action = action;
    
    /* store pointers to the affected itagval */
    new->seg = seg;
    new->cptr = cptr;
    new->iptr = iptr;
    
    /* "retain" ALL of the respective objects so they can't disappear until
     * after we process the actions
     */
    OBJ_RETAIN(seg);
    OBJ_RETAIN(cptr);
    OBJ_RETAIN(iptr);
    
    /* add the new action record to the array */
    if (0 > (rc = orte_pointer_array_add(&index, orte_gpr_replica_globals.acted_upon, new))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    return ORTE_SUCCESS;
}


int orte_gpr_replica_update_storage_locations(orte_gpr_replica_itagval_t *new_iptr)
{
    orte_gpr_replica_triggers_t **trig;
    orte_gpr_replica_counter_t **cntrs;
    orte_gpr_replica_itagval_t **old_iptrs;
    size_t i, j, k;
    bool replaced;
    
    trig = (orte_gpr_replica_triggers_t**)((orte_gpr_replica.triggers)->addr);
    for (i=0; i < (orte_gpr_replica.triggers)->size; i++) {
        if (NULL != trig[i]) {
            cntrs = (orte_gpr_replica_counter_t**)((trig[i]->counters)->addr);
            for (j=0; j < (trig[i]->counters)->size; j++) {
                if (NULL != cntrs[j]) {
                    old_iptrs = (orte_gpr_replica_itagval_t**)((orte_gpr_replica_globals.srch_ival)->addr);
                    for (k=0; k < (orte_gpr_replica_globals.srch_ival)->size; k++) {
                        replaced = false;
                        if (NULL != old_iptrs[k] && old_iptrs[k] == cntrs[j]->iptr) {
                            if (NULL == new_iptr || replaced) {
                                orte_pointer_array_set_item(trig[i]->counters, j, NULL);
                                (trig[i]->num_counters)--;
                            } else if (!replaced) {
                                cntrs[j]->iptr = new_iptr;
                                replaced = true;
                            }
                        }
                    }
                }
            }
        }
    }
    return ORTE_SUCCESS;
}


int orte_gpr_replica_check_subscriptions(orte_gpr_replica_segment_t *seg)
{
    orte_gpr_replica_triggers_t **trig;
    orte_gpr_replica_subscribed_data_t **sub;
    size_t i, j;
    int rc;

    trig = (orte_gpr_replica_triggers_t**)((orte_gpr_replica.triggers)->addr);
    for (i=0; i < (orte_gpr_replica.triggers)->size; i++) {
        if (NULL != trig[i]) {
            /* check if trigger is on this subscription - if so, check it */
            if (ORTE_GPR_TRIG_ANY & trig[i]->action) {
                if (ORTE_SUCCESS != (rc = orte_gpr_replica_check_trig(trig[i]))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
            /* check if notifier is on this subscription - if so, check to see
             * if it has fired, but ONLY if NOTIFY_START is NOT set
             */
            if ((ORTE_GPR_NOTIFY_ANY & trig[i]->action) &
                (ORTE_GPR_TRIG_NOTIFY_START & trig[i]->action)) {
                /* for notifier subscriptions, the data structures
                 * in the trigger define the data being monitored. First,
                 * check to see if the segment that was modified matches
                 * any of the data being monitored. If so, then we call the
                 * check_notify function to see if we should fire
                 */
                 sub = (orte_gpr_replica_subscribed_data_t**)
                        (trig[i]->subscribed_data)->addr;
                 for (j=0; j < (trig[i]->subscribed_data)->size; j++) {
                    if ((NULL != sub[j]) && (seg == sub[j]->seg)) {
                        if (ORTE_SUCCESS != (rc = orte_gpr_replica_check_notify(trig[i], sub[j]))) {
                            ORTE_ERROR_LOG(rc);
                            return rc;
                        }
                    }
                 }
            } /* if notify */
        }  /* if trig not NULL */
    }
    return ORTE_SUCCESS;
}


int orte_gpr_replica_check_trig(orte_gpr_replica_triggers_t *trig)
{
    orte_gpr_replica_counter_t **cntr;
    orte_gpr_replica_itagval_t *base_value=NULL;
    bool first, fire;
    size_t i;
    int cmp;
    int rc;
    
    if (ORTE_GPR_TRIG_CMP_LEVELS & trig->action) { /* compare the levels of the counters */
        cntr = (orte_gpr_replica_counter_t**)((trig->counters)->addr);
        first = true;
        fire = true;
        for (i=0; i < (trig->counters)->size && fire; i++) {
            if (NULL != cntr[i]) {
                if (first) {
                    base_value = cntr[i]->iptr;
                    first = false;
                } else {
                   if (ORTE_SUCCESS != (rc =
                            orte_gpr_replica_compare_values(&cmp, base_value,
                                                            cntr[i]->iptr))) {
                        ORTE_ERROR_LOG(rc);
                        return rc;
                   }
                   if (0 != cmp) {
                        fire = false;
                   }
                }
            }
        }
        if (fire) { /* all levels were equal */
            if (orte_gpr_replica_globals.debug) {
                ompi_output(0, "REGISTERING CALLBACK FOR TRIG %d", trig->index);
            }
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_register_callback(trig))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            goto FIRED;
        }
        return ORTE_SUCCESS;\
        
    } else if (ORTE_GPR_TRIG_AT_LEVEL & trig->action) { /* see if counters are at a level */
        cntr = (orte_gpr_replica_counter_t**)((trig->counters)->addr);
        fire = true;
        for (i=0; i < (trig->counters)->size && fire; i++) {
            if (NULL != cntr[i]) {
                if (ORTE_SUCCESS != (rc =
                            orte_gpr_replica_compare_values(&cmp, cntr[i]->iptr,
                                                            &(cntr[i]->trigger_level)))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
               }
               if (0 != cmp) {
                    fire = false;
               }
            }
        }
        if (fire) { /* all counters at specified trigger level */
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_register_callback(trig))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            goto FIRED;
        }
        return ORTE_SUCCESS;
    }
   
    return ORTE_SUCCESS;  /* neither cmp nor at level set */

FIRED:
    /* if notify_at_start set, unset it to indicate that trigger fired */
    if (ORTE_GPR_TRIG_NOTIFY_START & trig->action) {
        trig->action = trig->action & ~ORTE_GPR_TRIG_NOTIFY_START;
    }

    /* if one-shot, set flag to indicate it has fired so it can be cleaned
     * up later
     */
    if (ORTE_GPR_TRIG_ONE_SHOT & trig->action) {
        trig->one_shot_fired = true;
    }
    
    return ORTE_SUCCESS;
}

/*
 * When entering this function, we know two things: (a) something was modified
 * on the segment specified in the subscription, and (b) notifications are
 * active. What we now need to determine is whether or not any of the data
 * objects pointed to by the subscription were involved in the change. These
 * objects could just be a container - e.g., the subscriber might want to know
 * if anything gets added to a container - or could be a container plus one or
 * more keys when the subscriber wants to know if a specific value gets changed.
 */
int orte_gpr_replica_check_notify(orte_gpr_replica_triggers_t *trig,
                                  orte_gpr_replica_subscribed_data_t *sub)
{
    orte_gpr_replica_action_taken_t **ptr;
    size_t i;
    
    ptr = (orte_gpr_replica_action_taken_t**)((orte_gpr_replica_globals.acted_upon)->addr);
    for (i=0; i < (orte_gpr_replica_globals.acted_upon)->size; i++) {
        if (NULL != ptr[i]) {
            if ((trig->action & ORTE_GPR_NOTIFY_ADD_ENTRY) &&
                (ptr[i]->action & ORTE_GPR_REPLICA_ENTRY_ADDED)) {
                /* send back the added entry */
            } else if ((trig->action & ORTE_GPR_NOTIFY_DEL_ENTRY) &
                (ptr[i]->action & ORTE_GPR_REPLICA_ENTRY_DELETED)){
                /* send back the deleted entry */
            } else if ((trig->action & ORTE_GPR_NOTIFY_VALUE_CHG) &&
                (ptr[i]->action & ORTE_GPR_REPLICA_ENTRY_CHANGED)) {
                /* see if the acted_upon data was the target of the subscription */
                /* send back the new data */
            } else if ((trig->action & ORTE_GPR_NOTIFY_VALUE_CHG) &&
                (ptr[i]->action & ORTE_GPR_REPLICA_ENTRY_CHG_TO)) {
                /* ptr contains the "new" data - check to see if it matches
                 * the subscription. if so, send back the new data
                 */
            } else if ((trig->action & ORTE_GPR_NOTIFY_VALUE_CHG) &&
                (ptr[i]->action & ORTE_GPR_REPLICA_ENTRY_CHG_FRM)) {
                /* ptr contains the "old" data - check to see if it matches
                 * the subscription. if so, send back the new data
                 */
            }
        }
    }
    return ORTE_SUCCESS;
}


int orte_gpr_replica_purge_subscriptions(orte_process_name_t *proc)
{
    orte_gpr_replica_triggers_t **trig;
    size_t i;
    int rc;
    
    /* locate any notification events that have proc as the recipient
     */
    trig = (orte_gpr_replica_triggers_t**)((orte_gpr_replica.triggers)->addr);
    for (i=0; i < (orte_gpr_replica.triggers)->size; i++) {
        if (NULL != trig[i]) {
            if (NULL == proc && NULL == trig[i]->requestor) {
                if (ORTE_SUCCESS != (rc = orte_pointer_array_set_item(orte_gpr_replica.triggers,
                                                trig[i]->index, NULL))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                OBJ_RELEASE(trig);
            } else if (NULL != proc && NULL != trig[i]->requestor &&
                       0 == orte_ns.compare(ORTE_NS_CMP_ALL, proc, trig[i]->requestor)) {
                if (ORTE_SUCCESS != (rc = orte_pointer_array_set_item(orte_gpr_replica.triggers,
                                                trig[i]->index, NULL))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                OBJ_RELEASE(trig);
            }
        }
    }
    return ORTE_SUCCESS;
}
