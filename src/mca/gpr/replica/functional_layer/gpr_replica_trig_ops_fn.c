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
 * The Open MPI general purpose registry - support functions.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "mca/errmgr/errmgr.h"
#include "mca/ns/ns.h"

#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"
#include "gpr_replica_fn.h"


int
orte_gpr_replica_enter_notify_request(orte_gpr_notify_id_t *local_idtag,
                                      orte_process_name_t *requestor,
                                      orte_gpr_notify_id_t remote_idtag,
                                      int cnt, orte_gpr_subscription_t **subscriptions)
{
    orte_gpr_replica_triggers_t *trig;
    int rc, i;
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
        if (0 > (rc = orte_pointer_array_add(trig->subscribed_data, data))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        data->index = i;
    }
        
    if (0 > (rc = orte_pointer_array_add(orte_gpr_replica.triggers, trig))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    trig->index = rc;
    *local_idtag = (orte_gpr_notify_id_t)rc;

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


int orte_gpr_replica_update_storage_locations(orte_gpr_replica_itagval_t *new_iptr)
{
    orte_gpr_replica_triggers_t **trig;
    orte_gpr_replica_counter_t **cntrs;
    orte_gpr_replica_itagval_t **old_iptrs;
    int i, j, k;
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


int orte_gpr_replica_check_subscriptions(orte_gpr_replica_segment_t *seg,
                                         orte_gpr_replica_action_t action_taken)
{
    orte_gpr_replica_triggers_t **trig;
    orte_gpr_replica_subscribed_data_t **sptr;
    int i, j, n, rc;

    trig = (orte_gpr_replica_triggers_t**)((orte_gpr_replica.triggers)->addr);
    for (i=0; i < (orte_gpr_replica.triggers)->size; i++) {
        if (NULL != trig[i]) {
            sptr = (orte_gpr_replica_subscribed_data_t**)((trig[i]->subscribed_data)->addr);
            n = (trig[i]->subscribed_data)->size;
            for (j=0; j < n; j++) {
                if (NULL != sptr[j] && seg == sptr[j]->seg) {
#if 0
                    if (ORTE_GPR_NOTIFY_ANY & trig[i]->action &&
                        !(ORTE_GPR_TRIG_NOTIFY_START & trig[i]->action)) { /* notify exists and is active */
                        if (((ORTE_GPR_NOTIFY_ADD_ENTRY & trig[i]->action) && (ORTE_GPR_REPLICA_ENTRY_ADDED == action_taken)) ||
                            ((ORTE_GPR_NOTIFY_DEL_ENTRY & trig[i]->action) && (ORTE_GPR_REPLICA_ENTRY_DELETED == action_taken)) ||
                            ((ORTE_GPR_NOTIFY_VALUE_CHG & trig[i]->action) && (ORTE_GPR_REPLICA_ENTRY_CHANGED == action_taken)) ||
                            ((ORTE_GPR_NOTIFY_VALUE_CHG_TO & trig[i]->action) && (ORTE_GPR_REPLICA_ENTRY_CHG_TO == action_taken)) ||
                            ((ORTE_GPR_NOTIFY_VALUE_CHG_FRM & trig[i]->action) && (ORTE_GPR_REPLICA_ENTRY_CHG_FRM == action_taken))) {
                            if (ORTE_SUCCESS != (rc = orte_gpr_replica_register_callback(trig[i]))) {
                                ORTE_ERROR_LOG(rc);
                                return rc;
                            }
                        }
                    }
#endif                
                    /* check if trigger is on this subscription - if so, check it */
                    if (ORTE_GPR_TRIG_ANY & trig[i]->action) {
                        if (ORTE_SUCCESS != (rc = orte_gpr_replica_check_trig(trig[i]))) {
                            ORTE_ERROR_LOG(rc);
                            return rc;
                        }
                    }
                } /* if sptr not NULL */
            }  /* for j */
        }  /* if trig not NULL */
    }
    return ORTE_SUCCESS;
}


int orte_gpr_replica_check_trig(orte_gpr_replica_triggers_t *trig)
{
    orte_gpr_replica_counter_t **cntr;
    bool first, fire;
    int i, rc, level, level2;
    
    if (ORTE_GPR_TRIG_CMP_LEVELS & trig->action) { /* compare the levels of the counters */
        cntr = (orte_gpr_replica_counter_t**)((trig->counters)->addr);
        first = true;
        fire = true;
        for (i=0; i < (trig->counters)->size && fire; i++) {
            if (NULL != cntr[i]) {
                if (first) {
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_value((void*)&level, cntr[i]->iptr))) {
                        ORTE_ERROR_LOG(rc);
                        return rc;
                    }
                    first = false;
                } else {
                   if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_value((void*)&level2, cntr[i]->iptr))) {
                        ORTE_ERROR_LOG(rc);
                        return rc;
                   }
                   if (level2 != level) {
                        fire = false;
                   }
                }
            }
        }
        if (fire) { /* all levels were equal */
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_register_callback(trig))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            goto FIRED;
        }
    }
    return ORTE_SUCCESS;
    
    /* not comparing levels - check instead to see if counters are at a level */
    cntr = (orte_gpr_replica_counter_t**)((trig->counters)->addr);
    fire = true;
    for (i=0; i < (trig->counters)->size && fire; i++) {
        if (NULL != cntr[i]) {
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_value(&level, cntr[i]->iptr))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (cntr[i]->trigger_level != level) {
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

FIRED:
    /* if notify_at_start set, unset it to indicate that trigger fired */
    if (ORTE_GPR_TRIG_NOTIFY_START & trig->action) {
        trig->action = trig->action & ~ORTE_GPR_TRIG_NOTIFY_START;
    }

    return ORTE_SUCCESS;
}


int orte_gpr_replica_purge_subscriptions(orte_process_name_t *proc)
{
    orte_gpr_replica_triggers_t **trig;
    int i, rc;
    
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
