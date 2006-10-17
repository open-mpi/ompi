/*
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
 * The Open MPI general purpose registry - support functions.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "orte/mca/gpr/replica/transition_layer/gpr_replica_tl.h"
#include "orte/mca/gpr/replica/functional_layer/gpr_replica_fn.h"


/* FUNCTIONS REQUIRED FOR LOCAL SUBSCRIPTION AND TRIGGER
 * REGISTRATION
 */
int
orte_gpr_replica_enter_local_subscription(orte_std_cntr_t cnt, orte_gpr_subscription_t **subscriptions)
{
    orte_gpr_replica_local_subscriber_t *sub;
    orte_std_cntr_t i;

    OPAL_TRACE(2);
    
    for (i=0; i < cnt; i++) {
        sub = OBJ_NEW(orte_gpr_replica_local_subscriber_t);
        if (NULL == sub) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        if (NULL != subscriptions[i]->name) {
            sub->name = strdup(subscriptions[i]->name);
        }
        sub->callback = subscriptions[i]->cbfunc;
        sub->user_tag = subscriptions[i]->user_tag;
        if (0 > orte_pointer_array_add(&sub->index, orte_gpr_replica_globals.local_subscriptions, sub)) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        sub->id = orte_gpr_replica_globals.num_local_subs;
        subscriptions[i]->id = sub->id;
        (orte_gpr_replica_globals.num_local_subs)++;
    }

    return ORTE_SUCCESS;
}


int
orte_gpr_replica_enter_local_trigger(orte_std_cntr_t cnt, orte_gpr_trigger_t **trigs)
{
    orte_gpr_replica_local_trigger_t *trig, **tptr;
    orte_std_cntr_t i, j, k;

    OPAL_TRACE(2);

    for (i=0; i < cnt; i++) {
        /* If the provided trigger has a name, see if it already is on
         * the local trigger list. If so, then check to see if we
         * already defined a return point for it and/or if this trigger
         * doesn't - in either of those two cases, we ignore the
         * trigger and just use the existing entry
         */
        if (NULL != trigs[i]->name) {
            tptr = (orte_gpr_replica_local_trigger_t**)(orte_gpr_replica_globals.local_triggers)->addr;
            for (j=0, k=0; k < orte_gpr_replica_globals.num_local_trigs &&
                           j < (orte_gpr_replica_globals.local_triggers)->size; j++) {
                if (NULL != tptr[j]) {
                    k++;
                    if (NULL != tptr[j]->name && 0 == strcmp(tptr[j]->name, trigs[i]->name)) {
                        /* same name - trigger is already on list */
                        if (NULL != tptr[j]->callback || NULL == trigs[i]->cbfunc) {
                            /* ignore these cases */
                            trig = tptr[j];
                            goto MOVEON;
                        }
                        /* reach here if either the prior trigger didn't provide
                         * a callback, and the new one provides one. In this
                         * case, we update the existing trigger callback and then
                         * move on
                         */
                         tptr[j]->callback = trigs[i]->cbfunc;
                         trig = tptr[j];
                         goto MOVEON;
                    }
                }
            }
        }

        /* either the trigger doesn't have a name, OR it did, but it isn't
         * already on the list - add it to the list now
         */
        trig = OBJ_NEW(orte_gpr_replica_local_trigger_t);
        if (NULL == trig) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        if (NULL != trigs[i]->name) {
            trig->name = strdup(trigs[i]->name);
        }
        /* ensure that the proper routing flag is set
         * in the action field to match the trigger callback
         * function
         */
        if (NULL != trigs[i]->cbfunc) {
            trigs[i]->action = trigs[i]->action |
                    ORTE_GPR_TRIG_ROUTE_DATA_THRU_ME;
        } else {
            trigs[i]->action = trigs[i]->action &
                    ~ORTE_GPR_TRIG_ROUTE_DATA_THRU_ME;
        }
        trig->callback = trigs[i]->cbfunc;
        trig->user_tag = trigs[i]->user_tag;
        if (0 > orte_pointer_array_add(&trig->index, orte_gpr_replica_globals.local_triggers, trig)) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        trig->id = orte_gpr_replica_globals.num_local_trigs;
        (orte_gpr_replica_globals.num_local_trigs)++;
MOVEON:
        trigs[i]->id = trig->id;

    }

    return ORTE_SUCCESS;
}

int orte_gpr_replica_remove_local_subscription(orte_gpr_replica_local_subscriber_t *sub)
{
    orte_std_cntr_t index;
    
    OPAL_TRACE(2);
 
    if (NULL == sub) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    index = sub->index;
    OBJ_RELEASE(sub);
    orte_pointer_array_set_item(orte_gpr_replica_globals.local_subscriptions, index, NULL);

    return ORTE_SUCCESS;
}

int orte_gpr_replica_remove_local_trigger(orte_gpr_replica_local_trigger_t *trig)
{
    orte_std_cntr_t index;
    
    OPAL_TRACE(2);
 
    if (NULL == trig) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    index = trig->index;
    OBJ_RELEASE(trig);
    orte_pointer_array_set_item(orte_gpr_replica_globals.local_triggers, index, NULL);

    return ORTE_SUCCESS;
}

