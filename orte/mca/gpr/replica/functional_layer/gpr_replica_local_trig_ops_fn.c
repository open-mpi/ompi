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


/* FUNCTIONS REQUIRED FOR LOCAL SUBSCRIPTION AND TRIGGER
 * REGISTRATION
 */
int
orte_gpr_replica_enter_local_subscription(size_t cnt, orte_gpr_subscription_t **subscriptions)
{
    orte_gpr_replica_local_subscriber_t *sub;
    size_t i, id;
    
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
        if (0 > orte_pointer_array_add(&id, orte_gpr_replica_globals.local_subscriptions, sub)) {
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
orte_gpr_replica_enter_local_trigger(size_t cnt, orte_gpr_trigger_t **trigs)
{
    orte_gpr_replica_local_trigger_t *trig;
    size_t i, id;
    
    for (i=0; i < cnt; i++) {
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
        if (0 > orte_pointer_array_add(&id, orte_gpr_replica_globals.local_triggers, trig)) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        trig->id = orte_gpr_replica_globals.num_local_trigs;
        trigs[i]->id = trig->id;
        (orte_gpr_replica_globals.num_local_trigs)++;
    }
    
    return ORTE_SUCCESS;
}

int orte_gpr_replica_remove_local_subscription(orte_gpr_subscription_id_t id)
{
    if (NULL != (orte_gpr_replica_globals.local_subscriptions)->addr[id]) {
        OBJ_RELEASE((orte_gpr_replica_globals.local_subscriptions)->addr[id]);
        orte_pointer_array_set_item(orte_gpr_replica_globals.local_subscriptions, (size_t)id, NULL);
    }
    
    return ORTE_SUCCESS;
}

int orte_gpr_proxy_remove_local_trigger(orte_gpr_trigger_id_t id)
{
    if (NULL != (orte_gpr_replica_globals.local_triggers)->addr[id]) {
        OBJ_RELEASE((orte_gpr_replica_globals.local_triggers)->addr[id]);
        orte_pointer_array_set_item(orte_gpr_replica_globals.local_triggers, (size_t)id, NULL);
    }
    
    return ORTE_SUCCESS;
}

