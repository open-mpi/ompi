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
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "include/orte_constants.h"
#include "opal/util/output.h"
#include "util/proc_info.h"

#include "mca/errmgr/errmgr.h"
#include "mca/ns/ns_types.h"
#include "mca/oob/oob_types.h"
#include "mca/rml/rml.h"

#include "gpr_proxy.h"

int
orte_gpr_proxy_enter_subscription(size_t cnt, orte_gpr_subscription_t **subscriptions)
{
    orte_gpr_proxy_subscriber_t *sub;
    size_t i, id;
    
    for (i=0; i < cnt; i++) {
        sub = OBJ_NEW(orte_gpr_proxy_subscriber_t);
        if (NULL == sub) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        sub->callback = subscriptions[i]->cbfunc;
        sub->user_tag = subscriptions[i]->user_tag;
        if (0 > orte_pointer_array_add(&id, orte_gpr_proxy_globals.subscriptions, sub)) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        sub->id = (orte_gpr_subscription_id_t)id;
        subscriptions[i]->id = sub->id;
        (orte_gpr_proxy_globals.num_subs)++;
    }
    
    return ORTE_SUCCESS;
}


int
orte_gpr_proxy_enter_trigger(size_t cnt, orte_gpr_trigger_t **trigs)
{
    size_t i;
    
    for (i=0; i < cnt; i++) {
        if (ORTE_GPR_TRIGGER_ID_MAX-1 > orte_gpr_proxy_globals.trig_cntr) {
            trigs[i]->id = orte_gpr_proxy_globals.trig_cntr;
            (orte_gpr_proxy_globals.trig_cntr)++;
        } else {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
    }
    
    return ORTE_SUCCESS;
}


int
orte_gpr_proxy_remove_subscription(orte_gpr_subscription_id_t id)
{
    if (NULL != (orte_gpr_proxy_globals.subscriptions)->addr[id]) {
        OBJ_RELEASE((orte_gpr_proxy_globals.subscriptions)->addr[id]);
        orte_pointer_array_set_item(orte_gpr_proxy_globals.subscriptions, (size_t)id, NULL);
    }
    
    return ORTE_SUCCESS;
}

