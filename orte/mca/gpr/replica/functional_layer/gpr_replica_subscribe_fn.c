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
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/util/proc_info.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/replica/transition_layer/gpr_replica_tl.h"
#include "orte/mca/gpr/replica/functional_layer/gpr_replica_fn.h"

int orte_gpr_replica_subscribe_fn(orte_process_name_t *requestor,
                                  orte_std_cntr_t num_subs,
                                  orte_gpr_subscription_t **subscriptions,
                                  orte_std_cntr_t num_trigs,
                                  orte_gpr_trigger_t **trigs)
{
    orte_gpr_replica_subscription_t *sub=NULL, **subs, **trigsubs;
    orte_gpr_replica_trigger_t *trig=NULL;
    orte_std_cntr_t i, j, k, m, n, index;
    bool ignore;
    int rc=ORTE_SUCCESS;

    OPAL_TRACE(2);

    if (orte_gpr_replica_globals.debug) {
        opal_output(0, "[%lu,%lu,%lu] gpr_replica_subscribe: entered with num_trigs:%d",
                    ORTE_NAME_ARGS(orte_process_info.my_name), num_trigs);
    }

    /* ensure one of the search arrays is clear - in this case, we
     * use the sub_ptrs array to temporarily store the subscription pointers so we
     * can properly link them to the triggers
     */
    orte_pointer_array_clear(orte_gpr_replica_globals.sub_ptrs);
    subs = (orte_gpr_replica_subscription_t**)(orte_gpr_replica_globals.sub_ptrs)->addr;
    
    for (i=0; i < num_subs; i++) {
        if (ORTE_SUCCESS != (rc =
            orte_gpr_replica_register_subscription(&sub, requestor, subscriptions[i]))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* add the new subscription so we can link
         * it to the triggers later
         */
        if (0 > orte_pointer_array_add(&index, orte_gpr_replica_globals.sub_ptrs, sub)) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
    }
    
    /* now register any triggers */
    for (i=0; i < num_trigs; i++) {
        if (ORTE_SUCCESS != (rc =
            orte_gpr_replica_register_trigger(&trig, requestor, trigs[i]))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* link the subscriptions to the new trigger. only do this if the
         * subscription doesn't already exist on this trigger - otherwise,
         * we'd just be duplicating things.
         */
        trigsubs = (orte_gpr_replica_subscription_t**)(trig->subscriptions)->addr;
        for (j=0, k=0; k < num_subs &&
                       j < (orte_gpr_replica_globals.sub_ptrs)->size; j++) {
            if (NULL != subs[j]) {
                k++;
                /* check to see if this subscription is already attached
                 * to this trigger - if not, add it
                 */
                ignore = false;
                for (m=0, n=0; n < trig->num_subscriptions &&
                               m < (trig->subscriptions)->size; m++) {
                    if (NULL != trigsubs[m]) {
                        n++;
                        if (subs[j] == trigsubs[m]) { /* already present */
                            ignore = true;
                        }
                    }
                }
                if (!ignore) { /* new sub for this trig - add it */
                    if (0 > orte_pointer_array_add(&index, trig->subscriptions, subs[j])) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                    (trig->num_subscriptions)++;
                }
            }
        }
    }
    
    return rc;
}
