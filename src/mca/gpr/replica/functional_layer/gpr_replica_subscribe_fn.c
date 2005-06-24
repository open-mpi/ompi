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

#include "util/output.h"
#include "util/proc_info.h"
#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"

#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"
#include "gpr_replica_fn.h"

int orte_gpr_replica_subscribe_fn(orte_process_name_t *requestor,
                                  size_t num_subs,
                                  orte_gpr_subscription_t **subscriptions,
                                  size_t num_trigs,
                                  orte_gpr_trigger_t **trigs)
{
    orte_gpr_replica_subscription_t *sub=NULL, **subs;
    orte_gpr_replica_trigger_t *trig=NULL;
    size_t i, j, k, index;
    int rc=ORTE_SUCCESS;

    if (orte_gpr_replica_globals.debug) {
	   ompi_output(0, "[%lu,%lu,%lu] gpr replica: subscribe entered",
		    ORTE_NAME_ARGS(orte_process_info.my_name));
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
        /* link the subscriptions to the new trigger
         */
        for (j=0, k=0; k < num_subs &&
                       j < (orte_gpr_replica_globals.sub_ptrs)->size; j++) {
            if (NULL != subs[j]) {
                k++;
                if (0 > orte_pointer_array_add(&index, trig->subscriptions, subs[j])) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
            }
        }
        trig->num_subscriptions += num_subs;
    }
    
    return rc;
}
