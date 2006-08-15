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
#include "orte/util/proc_info.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/replica/api_layer/gpr_replica_api.h"

int orte_gpr_replica_dump_local_triggers(void)
{
    orte_gpr_replica_local_trigger_t **trigs;
    orte_std_cntr_t j, k;
    
    opal_output(orte_gpr_base_output, "DUMP OF LOCAL TRIGGERS for [%lu,%lu,%lu]\n",
            ORTE_NAME_ARGS(orte_process_info.my_name));
    opal_output(orte_gpr_base_output, "Number of triggers: %lu\n", (unsigned long) orte_gpr_replica_globals.num_local_trigs);

    trigs = (orte_gpr_replica_local_trigger_t**)(orte_gpr_replica_globals.local_triggers)->addr;
    for (j=0, k=0; k < orte_gpr_replica_globals.num_local_trigs &&
                   j < (orte_gpr_replica_globals.local_triggers)->size; j++) {
        if (NULL != trigs[j]) {
            k++;
            opal_output(orte_gpr_base_output, "Data for trigger %lu", (unsigned long) trigs[j]->id);
            if (NULL == trigs[j]->name) {
                opal_output(orte_gpr_base_output, "\tNOT a named trigger");
            } else {
                opal_output(orte_gpr_base_output, "\ttrigger name: %s", trigs[j]->name);
            }
            if (NULL == trigs[j]->callback) {
                opal_output(orte_gpr_base_output, "\tNULL callback");
            } else {
                opal_output(orte_gpr_base_output, "\tCallback %0x", trigs[j]->callback);
            }
        }
    }
    return ORTE_SUCCESS;    
}

int orte_gpr_replica_dump_local_subscriptions(void)
{
    orte_gpr_replica_local_subscriber_t **subs;
    orte_std_cntr_t j, k;
    
    opal_output(orte_gpr_base_output, "DUMP OF LOCAL SUBSCRIPTIONS for [%lu,%lu,%lu]\n",
            ORTE_NAME_ARGS(orte_process_info.my_name));
    opal_output(orte_gpr_base_output, "Number of subscriptions: %lu\n", (unsigned long) orte_gpr_replica_globals.num_local_subs);

    subs = (orte_gpr_replica_local_subscriber_t**)(orte_gpr_replica_globals.local_subscriptions)->addr;
    for (j=0, k=0; k < orte_gpr_replica_globals.num_local_subs &&
                   j < (orte_gpr_replica_globals.local_subscriptions)->size; j++) {
        if (NULL != subs[j]) {
            k++;
            opal_output(orte_gpr_base_output, "Data for subscription %lu", (unsigned long) subs[j]->id);
            if (NULL == subs[j]->name) {
                opal_output(orte_gpr_base_output, "\tNOT a named subscription");
            } else {
                opal_output(orte_gpr_base_output, "\tsubscription name: %s", subs[j]->name);
            }
            if (NULL == subs[j]->callback) {
                opal_output(orte_gpr_base_output, "\tNULL callback");
            } else {
                opal_output(orte_gpr_base_output, "\tCallback %0x", subs[j]->callback);
            }        }
    }
    return ORTE_SUCCESS;    
}
