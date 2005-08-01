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

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#include "orte/include/orte_constants.h"

#include "orte/mca/errmgr/errmgr.h"

#include "opal/util/output.h"

#include "gpr_proxy.h"

int orte_gpr_proxy_dump_local_triggers(int output_id)
{
    orte_gpr_proxy_trigger_t **trigs;
    size_t j, k;
    
    opal_output(output_id, "DUMP OF LOCAL TRIGGERS for [%lu,%lu,%lu]\n",
            ORTE_NAME_ARGS(orte_process_info.my_name));
    opal_output(output_id, "Number of triggers: %lu\n", (unsigned long) orte_gpr_proxy_globals.num_trigs);

    trigs = (orte_gpr_proxy_trigger_t**)(orte_gpr_proxy_globals.triggers)->addr;
    for (j=0, k=0; k < orte_gpr_proxy_globals.num_trigs &&
                   j < (orte_gpr_proxy_globals.triggers)->size; j++) {
        if (NULL != trigs[j]) {
            k++;
            opal_output(output_id, "Data for trigger %lu", (unsigned long) trigs[j]->id);
            if (NULL == trigs[j]->name) {
                opal_output(output_id, "\tNOT a named trigger");
            } else {
                opal_output(output_id, "\ttrigger name: %s", trigs[j]->name);
            }
        }
    }
    return ORTE_SUCCESS;    
}

int orte_gpr_proxy_dump_local_subscriptions(int output_id)
{
    orte_gpr_proxy_subscriber_t **subs;
    size_t j, k;
    
    opal_output(output_id, "DUMP OF LOCAL SUBSCRIPTIONS for [%lu,%lu,%lu]\n",
            ORTE_NAME_ARGS(orte_process_info.my_name));
    opal_output(output_id, "Number of subscriptions: %lu\n", (unsigned long) orte_gpr_proxy_globals.num_subs);

    subs = (orte_gpr_proxy_subscriber_t**)(orte_gpr_proxy_globals.subscriptions)->addr;
    for (j=0, k=0; k < orte_gpr_proxy_globals.num_subs &&
                   j < (orte_gpr_proxy_globals.subscriptions)->size; j++) {
        if (NULL != subs[j]) {
            k++;
            opal_output(output_id, "Data for subscription %lu", (unsigned long) subs[j]->id);
            if (NULL == subs[j]->name) {
                opal_output(output_id, "\tNOT a named subscription");
            } else {
                opal_output(output_id, "\tsubscription name: %s", subs[j]->name);
            }
        }
    }
    return ORTE_SUCCESS;    
}
