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
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/notifier/base/base.h"


int orte_notifier_base_close(void)
{
    opal_list_item_t *item;
    orte_notifier_base_selected_pair_t *pair;

    if (orte_notifier_base_log_event_selected) {
        orte_notifier_base_events_finalize();
    }

    /* Finalize all the selected modules
     * orte_notifier_base_selected_modules has been built as a merge of the
     * per interface selected modules lists, so only going through that list
     * to invoke the finalize routines is enough.
     */
    for (item = opal_list_remove_first(&orte_notifier_base_selected_modules);
         NULL != item;
         item = opal_list_remove_first(&orte_notifier_base_selected_modules)) {
        pair = (orte_notifier_base_selected_pair_t*) item;
        if (NULL != pair->onbsp_module->finalize) {
            pair->onbsp_module->finalize();
        }
        free(pair);
    }
    OBJ_DESTRUCT(&orte_notifier_base_selected_modules);
    OBJ_DESTRUCT(&orte_notifier_log_selected_modules);
    OBJ_DESTRUCT(&orte_notifier_help_selected_modules);
    OBJ_DESTRUCT(&orte_notifier_log_peer_selected_modules);
    OBJ_DESTRUCT(&orte_notifier_log_event_selected_modules);

    /* Close all remaining available components */
    mca_base_components_close(orte_notifier_base_output, 
                              &orte_notifier_base_components_available, NULL);
    
    /* All done */
    return ORTE_SUCCESS;
}
