/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"
#include "pmix_common.h"

#include <string.h>

#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"

#include "src/mca/psensor/base/base.h"

/* Function for selecting a prioritized list of components
 * from all those that are available. */
int pmix_psensor_base_select(void)
{
    pmix_mca_base_component_list_item_t *cli = NULL;
    pmix_psensor_base_component_t *component = NULL;
    pmix_psensor_active_module_t *newactive, *active;
    pmix_mca_base_module_t *mod;
    int pri;
    bool inserted;

    if (pmix_psensor_base.selected) {
        /* ensure we don't do this twice */
        return PMIX_SUCCESS;
    }
    pmix_psensor_base.selected = true;

    /* Query all available components and ask if they have a module */
    PMIX_LIST_FOREACH (cli, &pmix_psensor_base_framework.framework_components,
                       pmix_mca_base_component_list_item_t) {
        component = (pmix_psensor_base_component_t *) cli->cli_component;

        pmix_output_verbose(5, pmix_psensor_base_framework.framework_output,
                            "mca:psensor:select: checking available component %s",
                            component->pmix_mca_component_name);

        /* get the module for this component */
        if (PMIX_SUCCESS != component->pmix_mca_query_component(&mod, &pri)) {
            continue;
        }

        /* add to our prioritized list of available actives */
        newactive = PMIX_NEW(pmix_psensor_active_module_t);
        newactive->priority = pri;
        newactive->component = component;
        newactive->module = (pmix_psensor_base_module_t *) mod;

        /* maintain priority order */
        inserted = false;
        PMIX_LIST_FOREACH (active, &pmix_psensor_base.actives, pmix_psensor_active_module_t) {
            if (newactive->priority > active->priority) {
                pmix_list_insert_pos(&pmix_psensor_base.actives, (pmix_list_item_t *) active,
                                     &newactive->super);
                inserted = true;
                break;
            }
        }
        if (!inserted) {
            /* must be lowest priority - add to end */
            pmix_list_append(&pmix_psensor_base.actives, &newactive->super);
        }
    }

    if (4 < pmix_output_get_verbosity(pmix_psensor_base_framework.framework_output)) {
        pmix_output(0, "Final PSENSOR priorities");
        /* show the prioritized list */
        PMIX_LIST_FOREACH (active, &pmix_psensor_base.actives, pmix_psensor_active_module_t) {
            pmix_output(0, "\tPSENSOR: %s Priority: %d",
                        active->component->pmix_mca_component_name, active->priority);
        }
    }

    return PMIX_SUCCESS;
    ;
}
