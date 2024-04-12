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
 * Copyright (c) 2013      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"

#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"

#include "src/mca/grpcomm/base/base.h"

static bool selected = false;

/**
 * Function for selecting one component from all those that are
 * available.
 */
int prte_grpcomm_base_select(void)
{
    pmix_mca_base_component_list_item_t *cli = NULL;
    pmix_mca_base_component_t *component = NULL;
    pmix_mca_base_module_t *module = NULL;
    prte_grpcomm_base_module_t *nmodule;
    prte_grpcomm_base_active_t *newmodule, *mod;
    int rc, priority;
    bool inserted;

    if (selected) {
        /* ensure we don't do this twice */
        return PRTE_SUCCESS;
    }
    selected = true;

    /* Query all available components and ask if they have a module */
    PMIX_LIST_FOREACH(cli, &prte_grpcomm_base_framework.framework_components,
                      pmix_mca_base_component_list_item_t)
    {
        component = (pmix_mca_base_component_t *) cli->cli_component;

        pmix_output_verbose(5, prte_grpcomm_base_framework.framework_output,
                            "mca:grpcomm:select: checking available component %s",
                            component->pmix_mca_component_name);

        /* If there's no query function, skip it */
        if (NULL == component->pmix_mca_query_component) {
            pmix_output_verbose(5, prte_grpcomm_base_framework.framework_output,
                                "mca:grpcomm:select: Skipping component [%s]. It does not "
                                "implement a query function",
                                component->pmix_mca_component_name);
            continue;
        }

        /* Query the component */
        pmix_output_verbose(5, prte_grpcomm_base_framework.framework_output,
                            "mca:grpcomm:select: Querying component [%s]",
                            component->pmix_mca_component_name);
        rc = component->pmix_mca_query_component(&module, &priority);

        /* If no module was returned, then skip component */
        if (PRTE_SUCCESS != rc || NULL == module) {
            pmix_output_verbose(
                5, prte_grpcomm_base_framework.framework_output,
                "mca:grpcomm:select: Skipping component [%s]. Query failed to return a module",
                component->pmix_mca_component_name);
            continue;
        }
        nmodule = (prte_grpcomm_base_module_t *) module;

        /* if the module fails to init, skip it */
        if (NULL == nmodule->init || PRTE_SUCCESS != nmodule->init()) {
            continue;
        }

        /* add to the list of selected modules */
        newmodule = PMIX_NEW(prte_grpcomm_base_active_t);
        newmodule->pri = priority;
        newmodule->module = nmodule;
        newmodule->component = component;

        /* maintain priority order */
        inserted = false;
        PMIX_LIST_FOREACH(mod, &prte_grpcomm_base.actives, prte_grpcomm_base_active_t)
        {
            if (priority > mod->pri) {
                pmix_list_insert_pos(&prte_grpcomm_base.actives, (pmix_list_item_t *) mod,
                                     &newmodule->super);
                inserted = true;
                break;
            }
        }
        if (!inserted) {
            /* must be lowest priority - add to end */
            pmix_list_append(&prte_grpcomm_base.actives, &newmodule->super);
        }
    }

    if (4 < pmix_output_get_verbosity(prte_grpcomm_base_framework.framework_output)) {
        pmix_output(0, "%s: Final grpcomm priorities", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        /* show the prioritized list */
        PMIX_LIST_FOREACH(mod, &prte_grpcomm_base.actives, prte_grpcomm_base_active_t)
        {
            pmix_output(0, "\tComponent: %s Priority: %d", mod->component->pmix_mca_component_name,
                        mod->pri);
        }
    }
    return PRTE_SUCCESS;
}
