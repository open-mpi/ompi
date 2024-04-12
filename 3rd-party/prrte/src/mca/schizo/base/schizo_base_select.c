/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <stdio.h>
#include <string.h>

#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/util/pmix_output.h"

#include "src/util/pmix_show_help.h"

#include "src/mca/schizo/base/base.h"
#include "src/mca/schizo/schizo.h"
#include "src/runtime/prte_globals.h"

/**
 * Function for selecting all runnable modules from those that are
 * available.
 */

int prte_schizo_base_select(void)
{
    pmix_mca_base_component_list_item_t *cli = NULL;
    pmix_mca_base_component_t *component = NULL;
    pmix_mca_base_module_t *module = NULL;
    prte_schizo_base_module_t *nmodule;
    prte_schizo_base_active_module_t *newmodule, *mod;
    int rc, priority;
    bool inserted;

    if (0 < pmix_list_get_size(&prte_schizo_base.active_modules)) {
        /* ensure we don't do this twice */
        return PRTE_SUCCESS;
    }

    /* Query all available components and ask if they have a module */
    PMIX_LIST_FOREACH(cli, &prte_schizo_base_framework.framework_components,
                      pmix_mca_base_component_list_item_t)
    {
        component = (pmix_mca_base_component_t *) cli->cli_component;

        pmix_output_verbose(5, prte_schizo_base_framework.framework_output,
                            "mca:schizo:select: checking available component %s",
                            component->pmix_mca_component_name);

        /* If there's no query function, skip it */
        if (NULL == component->pmix_mca_query_component) {
            pmix_output_verbose(5, prte_schizo_base_framework.framework_output,
                                "mca:schizo:select: Skipping component [%s]. It does not implement "
                                "a query function",
                                component->pmix_mca_component_name);
            continue;
        }

        /* Query the component */
        pmix_output_verbose(5, prte_schizo_base_framework.framework_output,
                            "mca:schizo:select: Querying component [%s]",
                            component->pmix_mca_component_name);
        rc = component->pmix_mca_query_component(&module, &priority);

        /* If no module was returned, then skip component */
        if (PRTE_SUCCESS != rc || NULL == module) {
            pmix_output_verbose(
                5, prte_schizo_base_framework.framework_output,
                "mca:schizo:select: Skipping component [%s]. Query failed to return a module",
                component->pmix_mca_component_name);
            continue;
        }

        /* If we got a module, keep it */
        nmodule = (prte_schizo_base_module_t *) module;
        /* add to the list of active modules */
        newmodule = PMIX_NEW(prte_schizo_base_active_module_t);
        newmodule->pri = priority;
        newmodule->module = nmodule;
        newmodule->component = component;

        /* maintain priority order */
        inserted = false;
        PMIX_LIST_FOREACH(mod, &prte_schizo_base.active_modules, prte_schizo_base_active_module_t)
        {
            if (priority > mod->pri) {
                pmix_list_insert_pos(&prte_schizo_base.active_modules, (pmix_list_item_t *) mod,
                                     &newmodule->super);
                inserted = true;
                break;
            }
        }
        if (!inserted) {
            /* must be lowest priority - add to end */
            pmix_list_append(&prte_schizo_base.active_modules, &newmodule->super);
        }
    }

    if (4 < pmix_output_get_verbosity(prte_schizo_base_framework.framework_output)) {
        pmix_output(0, "Final schizo priorities");
        /* show the prioritized list */
        PMIX_LIST_FOREACH(mod, &prte_schizo_base.active_modules, prte_schizo_base_active_module_t)
        {
            pmix_output(0, "\tSchizo: %s Priority: %d", mod->component->pmix_mca_component_name,
                        mod->pri);
        }
    }

    return PRTE_SUCCESS;
    ;
}
