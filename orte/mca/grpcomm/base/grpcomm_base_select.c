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
 * Copyright (c) 2014      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"

#include "orte/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/grpcomm/base/base.h"


static bool selected = false;

/**
 * Function for selecting one component from all those that are
 * available.
 */
int orte_grpcomm_base_select(void)
{
    mca_base_component_list_item_t *cli = NULL;
    mca_base_component_t *component = NULL;
    mca_base_module_t *module = NULL;
    orte_grpcomm_base_module_t *nmodule;
    orte_grpcomm_base_active_t *newmodule, *mod;
    int rc, priority;
    bool inserted;

    if (selected) {
        /* ensure we don't do this twice */
        return ORTE_SUCCESS;
    }
    selected = true;

    /* Query all available components and ask if they have a module */
    OPAL_LIST_FOREACH(cli, &orte_grpcomm_base_framework.framework_components, mca_base_component_list_item_t) {
        component = (mca_base_component_t *) cli->cli_component;

        opal_output_verbose(5, orte_grpcomm_base_framework.framework_output,
                            "mca:grpcomm:select: checking available component %s", component->mca_component_name);

        /* If there's no query function, skip it */
        if (NULL == component->mca_query_component) {
            opal_output_verbose(5, orte_grpcomm_base_framework.framework_output,
                                "mca:grpcomm:select: Skipping component [%s]. It does not implement a query function",
                                component->mca_component_name );
            continue;
        }

        /* Query the component */
        opal_output_verbose(5, orte_grpcomm_base_framework.framework_output,
                            "mca:grpcomm:select: Querying component [%s]",
                            component->mca_component_name);
        rc = component->mca_query_component(&module, &priority);

        /* If no module was returned, then skip component */
        if (ORTE_SUCCESS != rc || NULL == module) {
            opal_output_verbose(5, orte_grpcomm_base_framework.framework_output,
                                "mca:grpcomm:select: Skipping component [%s]. Query failed to return a module",
                                component->mca_component_name );
            continue;
        }
        nmodule = (orte_grpcomm_base_module_t*) module;

        /* if the module fails to init, skip it */
        if (NULL == nmodule->init || ORTE_SUCCESS != nmodule->init()) {
            continue;
        }

        /* add to the list of selected modules */
        newmodule = OBJ_NEW(orte_grpcomm_base_active_t);
        newmodule->pri = priority;
        newmodule->module = nmodule;
        newmodule->component = component;

        /* maintain priority order */
        inserted = false;
        OPAL_LIST_FOREACH(mod, &orte_grpcomm_base.actives, orte_grpcomm_base_active_t) {
            if (priority > mod->pri) {
                opal_list_insert_pos(&orte_grpcomm_base.actives,
                                     (opal_list_item_t*)mod, &newmodule->super);
                inserted = true;
                break;
            }
        }
        if (!inserted) {
            /* must be lowest priority - add to end */
            opal_list_append(&orte_grpcomm_base.actives, &newmodule->super);
        }
    }

    if (4 < opal_output_get_verbosity(orte_grpcomm_base_framework.framework_output)) {
        opal_output(0, "%s: Final grpcomm priorities", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* show the prioritized list */
        OPAL_LIST_FOREACH(mod, &orte_grpcomm_base.actives, orte_grpcomm_base_active_t) {
            opal_output(0, "\tComponent: %s Priority: %d", mod->component->mca_component_name, mod->pri);
        }
    }
    return ORTE_SUCCESS;
}
