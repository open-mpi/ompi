/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved. 
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/util/output.h"

#include "opal/mca/db/base/base.h"

static bool selected = false;

int 
opal_db_base_select(bool restrict_local)
{
    mca_base_component_list_item_t *cli = NULL;
    opal_db_base_component_t *component = NULL;
    opal_db_base_module_t *module = NULL;
    opal_db_active_module_t *nmodule, *mod;
    int rc, fetch, store;
    bool inserted;

    if (selected) {
        /* ensure we don't do this twice */
        return OPAL_SUCCESS;
    }
    selected = true;
    
    /* Query all available components and ask if they have a module */
    OPAL_LIST_FOREACH(cli, &opal_db_base_framework.framework_components, mca_base_component_list_item_t) {
        component = (opal_db_base_component_t *) cli->cli_component;

        opal_output_verbose(5, opal_db_base_framework.framework_output,
                            "mca:db:select: checking available component %s",
                            component->base_version.mca_component_name);

        /* If there's no query function, skip it */
        if (NULL == component->query) {
            opal_output_verbose(5, opal_db_base_framework.framework_output,
                                "mca:db:select: Skipping component [%s]. It does not implement a query function",
                                component->base_version.mca_component_name );
            continue;
        }

        /* Query the component */
        opal_output_verbose(5, opal_db_base_framework.framework_output,
                            "mca:db:select: Querying component [%s]",
                            component->base_version.mca_component_name);
        rc = component->query(&module, &store, &fetch, restrict_local);

        /* If no module was returned, then skip component */
        if (OPAL_SUCCESS != rc || NULL == module) {
            opal_output_verbose(5, opal_db_base_framework.framework_output,
                                "mca:db:select: Skipping component [%s]. Query failed to return a module",
                                component->base_version.mca_component_name );
            continue;
        }

        /* attempt to initialize the module */
        if (NULL != module->init) {
            if (OPAL_SUCCESS != (rc = module->init())) {
                /* skip the module */
                continue;
            }
        }

        /* If we got a module, add to the store list */
        nmodule = OBJ_NEW(opal_db_active_module_t);
        nmodule->pri = store;
        nmodule->module = module;
        nmodule->component = component;

        /* maintain priority order */
        inserted = false;
        OPAL_LIST_FOREACH(mod, &opal_db_base.store_order, opal_db_active_module_t) {
            if (store > mod->pri) {
                opal_list_insert_pos(&opal_db_base.store_order,
                                     &mod->super, &nmodule->super);
                inserted = true;
                break;
            }
        }
        if (!inserted) {
            /* must be lowest priority - add to end */
            opal_list_append(&opal_db_base.store_order, &nmodule->super);
        }

        /* do the same for fetch list */
        nmodule = OBJ_NEW(opal_db_active_module_t);
        nmodule->pri = fetch;
        nmodule->module = module;
        nmodule->component = component;

        /* maintain priority order */
        inserted = false;
        OPAL_LIST_FOREACH(mod, &opal_db_base.fetch_order, opal_db_active_module_t) {
            if (fetch > mod->pri) {
                opal_list_insert_pos(&opal_db_base.fetch_order,
                                     &mod->super, &nmodule->super);
                inserted = true;
                break;
            }
        }
        if (!inserted) {
            /* must be lowest priority - add to end */
            opal_list_append(&opal_db_base.fetch_order, &nmodule->super);
        }
    }

    if (4 < opal_output_get_verbosity(opal_db_base_framework.framework_output)) {
        opal_output(0, "Final db priorities");
        /* show the prioritized list */
        OPAL_LIST_FOREACH(mod, &opal_db_base.store_order, opal_db_active_module_t) {
            opal_output(0, "\tComponent: %s Store Priority: %d",
                        mod->component->base_version.mca_component_name, mod->pri);
        }
        OPAL_LIST_FOREACH(mod, &opal_db_base.fetch_order, opal_db_active_module_t) {
            opal_output(0, "\tComponent: %s Fetch Priority: %d",
                        mod->component->base_version.mca_component_name, mod->pri);
        }
    }

    return OPAL_SUCCESS;;
}
