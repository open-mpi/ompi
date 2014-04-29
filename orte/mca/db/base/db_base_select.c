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

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/util/output.h"

#include "orte/mca/db/base/base.h"

static bool selected = false;

int orte_db_base_select(void)
{
    mca_base_component_list_item_t *cli;
    orte_db_base_component_t *component;
    orte_db_base_active_component_t *active, *ncomponent;
    bool inserted;

    if (selected) {
        /* ensure we don't do this twice */
        return OPAL_SUCCESS;
    }
    selected = true;
    
    /* Query all available components and ask if they have a module */
    OPAL_LIST_FOREACH(cli, &orte_db_base_framework.framework_components, mca_base_component_list_item_t) {
        component = (orte_db_base_component_t*)cli->cli_component;

        opal_output_verbose(5, orte_db_base_framework.framework_output,
                            "mca:db:select: checking available component %s",
                            component->base_version.mca_component_name);

        /* If there's no query function, skip it */
        if (NULL == component->available) {
            opal_output_verbose(5, orte_db_base_framework.framework_output,
                                "mca:db:select: Skipping component [%s]. It does not implement a query function",
                                component->base_version.mca_component_name );
            continue;
        }

        /* Query the component */
        opal_output_verbose(5, orte_db_base_framework.framework_output,
                            "mca:db:select: Querying component [%s]",
                            component->base_version.mca_component_name);

        /* If the component is not available, then skip it as
         * it has no available interfaces
         */
        if (!component->available()) {
            opal_output_verbose(5, orte_db_base_framework.framework_output,
                                "mca:db:select: Skipping component [%s] - not available",
                                component->base_version.mca_component_name );
            continue;
        }

        /* maintain priority order */
        inserted = false;
        ncomponent = OBJ_NEW(orte_db_base_active_component_t);
        ncomponent->component = component;
        OPAL_LIST_FOREACH(active, &orte_db_base.actives, orte_db_base_active_component_t) {
            if (component->priority > active->component->priority) {
                opal_list_insert_pos(&orte_db_base.actives,
                                     &active->super, &ncomponent->super);
                inserted = true;
                break;
            }
        }
        if (!inserted) {
            /* must be lowest priority - add to end */
            opal_list_append(&orte_db_base.actives, &ncomponent->super);
        }
    }

    /* if no components are available, that is an error */
    if (0 == opal_list_get_size(&orte_db_base.actives)) {
        return ORTE_ERR_NOT_FOUND;
    }

    if (4 < opal_output_get_verbosity(orte_db_base_framework.framework_output)) {
        opal_output(0, "Final db priorities");
        /* show the prioritized list */
        OPAL_LIST_FOREACH(active, &orte_db_base.actives, orte_db_base_active_component_t) {
            opal_output(0, "\tComponent: %s Store Priority: %d",
                        active->component->base_version.mca_component_name,
                        active->component->priority);
        }
    }

    return OPAL_SUCCESS;;
}
