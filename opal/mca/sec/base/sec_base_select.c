/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#include <string.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/util/output.h"

#include "opal/mca/sec/base/base.h"

static bool selected = false;

/*
 * Function for selecting one component from all those that are
 * available.
 */
int opal_sec_base_select(void)
{
    mca_base_component_list_item_t *cli = NULL;
    mca_base_component_t *component = NULL;
    mca_base_module_t *module = NULL;
    opal_sec_base_module_t *smodule, *nmodule = NULL;
    int rc, priority, pri = -1;

    if (selected) {
        /* ensure we don't do this twice */
        return OPAL_SUCCESS;
    }
    selected = true;

    /* Query all available components and ask if they have a module */
    OPAL_LIST_FOREACH(cli, &opal_sec_base_framework.framework_components, mca_base_component_list_item_t) {
        component = (mca_base_component_t *) cli->cli_component;

        opal_output_verbose(5, opal_sec_base_framework.framework_output,
                            "mca:sec:select: checking available component %s", component->mca_component_name);

        /* If there's no query function, skip it */
        if (NULL == component->mca_query_component) {
            opal_output_verbose(5, opal_sec_base_framework.framework_output,
                                "mca:sec:select: Skipping component [%s]. It does not implement a query function",
                                component->mca_component_name );
            continue;
        }

        /* Query the component */
        opal_output_verbose(5, opal_sec_base_framework.framework_output,
                            "mca:sec:select: Querying component [%s]",
                            component->mca_component_name);
        rc = component->mca_query_component(&module, &priority);

        /* If no module was returned, then skip component */
        if (OPAL_SUCCESS != rc || NULL == module) {
            opal_output_verbose(5, opal_sec_base_framework.framework_output,
                                "mca:sec:select: Skipping component [%s]. Query failed to return a module",
                                component->mca_component_name );
            continue;
        }

        smodule = (opal_sec_base_module_t*)module;
        /* modules are required to have an init function */
        if (NULL == smodule->init) {
            /* report the error */
            opal_output_verbose(5, opal_sec_base_framework.framework_output,
                                "mca:sec:select: Skipping component [%s]. It does not implement an init function",
                                component->mca_component_name);
            continue;
        }

        /* if we got a module, let it try to initialize */
        if (OPAL_SUCCESS != (rc = smodule->init())) {
            /* couldn't init - ignore it */
            opal_output_verbose(5, opal_sec_base_framework.framework_output,
                                "mca:sec:select: Skipping component [%s]. Init returned %d",
                                component->mca_component_name, rc);
            continue;
        }

        /* see if this is the one to keep - only retain the highest priority */
        if (pri < priority) {
            nmodule = smodule;
            pri = priority;
        }
    }

    if (NULL == nmodule) {
        /* no module available - error out */
        return OPAL_ERROR;
    }

    opal_sec = *nmodule;

    return OPAL_SUCCESS;;
}
