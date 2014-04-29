/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved. 
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
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

#include "opal/mca/dstore/base/base.h"

static bool selected = false;

int 
opal_dstore_base_select(void)
{
    mca_base_component_list_item_t *cli;
    opal_dstore_base_component_t *component, *best=NULL;
    int pri = -1000;

    if (selected) {
        /* ensure we don't do this twice */
        return OPAL_SUCCESS;
    }
    selected = true;
    
    /* Query all available components and ask if they have a module */
    OPAL_LIST_FOREACH(cli, &opal_dstore_base_framework.framework_components, mca_base_component_list_item_t) {
        component = (opal_dstore_base_component_t*)cli->cli_component;

        opal_output_verbose(5, opal_dstore_base_framework.framework_output,
                            "mca:dstore:select: checking available component %s",
                            component->base_version.mca_component_name);

        /* If there's no query function, skip it */
        if (NULL == component->available) {
            opal_output_verbose(5, opal_dstore_base_framework.framework_output,
                                "mca:dstore:select: Skipping component [%s]. It does not implement a query function",
                                component->base_version.mca_component_name );
            continue;
        }

        /* Query the component */
       opal_output_verbose(5, opal_dstore_base_framework.framework_output,
                            "mca:dstore:select: Querying component [%s]",
                            component->base_version.mca_component_name);

        /* If the component is not available, then skip it as
         * it has no available interfaces
         */
        if (!component->available()) {
            opal_output_verbose(5, opal_dstore_base_framework.framework_output,
                                "mca:dstore:select: Skipping component [%s] - not available",
                                component->base_version.mca_component_name );
            continue;
        }

        /* keep only the highest priority component */
        if (pri < component->priority) {
            best = component;
            pri = component->priority;
        }
    }

    /* if no components are available, that is an error */
    if (NULL == best) {
        return OPAL_ERR_NOT_FOUND;
    }

    opal_dstore_base.active = best;
    return OPAL_SUCCESS;;
}
