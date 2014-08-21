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
    mca_base_component_t *cmp;
    mca_base_module_t *md;
    int priority, cmp_pri, mod_pri;
    opal_dstore_base_module_t *mod=NULL;
    opal_dstore_base_component_t *comp=NULL;

    if (selected) {
        /* ensure we don't do this twice */
        return OPAL_SUCCESS;
    }
    selected = true;
    
    /* Query all available components and ask if they have a module */
    cmp_pri = -100000;
    mod_pri = -100000;
    OPAL_LIST_FOREACH(cli, &opal_dstore_base_framework.framework_components, mca_base_component_list_item_t) {
        cmp = (mca_base_component_t*)cli->cli_component;

        opal_output_verbose(5, opal_dstore_base_framework.framework_output,
                            "mca:dstore:select: checking available component %s",
                            cmp->mca_component_name);

        /* If there's no query function, skip it */
        if (NULL == cmp->mca_query_component) {
            opal_output_verbose(5, opal_dstore_base_framework.framework_output,
                                "mca:dstore:select: Skipping component [%s]. It does not implement a query function",
                                cmp->mca_component_name );
            continue;
        }

        /* Query the component */
        opal_output_verbose(5, opal_dstore_base_framework.framework_output,
                            "mca:dstore:select: Querying component [%s]",
                            cmp->mca_component_name);

        /* If the component reports failure, then skip component - however,
         * it is okay to return a NULL module */
        if (OPAL_SUCCESS != cmp->mca_query_component(&md, &priority)) {
            opal_output_verbose(5, opal_dstore_base_framework.framework_output,
                                "mca:dstore:select: Skipping component [%s] - not available",
                                cmp->mca_component_name );
            continue;
        }

        /* track the highest priority component that returned a NULL module - this
         * will become our storage element */
        if (NULL == md) {
            if (0 < priority && priority > cmp_pri) {
                comp = (opal_dstore_base_component_t*)cmp;
                cmp_pri = priority;
            }
        } else {
            /* track the highest priority module that was returned - this
             * will become our backfill element */
            if (priority > mod_pri) {
                mod = (opal_dstore_base_module_t*)md;
                mod_pri = priority;
            }
        }
    }

    if (NULL == comp) {
        /* no components available - that's bad */
        return OPAL_ERROR;
    }
    opal_dstore_base.storage_component = comp;

    /* it's okay not to have a backfill module */
    opal_dstore_base.backfill_module = mod;

    return OPAL_SUCCESS;;
}
