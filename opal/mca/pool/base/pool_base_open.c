/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/pool/pool.h"
#include "opal/mca/pool/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/pool/base/static-components.h"

/*
 * Globals
 */
opal_pool_base_component_2_0_0_t *opal_pool = NULL;


/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int opal_pool_base_open(mca_base_open_flag_t flags)
{
    mca_base_component_list_item_t *item, *next;
    opal_pool_base_component_2_0_0_t *tmp;
    int priority, highest_priority = 0;
    int ret;

    /* can only be zero or one */
    OPAL_LIST_FOREACH(item, &opal_pool_base_framework.framework_components, mca_base_component_list_item_t) {
        tmp = (opal_pool_base_component_2_0_0_t *) item->cli_component;
        ret = tmp->query (&priority);
        if (OPAL_SUCCESS != ret || priority < highest_priority) {
            continue;
        }

        highest_priority = priority;
        opal_pool = tmp;
    }

    OPAL_LIST_FOREACH_SAFE(item, next, &opal_pool_base_framework.framework_components, mca_base_component_list_item_t) {
        if ((void *) opal_pool != (void *) item->cli_component) {
            mca_base_component_unload (item->cli_component, opal_pool_base_framework.framework_output);
            opal_list_remove_item (&opal_pool_base_framework.framework_components, &item->super);
        }
    }

    /* open remaining component */
    ret = mca_base_framework_components_open (&opal_pool_base_framework, flags);
    if (ret != OPAL_SUCCESS) {
        return ret;
    }

    /* All done */
    return OPAL_SUCCESS;
}

/* Use default register/close functions */
MCA_BASE_FRAMEWORK_DECLARE(opal, pool, "pool of strings", NULL, opal_pool_base_open, NULL,
                           mca_pool_base_static_components, 0);
