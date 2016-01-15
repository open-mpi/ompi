/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
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
#include "opal/mca/pmix/pmix.h"
#include "opal/mca/pmix/base/base.h"

/*
 * Globals
 */

int opal_pmix_base_select(void)
{
    opal_pmix_base_component_t *best_component = NULL;
    opal_pmix_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("pmix", opal_pmix_base_framework.framework_output,
                                        &opal_pmix_base_framework.framework_components,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component, NULL) ) {
        /* notify caller that no available component found */
        return OPAL_ERR_NOT_FOUND;
    }

    /* Save the winner */
    opal_pmix = *best_module;

    /* do not initialize the module here as the type
     * of process determines which init (client or server)
     * should be done */

    return OPAL_SUCCESS;
}
