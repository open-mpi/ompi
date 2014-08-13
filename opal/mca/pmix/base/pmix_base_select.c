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
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/mca/pmix/base/base.h"

/*
 * Globals
 */

int opal_pmix_base_select(void)
{
    int ret, exit_status = OPAL_SUCCESS;
    opal_pmix_base_component_t *best_component = NULL;
    opal_pmix_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("pmix", opal_pmix_base_framework.framework_output,
                                        &opal_pmix_base_framework.framework_components,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* notify caller that no available component found */
        return OPAL_ERR_NOT_FOUND;
    }

    /* Save the winner */
    opal_pmix = *best_module;

    /* Initialize the winner */
    if (OPAL_SUCCESS != (ret = opal_pmix.init()) ) {
        /* connection not available is okay - just means
         * that a server hasn't already been defined */
        if (OPAL_ERR_SERVER_NOT_AVAIL == ret) {
            exit_status = OPAL_SUCCESS;
        } else {
            exit_status = ret;
        }
    }
    
    return exit_status;
}
