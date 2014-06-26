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
#include "opal/mca/pmi/pmi.h"
#include "opal/mca/pmi/base/base.h"

/*
 * Globals
 */

int opal_pmi_base_select(void)
{
    int ret, exit_status = OPAL_SUCCESS;
    opal_pmi_base_component_t *best_component = NULL;
    opal_pmi_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("pmi", opal_pmi_base_framework.framework_output,
                                        &opal_pmi_base_framework.framework_components,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* It is okay if we don't find a runnable component - default
         * to the unsupported default. 
         */
        goto cleanup;
    }

    /* Save the winner */
    opal_pmi = *best_module;

    /* Initialize the winner */
    if (OPAL_SUCCESS != (ret = opal_pmi.init()) ) {
        exit_status = ret;
        goto cleanup;
    }
    
 cleanup:
    return exit_status;
}
