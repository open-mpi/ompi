/*
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"

#include "orte/mca/dfs/base/base.h"

int orte_dfs_base_select(void)
{
    int exit_status = OPAL_SUCCESS;
    orte_dfs_base_component_t *best_component = NULL;
    orte_dfs_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("dfs", orte_dfs_base.output,
                                        &orte_dfs_base.components_available,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* This will only happen if no component was selected */
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /* Save the winner */
    orte_dfs = *best_module;

    /* Initialize the winner */
    if (NULL != best_module) {
        if (OPAL_SUCCESS != orte_dfs.init()) {
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
    }

 cleanup:
    return exit_status;
}
