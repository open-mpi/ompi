/*
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orca_config.h"

#ifdef HAVE_UNISTD_H
#include "unistd.h"
#endif

#include "orca/constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "orca/mca/stems/stems.h"
#include "orca/mca/stems/base/base.h"

int orca_stems_base_select(void)
{
    int exit_status = OPAL_SUCCESS;
    orca_stems_base_component_t *best_component = NULL;
    orca_stems_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("stems", orca_stems_base_output,
                                        &orca_stems_base_components_available,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* This will only happen if no component was selected */
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /* Save the winner */
    orca_stems_base_selected_component = *best_component;
    orca_stems = *best_module;

 cleanup:
    return exit_status;
}
