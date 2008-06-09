/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/util/show_help.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/base/mca_base_component_repository.h"

#include "ompi/mca/pubsub/pubsub.h"
#include "ompi/mca/pubsub/base/base.h"


int ompi_pubsub_base_select(void)
{
    int ret, exit_status = OPAL_SUCCESS;
    ompi_pubsub_base_component_t *best_component = NULL;
    ompi_pubsub_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("pubsub", ompi_pubsub_base_output,
                                        &ompi_pubsub_base_components_available,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* This will only happen if no component was selected */
        exit_status = OMPI_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* Save the winner */
    ompi_pubsub = *best_module;
    ompi_pubsub_base_selected_component = *best_component;
    
    /* init the selected module */
    if (NULL != ompi_pubsub.init) {
        if (OMPI_SUCCESS != (ret = ompi_pubsub.init())) {
            exit_status = ret;
            goto cleanup;
        }
    }

 cleanup:
    return exit_status;
}
