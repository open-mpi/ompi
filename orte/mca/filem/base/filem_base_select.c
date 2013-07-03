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

#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"


static orte_filem_base_component_t none_component = {
    /* Handle the general mca_component_t struct containing 
     *  meta information about the component itself
     */
    {
        ORTE_FILEM_BASE_VERSION_2_0_0,
        /* Component name and version */
        "none",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        
        /* Component open and close functions */
        orte_filem_base_none_open,
        orte_filem_base_none_close,
        orte_filem_base_none_query
    },
    {
        /* This component is checkpointable */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    
    /* Verbosity level */
    0,
    /* opal_output handler */
    -1,
    /* Default priority */
    1
};

static orte_filem_base_module_t none_module = {
    /** Initialization Function */
    orte_filem_base_module_init,
    /** Finalization Function */
    orte_filem_base_module_finalize,

    orte_filem_base_none_put,
    orte_filem_base_none_get,
    orte_filem_base_none_rm
};

int orte_filem_base_select(void)
{
    int exit_status = OPAL_SUCCESS;
    orte_filem_base_component_t *best_component = NULL;
    orte_filem_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("filem", orte_filem_base_framework.framework_output,
                                        &orte_filem_base_framework.framework_components,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* It is okay to not select anything - we'll just retain
         * the default none module
         */
        best_component = &none_component;
        best_module = &none_module;
    }

    /* Save the winner */
    orte_filem = *best_module;

    /* Initialize the winner */
    if (NULL != orte_filem.filem_init) {
        if (ORTE_SUCCESS != orte_filem.filem_init()) {
            exit_status = ORTE_ERROR;
        }
    }

    return exit_status;
}
