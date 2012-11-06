/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/state/state.h"
#include "orte/mca/state/base/base.h"
#include "orte/mca/state/base/state_private.h"


int orte_state_base_close(void)
{
    /* if not initialized, then skip this action. */
    if( !orte_state_base.initialized ) {
        return ORTE_SUCCESS;
    }

    /* Close selected component */
    if( NULL != orte_state.finalize ) {
        orte_state.finalize();
    }

    /* Close all remaining available components */
    mca_base_components_close(orte_state_base_output, 
                              &orte_state_base_components_available,
                              NULL);

    /* Close the framework output */
    opal_output_close (orte_state_base_output);
    orte_state_base_output = -1;

    orte_state_base.initialized = false;
    
    return ORTE_SUCCESS;
}
