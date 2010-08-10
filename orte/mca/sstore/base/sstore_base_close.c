/*
 * Copyright (c)      2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"

int orte_sstore_base_close(void)
{
    /* Close the selected component */
    if( NULL != orte_sstore.sstore_finalize ) {
        orte_sstore.sstore_finalize();
    }

    /* Close all available modules that are open */
    mca_base_components_close(orte_sstore_base_output,
                              &orte_sstore_base_components_available,
                              NULL);
    
    return ORTE_SUCCESS;
}
