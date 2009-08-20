/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 *
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
#include "orte/mca/fddp/base/base.h"


int orte_fddp_base_close(void)
{
    /* If we have a selected component and module, then finalize it */
    
    if (NULL != orte_fddp.finalize) {
        orte_fddp.finalize();
    }
    
    /* Close all remaining available components (may be one if this is a
     OpenRTE program, or [possibly] multiple if this is ompi_info) */
    
    mca_base_components_close(orte_fddp_base_output, 
                              &mca_fddp_base_components_available, NULL);
    
    /* All done */
    
    return ORTE_SUCCESS;
}
