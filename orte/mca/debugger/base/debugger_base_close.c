/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
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

#include "orte/mca/debugger/base/base.h"
#include "orte/mca/debugger/debugger.h"

int orte_debugger_base_close(void)
{
#if !ORTE_DISABLE_FULL_SUPPORT
    if (NULL != orte_debugger.finalize) {
        orte_debugger.finalize();
    }

    /* Close all remaining available components */
    mca_base_components_close(orte_debugger_base.output, 
                              &orte_debugger_base_components_available, NULL);
#endif
    
    /* All done */
    return ORTE_SUCCESS;
}
