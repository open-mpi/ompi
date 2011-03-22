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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/util/output.h"

#include "orte/mca/debugger/base/base.h"

int orte_debugger_base_select(void)
{
#if !ORTE_DISABLE_FULL_SUPPORT
    orte_debugger_base_module_t *best_module=NULL;
    orte_debugger_base_component_t *best_component=NULL;
    int ret;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("debugger", orte_debugger_base.output,
                                        &orte_debugger_base_components_available,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* This will only happen if no component was selected */
        ret = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* Save the winner */
    /* No global component structure */
    orte_debugger = *best_module;

    ret = orte_debugger.init();

 cleanup:
    return ret;
#else
    return ORTE_ERR_NOT_IMPLEMENTED;
#endif
}
