/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include <stdio.h>

#include "orte/orte_constants.h"
#include "opal/util/trace.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/base/odls_private.h"


int orte_odls_base_close(void)
{
    OPAL_TRACE(5);
    
    /* if no components are available, then punt */
    if (!orte_odls_base.components_available) {
        return ORTE_SUCCESS;
    }
    
    /* If we have a selected component and module, then finalize it */

    if (orte_odls_base.selected) {
        orte_odls_base.selected_component.finalize();
    }

    /* Close all available components (only one in this case)  */

    mca_base_components_close(orte_odls_globals.output, 
                            &orte_odls_base.available_components, NULL);

    /* All done */

    return ORTE_SUCCESS;
}
