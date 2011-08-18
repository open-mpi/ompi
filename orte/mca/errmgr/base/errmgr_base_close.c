/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
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
#include "orte/constants.h"

#include <stdio.h>

#include "opal/util/trace.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"


int orte_errmgr_base_close(void)
{
    OPAL_TRACE(5);

    /* if not initialized, then skip this action. */
    if( !orte_errmgr_base.initialized ) {
        return ORTE_SUCCESS;
    }

    /* Close selected component */
    if( NULL != orte_errmgr.finalize ) {
        orte_errmgr.finalize();
    }

    /* Close all remaining available components (may be one if this is a
     * OMPI RTE program, or [possibly] multiple if this is ompi_info)
     */
    mca_base_components_close(orte_errmgr_base.output, 
                              &orte_errmgr_base_components_available,
                              NULL);

    orte_errmgr_base.initialized = false;
    
    /* always leave a default set of fn pointers */
    orte_errmgr = orte_errmgr_default_fns;

    return ORTE_SUCCESS;
}
