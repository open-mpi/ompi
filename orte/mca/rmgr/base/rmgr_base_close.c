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

#include "opal/util/trace.h"

#include "orte/orte_constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/rmgr/base/base.h"


int orte_rmgr_base_close(void)
{
    OPAL_TRACE(5);
    
    /* finalize the selected component */

    if (NULL != orte_rmgr.finalize) {
        orte_rmgr.finalize();
    }

    /* Close all remaining available components (may be one if this is a
       Open RTE program, or [possibly] multiple if this is ompi_info) */

    mca_base_components_close(orte_rmgr_base.rmgr_output, 
                              &orte_rmgr_base.rmgr_components, NULL);
    return ORTE_SUCCESS;
}

