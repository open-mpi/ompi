/*
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved. 
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

#include "orte/mca/dfs/dfs.h"
#include "orte/mca/dfs/base/base.h"


int orte_dfs_base_close(void)
{
    /* if not initialized, then skip this action. */
    if( !orte_dfs_base.initialized ) {
        return ORTE_SUCCESS;
    }

    /* Close selected component */
    if( NULL != orte_dfs.finalize ) {
        orte_dfs.finalize();
    }

    /* Close all remaining available components (may be one if this is a
     * OMPI RTE program, or [possibly] multiple if this is ompi_info)
     */
    mca_base_components_close(orte_dfs_base.output, 
                              &orte_dfs_base.components_available,
                              NULL);

    orte_dfs_base.initialized = false;
    
    return ORTE_SUCCESS;
}
