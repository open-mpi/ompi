/*
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orca_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orca/constants.h"
#include "orca/mca/stems/stems.h"
#include "orca/mca/stems/base/base.h"

int orca_stems_base_close(void)
{
    /* Call the component's finalize routine */
    if( NULL != orca_stems.stems_finalize ) {
        orca_stems.stems_finalize();
    }

    /* Close all available modules that are open */
    mca_base_components_close(orca_stems_base_output,
                              &orca_stems_base_components_available,
                              NULL);
    
    return OPAL_SUCCESS;
}
