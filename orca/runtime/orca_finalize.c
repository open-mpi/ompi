/*
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orca_config.h"
#include "orca/constants.h"

#include "orca/include/rte_orca.h"
#include "orca/runtime/runtime.h"
#include "orca/mca/stems/base/base.h"

#include "opal/runtime/opal.h"

int orca_finalize(void)
{
    int ret, exit_status = ORCA_SUCCESS;

    /*
     * Finalize once for every initialization
     */
    orca_init_counter--;
    if( orca_init_counter != 0 ) {
        return ORCA_SUCCESS;
    }

    /*
     * Stem framework
     */
    if( ORCA_SUCCESS != (ret = orca_stems_base_close()) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Finalize OPAL
     */
    opal_finalize();

 cleanup:
    orca_initialized = false;
    return exit_status;
}
