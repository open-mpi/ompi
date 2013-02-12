/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include <stdio.h>

#include "oshmem_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "oshmem/constants.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/atomic/base/base.h"


int mca_atomic_base_close(void)
{
    int ret = OSHMEM_SUCCESS;
    if (NULL != mca_atomic_base_selected_component.atomic_finalize) {
        ret = mca_atomic_base_selected_component.atomic_finalize();
    }
    /* Close all components that are still open.  This may be the opened
     * list (if we're in ompi_info), or it may be the available list (if
     * we're anywhere else). */

    if (mca_atomic_base_components_opened_valid) {
        mca_base_components_close(mca_atomic_base_output,
                                  &mca_atomic_base_components_opened, NULL);
        OBJ_DESTRUCT(&mca_atomic_base_components_opened);
        mca_atomic_base_components_opened_valid = false;
    } else if (mca_atomic_base_components_available_valid) {
        mca_base_components_close(mca_atomic_base_output,
                                  &mca_atomic_base_components_available,
                                  NULL);
        OBJ_DESTRUCT(&mca_atomic_base_components_available);
        mca_atomic_base_components_available_valid = false;
    }
    

    /* All done */

    return ret;
}
