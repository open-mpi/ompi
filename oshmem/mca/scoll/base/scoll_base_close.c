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
#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/mca/scoll/base/base.h"


int mca_scoll_base_close(void)
{
    /* Close all components that are still open.  This may be the opened
     * list (if we're in ompi_info), or it may be the available list (if
     * we're anywhere else). */

    if (mca_scoll_base_components_opened_valid) {
        mca_base_components_close(mca_scoll_base_output,
                                  &mca_scoll_base_components_opened, NULL);
        OBJ_DESTRUCT(&mca_scoll_base_components_opened);
        mca_scoll_base_components_opened_valid = false;
    } else if (mca_scoll_base_components_available_valid) {
        mca_base_components_close(mca_scoll_base_output,
                                  &mca_scoll_base_components_available,
                                  NULL);
        OBJ_DESTRUCT(&mca_scoll_base_components_available);
        mca_scoll_base_components_available_valid = false;
    }

    /* This call should be done after memheap close */
    mca_scoll_disable();

    /* All done */

    return OSHMEM_SUCCESS;
}


void mca_scoll_disable(void) 
{   
    if (mca_scoll_sync_array)
    {
        void* ptr = (void*)mca_scoll_sync_array;

        MCA_MEMHEAP_CALL(private_free(ptr));
        mca_scoll_sync_array = NULL;
    }
}
