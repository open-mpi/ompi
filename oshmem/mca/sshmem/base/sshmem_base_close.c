/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include "opal/constants.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "oshmem/mca/sshmem/sshmem.h"
#include "oshmem/mca/sshmem/base/base.h"

/* ////////////////////////////////////////////////////////////////////////// */
int
mca_sshmem_base_close(void)
{
    /* if there is a selected sshmem module, finalize it */
    if (NULL != mca_sshmem_base_module &&
        NULL != mca_sshmem_base_module->module_finalize) {
        mca_sshmem_base_module->module_finalize();
    }

    return mca_base_framework_components_close (&oshmem_sshmem_base_framework,
                                                NULL);
}

