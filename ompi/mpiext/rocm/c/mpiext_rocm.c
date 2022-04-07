/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      NVIDIA, Inc. All rights reserved.
 * Copyright (c) 2022      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>

#include "opal/constants.h"
#include "opal/runtime/opal_params.h"
#include "ompi/mpiext/rocm/c/mpiext_rocm_c.h"

#if OPAL_ROCM_SUPPORT
#include "opal/rocm/common_rocm_prototypes.h"
#endif

int MPIX_Query_rocm_support(void)
{

    if (!opal_built_with_rocm_support) {
        return 0;
    } else {
        if ( opal_rocm_runtime_initialized ) {
            return 1;
        }
#if OPAL_ROCM_SUPPORT
        // There is a chance that the rocm runtime has simply not
        // been initialized yet, since that is done during the first convertor creation
        // Invoke a function that will trigger the rocm runtime initialized and
        // check the value again after that.

        int val1, val2;
        mca_common_rocm_check_bufs((char *)&val1, (char *)&val2);
#endif
    }

    return opal_rocm_runtime_initialized;
}
