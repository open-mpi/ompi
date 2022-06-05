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


int MPIX_Query_rocm_support(void)
{
    return opal_built_with_rocm_support && opal_rocm_runtime_initialized;
}
