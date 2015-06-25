/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      NVIDIA, Inc. All rights reserved.
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
#include "ompi/mpiext/cuda/c/mpiext_cuda_c.h"

int MPIx_CUDA_SUPPORT(void)
{
    return OPAL_ERR_NOT_SUPPORTED;
}
