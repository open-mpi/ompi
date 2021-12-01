/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2020      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/tool/mpit-internal.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_T_category_get_num = PMPI_T_category_get_num
#endif
#define MPI_T_category_get_num PMPI_T_category_get_num
#endif

int MPI_T_category_get_num (int *num_cat)
{
    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && NULL == num_cat) {
        return MPI_T_ERR_INVALID;
    }

    ompi_mpit_lock ();
    *num_cat = mca_base_var_group_get_count ();
    ompi_mpit_unlock ();

    return MPI_SUCCESS;
}
