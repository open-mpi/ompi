/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
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
#pragma weak MPI_T_pvar_get_index = PMPI_T_pvar_get_index
#endif
#define MPI_T_pvar_get_index PMPI_T_pvar_get_index
#endif

int MPI_T_pvar_get_index (const char *name, int var_class, int *pvar_index)
{
    int ret;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && (NULL == pvar_index || NULL == name)) {
        return MPI_T_ERR_INVALID;
    }

    ompi_mpit_lock ();
    ret = mca_base_pvar_find_by_name (name, var_class, pvar_index);
    ompi_mpit_unlock ();
    if (OPAL_SUCCESS != ret) {
        return MPI_T_ERR_INVALID_NAME;
    }

    return MPI_SUCCESS;
}
