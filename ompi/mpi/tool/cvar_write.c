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
#pragma weak MPI_T_cvar_write = PMPI_T_cvar_write
#endif
#define MPI_T_cvar_write PMPI_T_cvar_write
#endif

int MPI_T_cvar_write (MPI_T_cvar_handle handle, const void *buf)
{
    int rc = MPI_SUCCESS;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && NULL == buf) {
        return MPI_T_ERR_INVALID;
    }

    ompi_mpit_lock ();

    do {
        if (MCA_BASE_VAR_SCOPE_CONSTANT == handle->var->mbv_scope ||
            MCA_BASE_VAR_SCOPE_READONLY == handle->var->mbv_scope) {
            rc = MPI_T_ERR_CVAR_SET_NEVER;
            break;
        }

        if (!(MCA_BASE_VAR_FLAG_SETTABLE & handle->var->mbv_flags)) {
            rc = MPI_T_ERR_CVAR_SET_NOT_NOW;
            break;
        }

        rc = mca_base_var_set_value(handle->var->mbv_index, buf, sizeof(unsigned long long), MCA_BASE_VAR_SOURCE_SET, NULL);
        if (OPAL_SUCCESS != rc) {
            rc = MPI_T_ERR_CVAR_SET_NOT_NOW;
        }
    } while (0);

    ompi_mpit_unlock ();

    return rc;
}
