/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
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
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_T_pvar_readreset = PMPI_T_pvar_readreset
#endif
#define MPI_T_pvar_readreset PMPI_T_pvar_readreset
#endif

int MPI_T_pvar_readreset(MPI_T_pvar_session session, MPI_T_pvar_handle handle,
                         void *buf)
{
    int ret;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    // MPI_T_PVAR_ALL_HANDLES is explicitly disallowed for readreset
    // (no single buffer to fill).
    if (MPI_T_PVAR_ALL_HANDLES == handle || NULL == handle || session != handle->session) {
        return MPI_T_ERR_INVALID_HANDLE;
    }

    ompi_mpit_lock ();

    if (!mca_base_pvar_is_atomic (handle->pvar)) {
        ompi_mpit_unlock ();
        return MPI_T_ERR_PVAR_NO_ATOMIC;   // direct: not via ompit_opal_to_mpit_error
    }

    // Readreset is not allowed on read-only pvars
    if (mca_base_pvar_is_readonly (handle->pvar)) {
        ompi_mpit_unlock ();
        return MPI_T_ERR_PVAR_NO_WRITE;
    }

    ret = mca_base_pvar_handle_read_value (handle, buf);
    if (OPAL_SUCCESS == ret) {
        ret = mca_base_pvar_handle_reset (handle);
    }

    ompi_mpit_unlock ();

    return ompit_opal_to_mpit_error (ret);
}