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
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_T_pvar_readreset = PMPI_T_pvar_readreset
#endif
#define MPI_T_pvar_readreset PMPI_T_pvar_readreset
#endif

int MPI_T_pvar_readreset(MPI_T_pvar_session session, MPI_T_pvar_handle handle,
                         void *buf)
{
    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    /* XXX -- TODO -- Implement me */
    return MPI_T_ERR_INVALID_HANDLE;
}
