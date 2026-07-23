/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2025      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
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
#pragma weak MPI_T_event_read = PMPI_T_event_read
#endif
#define MPI_T_event_read PMPI_T_event_read
#endif

int MPI_T_event_read (MPI_T_event_instance event, int element_index, void *buffer)
{
    int rc;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && NULL == buffer) {
        return MPI_ERR_ARG;
    }

    /* In-callback accessors are lock-free (they read instance-owned data). */
    rc = mca_base_event_read (ompit_event_inst (event), element_index, buffer);
    if (OPAL_SUCCESS != rc) {
        return (OPAL_ERR_VALUE_OUT_OF_BOUNDS == rc) ? MPI_T_ERR_INVALID_INDEX
                                                    : MPI_T_ERR_INVALID_HANDLE;
    }

    return MPI_SUCCESS;
}
