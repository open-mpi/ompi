/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2025      Triad National Security, LLC. All rights
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
#pragma weak MPI_T_source_get_timestamp = PMPI_T_source_get_timestamp
#endif
#define MPI_T_source_get_timestamp PMPI_T_source_get_timestamp
#endif

int MPI_T_source_get_timestamp (int source_id, MPI_Count *timestamp)
{
    mca_base_source_t *source;
    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && !timestamp) {
        return MPI_ERR_ARG;
    }

    ompi_mpit_lock ();
    source = mca_base_source_get (source_id);
    ompi_mpit_unlock ();
    if (OPAL_UNLIKELY(NULL == source)) {
        return MPI_T_ERR_INVALID_INDEX;
    }

    *timestamp = source->source_time ();

    return MPI_SUCCESS;
}
