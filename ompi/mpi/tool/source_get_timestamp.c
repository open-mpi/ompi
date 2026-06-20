/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
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
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_T_source_get_timestamp = PMPI_T_source_get_timestamp
#endif
#define MPI_T_source_get_timestamp PMPI_T_source_get_timestamp
#endif

int MPI_T_source_get_timestamp (int source_index, MPI_Count *timestamp)
{
    int rc;
    opal_count_t ts = 0;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && NULL == timestamp) {
        return MPI_ERR_ARG;
    }

    /* No big lock: the OPAL framework is internally synchronized, and a read of
       a deferred-release source drives the fold, which can deliver events and
       invoke user callbacks -- they must not run with ompi_mpit_big_lock held
       (sec. 5.10). */
    rc = mca_base_event_source_get_timestamp (source_index, &ts);

    if (OPAL_SUCCESS != rc) {
        return (OPAL_ERR_NOT_SUPPORTED == rc) ? MPI_T_ERR_NOT_SUPPORTED : MPI_T_ERR_INVALID_INDEX;
    }
    *timestamp = (MPI_Count) ts;

    return MPI_SUCCESS;
}
