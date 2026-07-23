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
#pragma weak MPI_T_event_get_timestamp = PMPI_T_event_get_timestamp
#endif
#define MPI_T_event_get_timestamp PMPI_T_event_get_timestamp
#endif

int MPI_T_event_get_timestamp (MPI_T_event_instance event, MPI_Count *event_time)
{
    int rc;
    opal_count_t ts = 0;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && NULL == event_time) {
        return MPI_ERR_ARG;
    }

    rc = mca_base_event_instance_get_timestamp (ompit_event_inst (event), &ts);
    if (OPAL_SUCCESS != rc) {
        return MPI_T_ERR_INVALID_HANDLE;
    }
    *event_time = (MPI_Count) ts;

    return MPI_SUCCESS;
}
