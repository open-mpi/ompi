/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
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
#pragma weak MPI_T_event_get_timestamp = PMPI_T_event_get_timestamp
#endif
#define MPI_T_event_get_timestamp PMPI_T_event_get_timestamp
#endif

int MPI_T_event_get_timestamp (MPI_T_event_instance event, MPI_Count *event_time)
{
    uint64_t mca_time;
    int ret;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    ret = mca_base_event_get_time (event, &mca_time);
    *event_time = (MPI_Count) mca_time;

    return ompit_opal_to_mpit_error (ret);
}
