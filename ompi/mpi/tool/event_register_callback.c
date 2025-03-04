/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2018-2025 Triad National Security, LLC. All rights
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
#pragma weak MPI_T_event_register_callback = PMPI_T_event_register_callback
#endif
#define MPI_T_event_register_callback PMPI_T_event_register_callback
#endif

int MPI_T_event_register_callback (MPI_T_event_registration event_registration,
                                   MPI_T_cb_safety cb_safety, MPI_Info info, void *user_data,
                                   MPI_T_event_cb_function event_cb_function)
{
    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    return MPI_T_ERR_INVALID_HANDLE;
}
