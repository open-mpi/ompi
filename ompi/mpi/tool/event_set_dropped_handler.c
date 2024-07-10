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
#pragma weak MPI_T_event_set_dropped_handler = PMPI_T_event_set_dropped_handler
#endif
#define MPI_T_event_set_dropped_handler PMPI_T_event_set_dropped_handler
#endif

int MPI_T_event_set_dropped_handler (MPI_T_event_registration handle, MPI_T_event_dropped_cb_function dropped_cb_function)

{
    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    /* Check that this is a valid handle */
    if (MPI_T_EVENT_REGISTRATION_NULL == handle) {
        return MPI_T_ERR_INVALID_HANDLE;
    }

    ompi_mpit_lock ();

    mca_base_event_registration_set_dropped_handler (handle, (mca_base_event_dropped_cb_fn_t) dropped_cb_function);

    ompi_mpit_unlock ();

    return MPI_SUCCESS;
}
