/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2018-2019 Triad National Security, LLC. All rights
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
#pragma weak MPI_T_event_handle_free = PMPI_T_event_handle_free
#endif
#define MPI_T_event_handle_free PMPI_T_event_handle_free
#endif

int MPI_T_event_handle_free (MPI_T_event_registration event_registration,
                             void *user_data,
                             MPI_T_event_free_cb_function free_cb_function)
{
    int ret = MPI_SUCCESS;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    ompi_mpit_lock ();

    /* Check that this is a valid handle */
    if (MPI_T_EVENT_REGISTRATION_NULL == event_registration) {
        ret = MPI_T_ERR_INVALID_HANDLE;
    } else {
        mca_base_event_registration_free (event_registration, 
                                          (mca_base_event_registration_free_cb_fn_t) free_cb_function);
    }

    ompi_mpit_unlock ();

    return ret;
}
