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

#include "ompi/mpi/tool/mpit-internal.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_T_event_registration_free = PMPI_T_event_registration_free
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/tool/profile/defines.h"
#endif


int MPI_T_event_registration_free (MPI_T_event_free_cb_function free_cb_function,
                                   MPI_T_event_registration *handle)
{
    int ret = MPI_SUCCESS;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    ompi_mpit_lock ();

    do {
        /* Check that this is a valid handle */
        if (MPI_T_EVENT_REGISTRATION_NULL == *handle) {
            ret = MPI_T_ERR_INVALID_HANDLE;
            break;
        }

        mca_base_event_registration_free (*handle, (mca_base_event_registration_free_cb_fn_t) free_cb_function);
        *handle = MPI_T_EVENT_REGISTRATION_NULL;
    } while (0);

    ompi_mpit_unlock ();

    return ret;
}
