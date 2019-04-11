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

#include "ompi/mpi/tool/mpit-internal.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_T_event_register_callback = PMPI_T_event_register_callback
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/tool/profile/defines.h"
#endif


int MPI_T_event_register_callback (MPI_T_event_registration event_registration,
                                   MPI_T_cb_safety cb_safety, MPI_Info info, void *user_data,
                                   MPI_T_event_cb_function event_cb_function)
{
    mca_base_event_t * const event;
    int ret;
    
    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    ompi_mpit_lock ();

    ret = mca_base_event_register_callback (event_registration, (mca_base_cb_safety_t) cb_safety,
                                            &info->super, user_data, (mca_base_event_cb_fn_t) event_cb_function);

    ompi_mpit_unlock ();

    return ompit_opal_to_mpit_error(ret);
}
