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
#pragma weak MPI_T_event_handle_alloc = PMPI_T_event_handle_alloc
#endif
#define MPI_T_event_handle_alloc PMPI_T_event_handle_alloc
#endif

int MPI_T_event_handle_alloc (int event_index, void *obj_handle, MPI_Info info,
                              MPI_T_event_registration *event_registration)
{
    mca_base_event_t * const event;
    int ret;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    ompi_mpit_lock ();

    /* Find the performance variable. mca_base_event_get() handles the
       bounds checking. */
    ret = mca_base_event_get_by_index (event_index, (mca_base_event_t **) &event);
    if (OMPI_SUCCESS != ret) {
        goto fn_fail;
    }

    /* Check the variable binding is something sane */
    if (event->event_bind > MPI_T_BIND_MPI_INFO || event->event_bind < MPI_T_BIND_NO_OBJECT) {
        /* This variable specified an invalid binding (not an MPI object). */
        ret = MPI_T_ERR_INVALID_INDEX;
        goto fn_fail;
    }

    ret = mca_base_event_registration_alloc (event, obj_handle, &info->super, event_registration);

fn_fail:
    ompi_mpit_unlock ();

    return ompit_opal_to_mpit_error(ret);
}
