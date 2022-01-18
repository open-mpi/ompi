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
#pragma weak MPI_T_event_get_index = PMPI_T_event_get_index
#endif
#define MPI_T_event_get_index PMPI_T_event_get_index
#endif

int MPI_T_event_get_index (const char *name, int *event_index)
{
    mca_base_event_t *event = NULL;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && (NULL == event_index || NULL == name)) {
        return MPI_ERR_ARG;
    }

    ompi_mpit_lock ();
    (void) mca_base_event_get_by_fullname (name, &event);
    ompi_mpit_unlock ();

    if (NULL == event) {
        return MPI_T_ERR_INVALID_NAME;
    }

    *event_index = event->event_index;
    return MPI_SUCCESS;
}
